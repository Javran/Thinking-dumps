module LanguageHead where

import Keyboard exposing (..)
import Mouse exposing (..)
import Text exposing (..)
import Time exposing (..)
import Signal.Extra exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Text
import Tools exposing (..)

import List
import Signal
import Random

type State = Play | Pause | GameOver

type alias Input =
  { space : Bool
  , x : Int
  , delta : () -- no need of this piece of info
  , rand : Int
  }

type alias Head =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , img : String
  }

type alias Player =
  { x : Float
  , score : Int
  }

type alias Game =
  { state : State
  , heads : List Head
  , player : Player
  }

defaultHead : Int -> Head
defaultHead n = {x=100.0, y=75, vx=60, vy=0.0, img=headImage n }

defaultGame : Game
defaultGame =
  { state = Pause
  , heads = []
  , player = {x=0.0, score=0} 
  }

headImage : Int -> String
headImage n = case n of
  0 -> "/img/brucetate.png"
  1 -> "/img/davethomas.png"
  2 -> "/img/evanczaplicki.png"
  3 -> "/img/joearmstrong.png"
  4 -> "/img/josevalim.png"
  _ -> ""

bottom : Float
bottom = 550

secsPerFrame : Float
secsPerFrame = 1.0 / 50.0

delta : Signal ()
delta = always () <~ fps 50

randomNums : Signal a -> Signal Int
randomNums sg =
  let g = Random.int 0 4
      initS = Random.initialSeed 1234
  in Signal.foldp
       (\_ (vOld,s) -> Random.generate g s)
       (0, initS) sg
     |> Signal.map fst

input : Signal Input
input = Signal.sampleOn delta (Input <~ Keyboard.space
                               ~ Mouse.x
                               ~ delta
                               ~ randomNums (every secsPerFrame))

main : Signal Element
main = Signal.map display gameState

gameState : Signal Game
gameState = Signal.foldp stepGame defaultGame input

stepGame : Input -> Game -> Game
stepGame input game =
  let 
    stepGamePlay : Input -> Game -> Game
    stepGamePlay {space, x, delta, rand} ({state, heads, player} as game) =  -- (4)
      { game 
      | state =  stepGameOver x heads
      , heads = stepHeads heads delta x player.score rand
      , player = stepPlayer player x heads 
      }

    stepGameOver : Int -> List Head -> State
    stepGameOver x heads =
      if allHeadsSafe (toFloat x) heads then Play else GameOver

  in case game.state of
    Play -> stepGamePlay input game
    Pause -> stepGamePaused input game
    GameOver -> stepGameFinished input game

allHeadsSafe : Float -> List Head -> Bool
allHeadsSafe x heads =
    List.all (headSafe x) heads

headSafe : Float -> Head -> Bool
headSafe x head =
    head.y < bottom || abs (head.x - x) < 50

-- TODO: we don't actually care about what's in "delta"?
stepHeads : List Head -> a -> b -> Int -> Int -> List Head
stepHeads heads delta x score rand =
  spawnHead score heads rand
  |> bounceHeads
  |> removeComplete
  |> moveHeads delta

spawnHead : Int -> List Head -> Int -> List Head
spawnHead score heads rand =
  let addHead = List.length heads < (score // 5000 + 1)
    && List.all (\head -> head.x > 107.0) heads in
  if addHead then defaultHead rand :: heads else heads

bounceHeads : List Head -> List Head
bounceHeads heads = List.map bounce heads

bounce : Head -> Head
bounce head =
  { head | vy = if head.y > bottom && head.vy > 0
                 then -head.vy * 0.95
                 else head.vy }

removeComplete : List Head -> List Head
removeComplete heads = List.filter (\x -> not (complete x)) heads  -- (8)

complete : Head -> Bool
complete {x} = x > 750

moveHeads : a -> List Head -> List Head
moveHeads delta heads = List.map moveHead heads     -- (9)

moveHead : Head -> Head
moveHead ({x, y, vx, vy} as head) =
  { head | x = x + vx * secsPerFrame
         , y = y + vy * secsPerFrame
         , vy = vy + secsPerFrame * 400 }

stepPlayer : Player -> Int -> List Head -> Player
stepPlayer player mouseX heads =     -- (10)
  { player | score = stepScore player heads
           , x = toFloat mouseX }

stepScore : Player -> List Head -> Int
stepScore player heads =   -- (11)
  player.score +
  1 +
  1000 * (List.length (List.filter complete heads))

stepGamePaused : Input -> Game -> Game
stepGamePaused {space, x, delta} ({state, heads, player} as game) =    -- (12)
  { game | state = stepState space state
         , player = { player |  x = toFloat x } }

stepGameFinished : Input -> Game -> Game
stepGameFinished {space, x, delta} ({state, heads, player} as game) =   -- (13)
  if space then defaultGame
  else { game | state = GameOver
              , player = { player |  x = toFloat x } }

stepState : Bool -> State -> State
stepState space state = if space then Play else state   -- (14)

display : Game -> Element
display ({state, heads, player} as game) =
  let (w, h) = (800, 600)
      drawRoad =
        filled gray (rect (toFloat w) 100)
          |> moveY (-(half h) + 50)
      drawBuilding =
        filled red (rect 100 (toFloat h))
          |> moveX (-(half w) + 50)

      drawHeads : List Head -> List Form
      drawHeads heads =
        let
          drawHead : Head -> Form
          drawHead head =
            let x = half w - head.x
                y = half h - head.y
                src = head.img
            in toForm (image 75 75 src)
              |> move (-x, y)
              |> rotate (degrees (x * 2 - 100))
        in List.map drawHead heads

      drawPaddle : Float -> Form
      drawPaddle x =
        filled black (rect 80 10)
          |> moveX (x +  10 -  half w)
          |> moveY (-(half h - 30))

      txt : (Text -> Text) -> String -> Element
      txt f = leftAligned << f << monospace << Text.color blue << Text.fromString

      drawScore : Player -> Form
      drawScore player =
        let 
          fullScore : Player -> Element
          fullScore player = txt (Text.height 50) (toString player.score)
        in toForm (fullScore player)
          |> move (half w - 150, half h - 40)

      drawMessage : State -> Form
      drawMessage state =
        let 
          stateMessage : State -> String
          stateMessage state =
            if state == GameOver then "Game Over" else "Language Head"
        in toForm (txt (Text.height 50) (stateMessage state))
          |> move (50, 50)

  in collage w h
       ([ drawRoad
        , drawBuilding
        , drawPaddle player.x
        , drawScore player
        , drawMessage state] ++
          (drawHeads heads))

half : Int -> Float
half x = toFloat x / 2
