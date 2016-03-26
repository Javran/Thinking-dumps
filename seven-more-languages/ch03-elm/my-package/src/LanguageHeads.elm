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

import List
import Signal
import Random

type State = Play | Pause | GameOver

type alias Input = 
  { space : Bool
  , x : Int
  , delta : Time
  , rand : Int 
  }

type alias Head = 
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
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

defaultHead n = {x=100.0, y=75, vx=60, vy=0.0, img=headImage n }  -- (2)
defaultGame = { state   = Pause,
                heads   = [], 
                player  = {x=0.0, score=0} }

headImage n = case n of
  0 -> "/img/brucetate.png"
  1 -> "/img/davethomas.png"
  2 -> "/img/evanczaplicki.png"
  3 -> "/img/joearmstrong.png"
  4 -> "/img/josevalim.png"
  _ -> ""

bottom = 550

-- START:part3
secsPerFrame = 1.0 / 50.0  
delta = inSeconds <~ fps 50

randomNums sg =
  let g = Random.int 0 4
      initS = Random.initialSeed 1234
  in Signal.foldp 
       (\_ (vOld,s) -> Random.generate g s)
       (Debug.crash "undefined", initS) sg
     |> Signal.map fst

input = Signal.sampleOn delta (Input <~ Keyboard.space
                               ~ Mouse.x
                               ~ delta
                               ~ randomNums (every secsPerFrame))

main = Signal.map display gameState

gameState = Signal.foldp stepGame defaultGame input
-- END:part3

-- START:part4a
stepGame input game =             -- (3)
  case game.state of
    Play -> stepGamePlay input game
    Pause -> stepGamePaused input game
    GameOver -> stepGameFinished input game
    
stepGamePlay {space, x, delta, rand} ({state, heads, player} as game) =  -- (4)
  { game | state =  stepGameOver x heads
         , heads = stepHeads heads delta x player.score rand
         , player = stepPlayer player x heads }

stepGameOver x heads = 
  if allHeadsSafe (toFloat x) heads then Play else GameOver
  
allHeadsSafe x heads =
    List.all (headSafe x) heads

headSafe x head =
    head.y < bottom || abs (head.x - x) < 50
-- END:part4a

-- START:part4b
stepHeads heads delta x score rand =    -- (5)
  spawnHead score heads rand 
  |> bounceHeads
  |> removeComplete
  |> moveHeads delta
  
spawnHead score heads rand =   -- (6)
  let addHead = List.length heads < (score // 5000 + 1) 
    && List.all (\head -> head.x > 107.0) heads in 
  if addHead then defaultHead rand :: heads else heads

bounceHeads heads = List.map bounce heads      -- (7)

bounce head = 
  { head | vy = if head.y > bottom && head.vy > 0 
                 then -head.vy * 0.95 
                 else head.vy }

removeComplete heads = filter (\x -> not (complete x)) heads  -- (8)

complete {x} = x > 750

moveHeads delta heads = List.map moveHead heads     -- (9)

moveHead ({x, y, vx, vy} as head) = 
  { head | x = x + vx * secsPerFrame
         , y = y + vy * secsPerFrame
         , vy = vy + secsPerFrame * 400 }

-- END:part4b

-- START:part4c
stepPlayer player mouseX heads =     -- (10)
  { player | score = stepScore player heads
           , x = toFloat mouseX }
           
stepScore player heads =   -- (11)
  player.score + 
  1 + 
  1000 * (List.length (List.filter complete heads))
-- END:part4c

-- START:part4d
stepGamePaused {space, x, delta} ({state, heads, player} as game) =    -- (12)
  { game | state = stepState space state
         , player = { player |  x = toFloat x } }    

stepGameFinished {space, x, delta} ({state, heads, player} as game) =   -- (13)
  if space then defaultGame    
  else { game | state = GameOver
              , player = { player |  x = toFloat x } }

stepState space state = if space then Play else state   -- (14)
-- END:part4d

-- START:part5
display ({state, heads, player} as game) =   -- (15)
  let (w, h) = (800, 600)
  in collage w h
       ([ drawRoad w h
       , drawBuilding w h
       , drawPaddle w h player.x
       , drawScore w h player
       , drawMessage w h state] ++ 
       (drawHeads w h heads))

drawRoad w h =   -- (16)
  filled gray (rect (toFloat w) 100) 
  |> moveY (-(half h) + 50)
  
drawBuilding w h =
  filled red (rect 100 (toFloat h)) 
  |> moveX (-(half w) + 50)

drawHeads w h heads = Signal.map (drawHead w h) heads   -- (17)

drawHead w h head = 
  let x = half w - head.x
      y = half h - head.y
      src = head.img
  in toForm (image 75 75 src) 
     |> move (-x, y)
     |> rotate (degrees (x * 2 - 100))

drawPaddle w h x =   -- (18)
  filled black (rect 80 10) 
  |> moveX (x +  10 -  half w) 
  |> moveY (-(half h - 30))

half x = toFloat x / 2

drawScore w h player =     -- (19)
  toForm (fullScore player) 
  |> move (half w - 150, half h - 40)

fullScore player = txt (Text.height 50) (toString player.score)

txt f = leftAligned << f << monospace << Text.color blue << Text.fromString

drawMessage w h state =    -- (20)
  toForm (txt (Text.height 50) (stateMessage state)) 
  |> move (50, 50)
  
stateMessage state = 
  if state == GameOver then "Game Over" else "Language Head"
