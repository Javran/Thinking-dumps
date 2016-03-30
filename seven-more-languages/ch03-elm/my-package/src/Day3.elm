module Day3 where

-- Day3 is just a copy of LanguageHead
-- where we modify it to accomplish all tasks

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
import Maybe

import List
import Signal
import Random

type State = Play | Pause | GameOver

type alias Input =
  { space : Bool -- ^ whether "space" has been pressed
  , x : Int -- ^ x coord of the mouse
  }

type alias Head =
  { x : Float -- x,y coordinate
  , y : Float
  , vx : Float -- speed of x & y directions
  , vy : Float
  , img : String -- image source
  }

type alias Player =
  { x : Float
  , score : Int
  }

-- exercise: add some random elements to when heads get added so that all of
-- the games are no longer the same.

-- not sure what this exercise means, but we have already make every game looks different
-- by taking timestamp to initialized random generator, and the result looks pretty good.
-- so I consider my solution a better one and call this exercise done.

type alias Game =
  { state : State
  , heads : List Head
  , player : Player
    -- "nextRndInt" and "mSeed" are added
    -- for generating (pseudo) random numbers
    -- "nextRndInt" should not be modified after initialization
    -- but "mSeed" must be updated once a new random number is
    -- generated and used.
  , nextRndInt : Random.Seed -> (Int, Random.Seed)
  , mSeed : Maybe Random.Seed
  }

-- get element at a specific index of a list
-- a default value will be returned if the index
-- is invalid
listGetWithDefault : a -> Int -> List a -> a
listGetWithDefault d n xs = case n of
    0 -> case xs of
           [] -> d
           (h :: _) -> h
    _ -> if n < 0
           then d
           else case xs of
                  [] -> d
                  (_::t) -> listGet (n-1) t

-- exercise: make the game choose a random head from
-- a list of graphics.
-- to achieve this we just need to make an explicit list
-- of available graphics and let user pick one index

headSources : List String
headSources =
  [ "/img/brucetate.png"
  , "/img/davethomas.png"
  , "/img/evanczaplicki.png"
  , "/img/joearmstrong.png"
  , "/img/josevalim.png"
  ]

-- generate heads of different people
defaultHead : Int -> Head
defaultHead n =
  let
    headImage : Int -> String
    headImage n = listGetWithDefault "" n headSources

    -- to make the head bounces more times,
    -- we need to make it take more time for a head to pass the screen
    -- one simple solution is just make velocity of x-axis a bit slower:
    -- defaultVx = 60
    defaultVx = 55
  in {x=100.0, y=75, vx=defaultVx, vy=0.0, img=headImage n}

defaultGame : Game
defaultGame =
  -- since we have an explicit list of available heads
  -- we should adjust range of the random generator accordingly
  let g = Random.int 0 (List.length headSources - 1)
  in 
    { state = Pause
    , heads = []
    , player = {x=0.0, score=0} 
    , nextRndInt = Random.generate g
    , mSeed = Nothing
    }

bottom : Float
bottom = 550

secsPerFrame : Float
secsPerFrame = 1.0 / 50.0

-- sampling input (keyboard & mouse) 50 times per second
-- and this also makes the game tick
input : Signal Input
input = Signal.sampleOn
          (fps 50)
          (Input <~ Keyboard.space
                  ~ Mouse.x)

-- consume input and update game state
-- a timestamp is added to each input signal
-- so we can generate a seed on demand
gameState : Signal Game
gameState =
  let
    stepGame : (Time, Input) -> Game -> Game
    stepGame (ts,input) game =
      let 
        -- shared bindings & util functions
        {space, x} = input
        xFloat = toFloat x
        {state, heads, player} = game
    
        complete : Head -> Bool
        complete {x} = x > 750

        count : (a -> Bool) -> List a -> Int
        count pred = List.filter pred >> List.length
    
        stepGamePlay : Game
        stepGamePlay =
          let
            -- if any head is not save, the game is over
            stepGameOver : State
            stepGameOver =
              let 
                -- a head is safe when one of the following is met:
                headSafe : Head -> Bool
                headSafe head =
                  -- * the head is above bottom
                     head.y < bottom
                  -- * or the head is within reach of the paddle
                  || abs (head.x - xFloat) < 50
              in if List.all headSafe heads 
                 then Play
                 else GameOver
    
            -- calculate player's current score
            stepPlayer : Player
            stepPlayer =
              let
                stepScore = player.score + 1 + 1000 * (count complete heads)
              in { player
                 | score = stepScore
                 , x = xFloat }
    
            -- calculate next state of each head,
            -- might create new ones if necessary
            stepHeads : (List Head, Maybe Random.Seed)
            stepHeads =
              let
                -- make "seed" available to other member definitions
                -- create one from timestamp if necessary
                seed = Maybe.withDefault
                         (ts
                           |> inMilliseconds 
                           |> round 
                           |> Random.initialSeed)
                         game.mSeed
                -- generate next random number "rand",
                -- note that if this random number is not used
                -- then no update should happen to the mSeed field
                (rand,nextSeed) = game.nextRndInt seed

                -- a flag to indicate if we need to create a new head on the screen
                -- exercise: don't allow another head to be added too closely to
                -- an existing head.
                -- to do so, we add extra constraint to the condition of generating new head:
                -- * get latest generated head (if there is one)
                -- * see if it is "far enough" from the initial point
                --   (here we set the threshold to 10)
                newHeadCreated = List.length heads < (player.score // 5000 + 1)
                                 && List.all (\head -> head.x > 107.0) heads
                                 && (Maybe.withDefault True
                                       (List.head heads
                                         |> Maybe.map (\h -> h.x >= 10)))

                spawnHead : List Head
                spawnHead =
                  if newHeadCreated
                    then defaultHead rand :: heads
                    else heads
                
                bounce : Head -> Head
                bounce head =
                  { head | vy = if head.y > bottom && head.vy > 0
                                then -head.vy * 0.95 -- bounces the head
                                else head.vy }
    
                moveHead : Head -> Head
                moveHead ({x, y, vx, vy} as head) =
                  { head
                  | x = x + vx * secsPerFrame
                  , y = y + vy * secsPerFrame
                  , vy = vy + secsPerFrame * 400 -- simulate gravity
                  }
    
              in (spawnHead
                   |> List.map bounce -- bounceHeads
                   |> List.filter (not << complete) -- removeComplete
                   |> List.map moveHead --  moveHeads
                 , if newHeadCreated then Just nextSeed else game.mSeed)
    
            (nextHeads,nextMSeed) = stepHeads
        in
            { game 
            | state = stepGameOver
            , heads = nextHeads
            , player = stepPlayer
            , mSeed = nextMSeed
            }
        -- END of stepGamePlay

        -- show game over message,
        -- and restart the game when space is pressed
        -- paddle's x coordinate is updated accordingly
        stepGameFinished :  Game
        stepGameFinished =
          if space 
            then defaultGame
            else { game 
                 | state = GameOver
                 , player = { player |  x = toFloat x }
                 }
    
        -- game is initialized at "Pause" state,
        -- when "space" is pressed, the game state will transit to "Play" state
        -- paddle's x coordinate is updated accordingly
        stepGamePaused : Game
        stepGamePaused =
          { game 
          | state = if space then Play else state
          , player = { player | x = toFloat x } }
    
      in case game.state of
        Play -> stepGamePlay
        Pause -> stepGamePaused
        GameOver -> stepGameFinished
    
  in Signal.foldp stepGame defaultGame (Time.timestamp input)

-- render game state on the screen
main : Signal Element
main = Signal.map display gameState
    
-- render game state on screen
display : Game -> Element
display ({state, heads, player} as game) =
  let (w, h) = (800, 600)

      half : Int -> Float
      half x = toFloat x / 2

      -- type signatures for the following definitions
      -- are all default to "Form" if not stated explicitly


      -- exercise: make the road look more like a road and the building
      -- look more like a building

      -- what we can do is to have some texture and do:
      {-
      drawRoad =
        textured "/img/some-road-texture" (rect (toFloat w) 100)
          |> moveY (-(half h) + 50)
      -}
      -- however, "textured" in Collage has poor performance
      -- rendering the game almost unplayable.
      -- so we just leave this part commented and skip this exercise.

      drawRoad =
        filled gray (rect (toFloat w) 100)
          |> moveY (-(half h) + 50)

      drawBuilding =
        filled red (rect 100 (toFloat h))
          |> moveX (-(half w) + 50)

      drawHeads : List Form
      drawHeads =
        let
          drawHead : Head -> Form
          drawHead head =
            let x = half w - head.x
                y = half h - head.y
                src = head.img
                -- exercise: show a different kind of head when one reaches the bottom.
                -- to do so, we just apply a differnt style on heads
                -- that reach the bottom (the "+ 10" is just for relaxing the condition).
                -- there is not much we can do within Collage,
                -- to keep it simple we just apply a different opacity on the picture:
                -- when the head reaches the bottom we use 50% opacity instead of 100%
                hOpacity = if head.y + 10 >= bottom then 0.5 else 1
            in toForm (opacity hOpacity (image 75 75 src))
              |> move (-x, y)
              |> rotate (degrees (x * 2 - 100))
        in List.map drawHead heads

      drawPaddle =
        filled black (rect 80 10)
          |> moveX (player.x +  10 -  half w)
          |> moveY (-(half h - 30))

      txt : (Text -> Text) -> String -> Element
      txt f = leftAligned << f << monospace << Text.color blue << Text.fromString

      drawScore =
        let 
          fullScore : Player -> Element
          fullScore player = txt (Text.height 50) (toString player.score)
        in toForm (fullScore player)
          |> move (half w - 150, half h - 40)

      drawMessage =
        let 
          stateMessage = case state of
            GameOver -> "Game Over"
            -- asks the user to press the spacebar to start.
            Pause -> "Press Spacebar to Start"
            _ -> "Language Head"
        in toForm (txt (Text.height 50) stateMessage)
          |> move (50, 50)

  in collage w h
       ([ drawRoad
        , drawBuilding
        , drawPaddle
        , drawScore
        , drawMessage ] ++
          drawHeads)
