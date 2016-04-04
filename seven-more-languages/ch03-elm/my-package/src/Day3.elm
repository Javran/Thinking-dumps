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
import Char

import List
import Signal
import Random

type State = Play | Pause | GameOver

-- exercise: add other features that show up at different score increments.
-- for example, bounce the heads up in the air, wherever they are, when a user
-- presses a key. This will let the user survive when two heads reach the bottom
-- at the same time.
--
-- I'm not sure what exactly we are supposed to do.
-- From what I can understand, let's just do the following:
-- * When there are more than 2 heads visible in game,
--   we enable a feature that allows user to press a specific key (we are reusing space)
--   so that all falling heads will bounce up.
-- * However, I don't think this solution is good, if 2 heads reache the bottom
--   at the same time, they will still be reaching the bottom at the same time,
--   no matter how many time they bounce.
--   so let's just add some randomness: for each head, a random multiplier in range [0.5,1.0] will
--   be applied to the speed when bouncing. so it's more likely that two heads won't
--   reach the bottom at the same time.

-- skipping the following exercises because I feel implementing them
-- is more of game designing rather than praticing programming skills.
-- * (skipped) exercise: provide a better formula for when to add additional heads
-- * (skipped) exercise: add heads at predetermined spacings.

-- exercise: add another paddle users could move with the A and D keys,
-- or arrow keys.
-- to implement this, "lr" is introduced in Input to receive keyboard controls
-- and then in Player we keep an extra field to keep track of its position then
-- render it properly on screen.
-- finally we modify the condition of testing whether a head is safe.

type alias Input =
  { space : Bool -- ^ whether "space" has been pressed
  , x : Int -- ^ x coord of the mouse
  , lr : { x : Int, y : Int } -- ^ arrows or wasd
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
  , xKey : Float -- the paddle controlled by key
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
    -- "mSeed" is added
    -- for generating (pseudo) random values,
    -- "mSeed" must be updated whenever a new random value is
    -- generated and used.
  , mSeed : Maybe Random.Seed
    -- exercise: give the user three lives. add additional lives when
    -- the user hits a certain score.

    -- we add 2 components to Game for implementing this:
    -- "life" indicates how many lifes the player currently have
    -- when it reaches 0, the game is over
    -- "lastAwardedTier" is to prevent same award from being given multiple times
    -- when player reaches certain score, this "variable" will be increased to
    -- indicate that the award of passing this score has been given.
  , life : Int
    -- initiallly 0,
    -- should increases as game continues
    -- to indicate whether life has been properly awarded.
    -- e.g. lastAwardedTier = 3 means the award of reaching 3*5000 has been given
  , lastAwardedTier : Int
  }

-- extract the seed from Maybe, if we haven't initialized
-- it, then taka timestamp to generate one.
extractSeed : Time -> Maybe Random.Seed -> Random.Seed
extractSeed ts =
  Maybe.withDefault
         (ts
         |> inMilliseconds 
         |> round 
         |> Random.initialSeed)

-- test whether bounce function is available
canBounce : Game -> Bool
canBounce g = List.length g.heads > 2

-- apply random bounce to heads, ignoring condition checks
applyBounce : Time -> Game -> Game
applyBounce ts ({state, heads, player,life} as game) = 
  let 
    seed = extractSeed ts game.mSeed
    (multipliers, newSeed) = 
      Random.generate 
              (Random.list 
                 (List.length heads) 
                 (Random.float 0.5 1))
              seed
    applyMult head multiplier = 
      let newVy = 
            if head.vy > 0
              then -head.vy * multiplier
              else head.vy
      in { head | vy = newVy }
    newHeads = List.map2 applyMult heads multipliers
  in { game 
     | heads = newHeads 
     , mSeed = Just newSeed
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

genHeadSource : Random.Generator String
genHeadSource =
  -- listGet is safe, because we only generate
  -- valid indices
  Random.int 0 (List.length headSources - 1)
    |> Random.map (\x -> listGet x headSources)

genHeadSpeedV : Random.Generator Float
genHeadSpeedV =
  Random.float (-200) 200

genHead : Random.Generator Head
genHead = Random.map2 defaultHead genHeadSource genHeadSpeedV

-- exercise: prevent heads from reaching the bottom at the same time.
-- 2 possible solutions:
-- * we don't intentionally prevent this from happening,
--   but give a initial speed in Vy direction, so it's more likely
--   that they reaches the bottom at different time
-- * use math formula to deduce when the head will reach the bottom
--   when generating new heads, we check if this new head reaches the bottom
--   at roughly the same time (use a time difference threshold) as other heads.
--   new head can only be generated if enough time difference can be confirmed
-- note that even with these 2 steps, it is still possible that 2 heads will reach the bottom
-- at roughly the same time, this is because the formula we use can only say things about *first
-- time* the head reaches the bottom. if a head bounces, it maybe have a new reaching-bottom time
-- that coincide with other heads' reaching-bottom time.
-- we won't be addressing this issue, as I think the current solution is good enough.
-- also, more computation will be required if we want to completely prevent this from happening
-- but we want the check to be as quick as possible because we will be calling this check
-- on each frame.

-- generate heads of different people
defaultHead : String -> Float -> Head
defaultHead src vyInit =
  let
    -- exercise: make the heads bounce more times as they
    -- cross the screen.
    --
    -- to make the head bounces more times,
    -- we need to make it take more time for a head to pass the screen
    -- one simple solution is just make velocity of x-axis a bit slower:
    -- defaultVx = 60
    defaultVx = 55
  in { x=100.0, y=75, vx=defaultVx
     , vy=vyInit
     , img=src}

defaultGame : Game
defaultGame =
  -- since we have an explicit list of available heads
  -- we should adjust range of the random generator accordingly
  let g = Random.int 0 (List.length headSources - 1)
  in 
    { state = Pause
    , heads = []
      -- xKey: we want to put the keyboard-controlled paddle in the middle
      -- of the play-zone
    , player = {x=0.0, score=0, xKey=100+(800-100)/2}
    , mSeed = Nothing
      -- have 3 lives initially
    , life = 3
    , lastAwardedTier = 0
    }

bottom : Float
bottom = 550

secsPerFrame : Float
secsPerFrame = 1.0 / 50.0

-- sampling input (keyboard & mouse) 50 times per second
-- and this also makes the game tick
-- NOTE: not sure why, but Keyboard.isDown is not working for me.
-- I was planning to use some other keys as the "bounce" key
-- but because of this problem, I decide to workaround by using space key
input : Signal Input
input = Signal.sampleOn
          (fps 50)
          (Input <~ Keyboard.space
                  ~ Mouse.x
                  ~ Signal.merge wasd arrows)

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
        {space, x, lr} = input
        xFloat = toFloat x

        {state, heads, player,life,lastAwardedTier} = game

        nextPaddle2Pos =
          let dx = 
                if lr.x == -1
                  then -10
                  else 
                    if lr.x == 1 then 10 else 0
          in clamp 0 800 (player.xKey + dx)
    
        complete : Head -> Bool
        complete {x} = x > 750

        count : (a -> Bool) -> List a -> Int
        count pred = List.filter pred >> List.length
    
        stepGamePlay : Game
        stepGamePlay =
          let
            -- decreases life if player has failed to safe heads
            lifeDecrModifier : Int -> Int
            lifeDecrModifier =
              let 
                -- a head is safe when one of the following is met:
                headSafe : Head -> Bool
                headSafe head =
                  -- * the head is above bottom
                     head.y < bottom
                  -- * or the head is within reach of the paddle
                  || abs (head.x - xFloat) < 50
                  -- * or the head is within reach of the second paddle
                  || abs (head.x - nextPaddle2Pos) < 50
              in if List.all headSafe heads 
                 then identity
                 else (\x -> x-1)
    
            -- calculate player's current score
            nextPlayerAndTier : (Player,Int)
            nextPlayerAndTier =
              let
                nextScore = player.score + 1 + 1000 * (count complete heads)
                currentTier = nextScore // 5000
                p = { player
                    | score = nextScore
                    , x = xFloat
                    , xKey = nextPaddle2Pos }
              in (p, currentTier)
    
            -- calculate next state of each head,
            -- might create new ones if necessary
            stepHeads : (List Head, Maybe Random.Seed)
            stepHeads =
              let
                -- make "seed" available to other member definitions
                -- create one from timestamp if necessary
                seed = extractSeed ts game.mSeed

                -- generate next random number "rand",
                -- note that if this random number is not used
                -- then no update should happen to the mSeed field
                (newHead,nextSeed) = Random.generate genHead seed

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
                                 && noBottomConflict

                noBottomConflict =
                  let newHeadBottomTime = solveBottomTime newHead.y newHead.vy
                  in case newHeadBottomTime of
                       -- new head's bottom time is valid
                       Just nhbt ->
                         let noConflict head =
                               let curHeadBottomTime = solveBottomTime head.y head.vy
                               in case curHeadBottomTime of 
                                    Just chbt ->
                                      -- newly added head might have a "distinct" bottom time
                                      -- in a sense that there should be at least an interval
                                      -- of "30" time units for players to move paddle
                                      -- to save heads
                                      abs (nhbt - chbt) > 30
                                    Nothing -> False
                         in List.all noConflict (List.filter (not << complete) heads)
                       Nothing -> False

                spawnHead : List Head
                spawnHead =
                  if newHeadCreated
                    then newHead :: heads
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
            (nextPlayer,currentTier) = nextPlayerAndTier

            lifeGiveBonus : Int -> Int 
            lifeGiveBonus =
              if lastAwardedTier /= currentTier
                 then (\x -> x + (currentTier - lastAwardedTier))
                 else identity
                                       
            nextLife = lifeDecrModifier life |> lifeGiveBonus
            nextState = if nextLife <= 0 then GameOver else Play
            nextLastAwardedTier =
              if lastAwardedTier /= currentTier
                 then currentTier
                 else lastAwardedTier

            bounceOptionally = 
              if space && canBounce game
                 then applyBounce ts
                 else identity
        in
            { game 
            | state = nextState
            , heads = nextHeads
            , life = nextLife
            , lastAwardedTier = nextLastAwardedTier
            , player = nextPlayer
            , mSeed = nextMSeed
            } |> bounceOptionally
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
                 , player = { player |  x = xFloat, xKey = nextPaddle2Pos }
                 }
    
        -- game is initialized at "Pause" state,
        -- when "space" is pressed, the game state will transit to "Play" state
        -- paddle's x coordinate is updated accordingly
        stepGamePaused : Game
        stepGamePaused =
          { game 
          | state = if space then Play else state
          , player = { player | x = xFloat, xKey = nextPaddle2Pos } }
    
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
display ({state, heads, player,life} as game) =
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

      drawPaddle2 =
        filled blue (rect 80 10)
          |> moveX (player.xKey +  10 -  half w)
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
            _ -> "Live(s):" ++ toString life 
                 ++ if canBounce game
                      then "(B)" -- "can bounce" incidator
                      else ""
        in toForm (txt (Text.height 50) stateMessage)
          |> move (50, 50)

  in collage w h
       ([ drawRoad
        , drawBuilding
        , drawPaddle
        , drawPaddle2
        , drawScore
        , drawMessage ] ++
          drawHeads)

{-
    yInit: initial y location
    vyInit: initial y speed
    accInit: acceleration
    
    y = yInit + integrate( ySpeed, t: [0..t])
      = yInit + integrate( vyInit + accInit * t, t: [0..t])
      = yInit + vyInit*t + accInit * integrate( t, t:[0..t] )
      = yInit + vyInit*t + 1/2 * accInit*t*t
    
    let accInit = secsPerFrame * 400
    we have:

    h(t) = yInit + vyInit*t + secsPerFrame*200*t*t

    now that we consider to solve the equation:

                                       h(t) = bottom
    yInit + vyInit*t + secsPerFrame*200*t*t - bottom = 0
    
    a = secsPerFrame*200
    b = vyInit
    c = yInit-bottom

    if this equation has solution, we take the minimal positive one,
    which should roughly be the time when this head reaches the bottom.

    we don't have to worry about time units,
    they are not important as all heads are calculated in the same way.
-}
solveBottomTime : Float -> Float -> Maybe Float
solveBottomTime yInit vyInit = 
  let a = secsPerFrame*200
      b = vyInit
      c = yInit-bottom
      delta = b*b - 4*a*c
  in if delta < 0
       then Nothing
       else
         let 
           sDelta = sqrt delta
           x1 = (-b + sDelta) / (2*a)
           x2 = (-b - sDelta) / (2*a)
         in [x1,x2]
            |> List.filter (\x -> x >= 0)
            |> List.minimum
