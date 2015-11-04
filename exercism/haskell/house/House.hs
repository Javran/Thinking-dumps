module House
  ( rhyme
  ) where

import Control.Monad.State

-- a context is some text with a hole in it
type Context = String -> String
type TransformInfo = (String, Context)

rhymeInfos :: [ TransformInfo ]
rhymeInfos =
    [ makeItem "that lay in" "the house"
    , makeItem "that ate" "the malt"
    , makeItem "that killed" "the rat"
    , makeItem "that worried" "the cat"
    , makeItem "that tossed" "the dog"
    , makeItem "that milked" "the cow with the crumpled horn"
    , makeItem "that kissed" "the maiden all forlorn"
    , makeItem "that married" "the man all tattered and torn"
    , makeItem "that woke" "the priest all shaven and shorn"
    , makeItem "that kept" "the rooster that crowed in the morn"
    , makeItem "that belonged to" "the farmer sowing his corn"
    , makeItem "<placeholder>" "the horse and the hound and the horn"
    ]
  where
    makeItem xs ys = (ys, (++ "\n" ++ xs ++ " " ++ ys))

initContext :: Context
initContext xs = unwords [ "This is", xs, "that Jack built." ]

applyTransformInfo :: TransformInfo -> State Context String
applyTransformInfo (obj, oCtxt) = do
    ctxt <- get
    modify (. oCtxt)
    return (ctxt obj)

rhyme :: String
rhyme = concatMap (++ "\n\n") parts
  where
    parts = evalState (mapM applyTransformInfo rhymeInfos) initContext
