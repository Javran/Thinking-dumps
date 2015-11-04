module House
  ( rhyme
  ) where

import Control.Monad.State

-- a Context is some text with a hole in it
type Context = String -> String

-- a TransformInfo is information for transforming a context
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
      -- the first item of this element
      -- is never used in our generating algorithm anyway
    , makeItem "<placeholder>" "the horse and the hound and the horn"
    ]
  where
    -- a TransformInfo includes two pieces of information
    -- + the item to be filled in the hole to generate current item
    -- + the transformation to be performed on current context
    --   to generate the next context
    makeItem xs ys = (ys, (++ "\n" ++ xs ++ " " ++ ys))

initContext :: Context
initContext xs = unwords [ "This is", xs, "that Jack built." ]

applyTransformInfo :: TransformInfo -> State Context String
applyTransformInfo (obj, oCtxt) = gets ($ obj) <* modify (. oCtxt)

rhyme :: String
rhyme = concatMap (++ "\n\n") parts
  where
    parts = evalState (mapM applyTransformInfo rhymeInfos) initContext
