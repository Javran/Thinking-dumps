module House
  ( rhyme
  ) where

-- a context is some text with a hole in it
type Context = String -> String

rhymeInfos :: [ (String, Context) ]
rhymeInfos =
    [ makeItem "that lay in" "the house"
    , makeItem "that ate" "the malt"
    , makeItem "that killed" "the rat"
    ]
  where
    makeItem xs ys = (ys, (++ "\n" ++ xs ++ " " ++ ys))

initContext :: Context
initContext xs = unwords [ "This is", xs, "that Jack built." ]

-- 1: initContext (fst (rhymeInfos !! 0))
-- 2: (initContext . (snd (rhymeInfos !! 0))) (fst (rhymeInfos !! 1))
-- 3: (initContext . (snd (rhymeInfos !! 0)) . (snd (rhymeInfos !! 1))) (fst (rhymeInfos !! 2))

rhyme :: String
rhyme = ""

{-

This is <the house> that Jack built.

{ the house ==> the malt \n that lay in the house }

This is the malt \n that lay in the house that Jack built.

{ the malt => the rat \n that ate the malt }

This is the rat \n that ate the malt \n that lay in the house that Jack built.

{ the rat => the cat \n that killed the rat }

This is the cat \n  that killed the rat \n that ate the malt \n that lay in the house that Jack built.

...

-}

{-

start: This is <> that Jack built
insert <the house> into the hole we'll get first sentense

insert another context: "<> that lay in the house"
gives us:

This is <> that lay in the house that Jack built

we can category things by: context / normal text,
apply them to a context could give either a result or another context

-}
