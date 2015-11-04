module House
  ( rhyme
  ) where

-- a context is some text with a hole in it
type Context = String -> String

rhymeInfos :: [ (String, Context) ]
rhymeInfos =
    [ ( "the house"
      , (++ "\n\
            \that lay in the house"))
    , ( "the malt"
      , (++ "\n\
            \that ate the malt"))
    , ( "the rat"
      , (++ "\n\
            \that killed the rat"))
    ] -- todo: `makeItem "that lay in" "the house"`

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
