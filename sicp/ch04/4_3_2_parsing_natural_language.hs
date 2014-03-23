import Control.Monad.State

{- attempt #1 put them together into a db?
classifyDb :: [(String, [String])]
classifyDb =
    [ ( "nouns"
      , [ "student", "professor", "cat", "class" ] )
    , ( "verbs"
      , [ "studies", "lectures", "eats", "sleeps" ] )
    , ( "articles"
      , [ "the", "a" ] )
    ]

-}

-- attempt #2 types

data Nouns
    = Student
    | Professor
    | Cat
    | Class
      deriving (Show, Eq, Enum, Bounded)

data Verbs
    = Studies
    | Lectures
    | Eats
    | Sleeps
      deriving (Show, Eq, Enum, Bounded)

data Articles
    = The
    | A_
      deriving (Show, Eq, Enum, Bounded)

-- problem: we don't have the flexibility
-- because of the type constraints..
-- use another type to union them would work,
-- but that's boring

main :: IO ()
main = undefined
