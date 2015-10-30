module PigLatin
  ( translate
  ) where

{-
  some complaints: it's really frustrating that
  an exercise have no clean rule of how the program
  should behavior and we have to look at testcases to find out
  what's going on.
-}

{-
  spreaking of rules, these 2 rules can be unified with following operation:
  * split a word into two parts: `xs` and `ys`
    * if it's [aeiou], stop and make remaining string `ys`
    * if it's "qu", append it to `xs`
      (in other words, "qu" should be treated as a single unit)
    * otherwise, the current char to `xs`

  * `ys ++ xs ++ "ay"` is the result
-}


translate :: String -> String
translate ws = undefined
