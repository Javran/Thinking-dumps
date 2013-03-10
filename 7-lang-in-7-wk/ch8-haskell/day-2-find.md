## Find functions that can opearate List/String/Tuple

I don't know what it exactly means. Basically type `String` and `[Char]` are identical:

    Prelude> :info String
    type String = [Char] 	-- Defined in `GHC.Base'

So what we need are some functions that accept Lists or Tuples.

* [Lists and Tuples](http://en.wikibooks.org/wiki/Haskell/Lists_and_tuples)
* [LYAH -- An intro to lists](http://learnyouahaskell.com/starting-out#an-intro-to-lists)
* [LYAH -- Tuples](http://learnyouahaskell.com/starting-out#tuples)
* [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html)
* [Data.Tuple](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Tuple.html)

## Method to sort a list

2 native functions can achieve this:

* [Data.List.sort](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#v:sort)
* [Data.List.sortBy](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#v:sortBy)

where `sortBy` compares item with the help of an ordering function.

We can make our own sort function! Check `day-2-do-my-sort.hs`.
