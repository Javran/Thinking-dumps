## a few monad tutorials

* [Monad - HaskellWiki](http://www.haskell.org/haskellwiki/Monad)

* [Strong Typed: A monad non-tutorial](http://strongtyped.blogspot.jp/2010/01/monad-non-tutorial.html)

* [Understanding monads](http://en.wikibooks.org/wiki/Haskell/Understanding_monads)

* [Monad in Wikipedia](http://en.wikipedia.org/wiki/Monad_(functional_programming))

* [A fistful of Monads - LYAH](http://learnyouahaskell.com/a-fistful-of-monads)

* [Monads - RWH](http://book.realworldhaskell.org/read/monads.html)

## a list of the monads in Haskell

Actually you can use `ghci` to discover some of them
by run command `:info Monad`

The output should be like:

	Prelude> :info Monad
	class Monad m where
		(>>=) :: m a -> (a -> m b) -> m b
		(>>) :: m a -> m b -> m b
		return :: a -> m a
		fail :: String -> m a
  			-- Defined in `GHC.Base'

	instance Monad Maybe -- Defined in `Data.Maybe'
	instance Monad (Either e) -- Defined in `Data.Either'
	instance Monad [] -- Defined in `GHC.Base'
	instance Monad IO -- Defined in `GHC.Base'
	instance Monad ((->) r) -- Defined in `GHC.Base'
	Prelude> 

So, `Maybe`, `Either e`, List(`[]`), `IO` and `((->) r)` are all monads.

There are many other monads in Haskell platform, e.g.: 

* [State Monad](http://cvs.haskell.org/Hugs/pages/libraries/mtl/Control-Monad-State.html)

* [Reader Monad](http://cvs.haskell.org/Hugs/pages/libraries/mtl/Control-Monad-Reader.html)

* [Writer Monad](http://cvs.haskell.org/Hugs/pages/libraries/mtl/Control-Monad-Writer.html)

* [Cont Monad](http://cvs.haskell.org/Hugs/pages/libraries/mtl/Control-Monad-Cont.html)

* [Identity Monad](http://cvs.haskell.org/Hugs/pages/libraries/mtl/Control-Monad-Identity.html)
