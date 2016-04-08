import Control.Concurrent
import Control.Monad
import System.IO

data Chan a
  = Chan (MVar (Stream a))
         (MVar (Stream a))
data Item a = Item a (Stream a)

type Stream a = MVar (Item a)

main :: IO ()
main = pure ()
