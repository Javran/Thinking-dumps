import Control.Concurrent hiding (Chan)
import Control.Monad
import System.IO

data Chan a
  = Chan (MVar (Stream a))
         (MVar (Stream a))
data Item a = Item a (Stream a)

type Stream a = MVar (Item a)

newChan :: IO (Chan a)
newChan = do
    -- new channel with empty content
    hole <- newEmptyMVar
    readVar <- newMVar hole
    writeVar <- newMVar hole
    pure (Chan readVar writeVar)

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
    newHole <- newEmptyMVar
    oldHole <- takeMVar writeVar
    -- put value under writer pointer
    putMVar oldHole (Item val newHole)
    -- move write pointer forward
    putMVar writeVar newHole

main :: IO ()
main = pure ()
