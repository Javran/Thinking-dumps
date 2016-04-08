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

readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
    stream <- takeMVar readVar
    -- get value under read pointer
    Item val tl <- takeMVar stream
    -- move pointer of read pointer forward
    putMVar readVar tl
    pure val

main :: IO ()
main = pure ()
