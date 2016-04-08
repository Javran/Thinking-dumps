import Control.Concurrent hiding (Chan,readChan,newChan,writeChan)
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

-- TODO: test channel by starting multiple threads
-- that takes elements from the channel
-- and use a single thread to feed the channel
-- because the consideration of fairness,
-- each thread should get a good chance taking some values
-- from the channel

-- take n elements from the channel and put it on MVar
listConsumer :: Int -> Chan Int -> MVar [Int] -> IO ()
listConsumer n chan retVar = do
    xs <- replicateM n (readChan chan)
    putMVar retVar xs

listProducer :: Chan Int -> IO a
listProducer chan = do
    let loop n = writeChan chan n >> loop (n+1)
    loop 0

main :: IO ()
main = do
    c <- newChan
    forkIO $ listProducer c

    cs <- replicateM 10 newEmptyMVar
    forM_ cs $ \mvar -> forkIO $ do
        listConsumer 10 c mvar

    -- wait for all consumers to stop their tasks
    csResult <- mapM takeMVar cs
    mapM_ print csResult
    pure ()
