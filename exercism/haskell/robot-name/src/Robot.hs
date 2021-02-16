module Robot
  ( robotName
  , mkRobot
  , resetName
  , initialState
  )
where

import Control.Arrow
import Control.Monad.Random
import Control.Monad.State
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Unique

type RunState =
  ( -- Names for all existing robots
    S.Set T.Text
  , -- Names used for each individual robots
    M.Map Unique (S.Set T.Text)
  )

type Robot = (Unique, IORef T.Text)

type M = StateT RunState IO

initialState :: RunState
initialState = (S.empty, M.empty)

genName :: M T.Text
genName =
  T.pack
    <$> ((<>)
           <$> replicateM 2 (getRandomR ('A', 'Z'))
           <*> replicateM 3 (getRandomR ('0', '9')))

-- | Generates a new Robot name while avoiding collision with existing names
--   and all names used by itself in the past.
--   Newly generated name also gets registered in RunState.
mkNewRobotName :: Unique -> M T.Text
mkNewRobotName u = do
  (allNames, pastNameMap) <- get
  let pastNames = pastNameMap M.! u
  n <- fix $ \redo -> do
    n <- genName
    if n `elem` allNames || n `elem` pastNames
      then redo
      else pure n
  n <$ modify (S.insert n *** M.adjust (S.insert n) u)

mkRobot :: M Robot
mkRobot = do
  u <- lift newUnique
  modify (second (M.insert u S.empty))
  n <- mkNewRobotName u
  ref <- lift $ newIORef n
  pure (u, ref)

resetName :: Robot -> M ()
resetName r@(u, ref) = do
  n <- lift $ robotNameT r
  modify (first (S.delete n))
  n' <- mkNewRobotName u
  lift $ writeIORef ref n'

robotName :: Robot -> IO String
robotName = fmap T.unpack . robotNameT

robotNameT :: Robot -> IO T.Text
robotNameT (_, ref) = readIORef ref
