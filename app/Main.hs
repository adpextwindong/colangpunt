{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.State
import Control.Monad.Base
import Control.Concurrent.MVar
import System.Clock
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Map as M
import Data.IORef.Lifted

main :: IO ()
main = print "hello world"

-- See page 14 of the SCUMM Tutorial 0.1 pdf
-- First POC two clocks ticking at different reallocBytes
-- Second POC cuckoo clock where it spawns a cuckoo that waits n seconds and prints

data Value      --TODO
data Identifier --TODO

type Env = M.Map Identifier (IORef Value)

data Stmt
  = PrintStmt String
  | SleepStmt Int
  deriving Show


data InterpreterState = InterpreterState
  { isEnv :: Env
  , isCoroutines :: Queue (Coroutine ())
  }

newtype Interpreter a = Interpreter {
  runInterpreter :: StateT InterpreterState IO a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState InterpreterState
    , MonadIO
    , MonadBase IO
    )

builtinEnv :: IO Env
builtinEnv = return M.empty --TODO builtins?

initInterpreterState :: IO InterpreterState
initInterpreterState = InterpreterState <$> builtinEnv <*> newQueue

currentSystemTime :: MonadIO m => m TimeSpec
currentSystemTime = liftIO $ getTime Monotonic

data Coroutine a = Coroutine
  { corEnv :: Env
  , corCont :: a -> Interpreter ()
  , corReady :: MVar TimeSpec
  }

newCoroutine :: Env -> (a -> Interpreter ()) -> Interpreter (Coroutine a)
newCoroutine env k = do
  ready <- liftIO $ newMVar =<< currentSystemTime
  return $ Coroutine env k ready

type Queue a = IORef (PQ.MinPQueue TimeSpec a, TimeSpec)

newQueue :: MonadBase IO m => m (Queue a)
newQueue = do
  now <- liftBase currentSystemTime
  newIORef (PQ.empty, now)

queueSize :: MonadBase IO m => Queue a -> m Int
queueSize = fmap (PQ.size . fst) . readIORef

--TODO SCHEDULING COROUTINES
-- https://abhinavsarkar.net/posts/implementing-co-3/
