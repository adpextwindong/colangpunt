{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.State
import Control.Monad.Base
import Control.Monad.Cont
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Concurrent.MVar
import System.Clock
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Map as M
import Data.IORef.Lifted
import Data.Foldable

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
  | YieldStmt
  -- | SleepStmt Int
  | DoWhile [Stmt]
  deriving Show

data InterpreterState = InterpreterState
  { isEnv :: Env
  , isCoroutines :: Queue (Coroutine ())
  }

data Exception
  = Return Value
  | RuntimeError String
  | CoroutineQueueEmpty

--TODO

newtype Interpreter a = Interpreter {
  runInterpreter :: ExceptT Exception
                          (ContT
                              (Either Exception () )
                              (StateT InterpreterState IO))
                          a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState InterpreterState
    , MonadIO
    , MonadBase IO
    , MonadCont
    , MonadError Exception
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

type Queue a = IORef (PQ.MinPQueue TimeSpec a)

newQueue :: MonadBase IO m => m (Queue a)
newQueue = newIORef PQ.empty

queueSize :: MonadBase IO m => Queue a -> m Int
queueSize = fmap PQ.size . readIORef

--TODO SCHEDULING COROUTINES
-- https://abhinavsarkar.net/posts/implementing-co-3/

enqueueAt :: TimeSpec -> a -> Queue a -> Interpreter ()
enqueueAt time val queue = atomicModifyIORef' queue $ \q ->
  (PQ.insert time val q, ())

enqueue :: a -> Queue a -> Interpreter ()
enqueue val queue = do
  now <- currentSystemTime
  enqueueAt now val queue

dequeue :: Queue a -> Interpreter (Maybe a)
dequeue queue = atomicModifyIORef' queue $ \q ->
  if PQ.null q
  then (q, Nothing)
  else
    let ((_,val), q') = PQ.deleteFindMin q
    in (q', Just val)

scheduleCoroutine :: Coroutine () -> Interpreter ()
scheduleCoroutine coroutine =
  gets isCoroutines >>= enqueue coroutine

setEnv = undefined --TODO

runNextCoroutine :: Interpreter ()
runNextCoroutine =
  gets isCoroutines >>= dequeue >>= \case
    Nothing -> throwError CoroutineQueueEmpty
    Just Coroutine {..} -> do
      --setEnv corEnv
      corCont ()

yield :: Interpreter ()
yield = do
  env <- gets isEnv
  callCC $ \cont -> do
    newCoroutine env cont >>= scheduleCoroutine
    runNextCoroutine

execute :: Stmt -> Interpreter ()
execute = \case
  PrintStmt s -> liftIO $ print s
  YieldStmt -> yield
  --SleepStmt t -> undefined --TODO
  DoWhile b -> traverse_ execute b

awaitTermination :: Interpreter ()
awaitTermination = do
  coroutines <- readIORef =<< gets isCoroutines
  unless (PQ.null coroutines) $ yield >> awaitTermination
--TODO try interleaving two prints

type Program = [Stmt]

interpret :: Program -> IO (Either String ())
interpret program = do
  state <- initInterpreterState
  retVal <- flip evalStateT state
    . flip runContT return
    . runExceptT
    . runInterpreter
    $ (traverse_ execute program >> awaitTermination)
  case retVal of
    Left (RuntimeError err) -> return $ Left err
    Left (Return _) -> return $ Left "Cannot return from outside functions"
    Left CoroutineQueueEmpty -> return $ Right ()
    Right _ -> return $ Right ()

p1 = [PrintStmt "foo"]
p2 = [PrintStmt "bar"]
