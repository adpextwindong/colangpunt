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
builtinEnv = return M.empty

initInterpreterState :: IO InterpreterState
initInterpreterState = InterpreterState <$> builtinEnv <*> newQueue

currentSystemTime :: MonadIO m => m TimeSpec
currentSystemTime = liftIO $ getTime Monotonic

type Queue a = IORef (PQ.MinPQueue TimeSpec a)

newQueue :: MonadBase IO m => m (Queue a)
newQueue = newIORef PQ.empty

--queueSize :: MonadBase IO m => Queue a -> m Int
--queueSize = fmap PQ.size . readIORef

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

data Coroutine a = Coroutine
  { corEnv :: Env
  , corCont :: a -> Interpreter ()
  }

newCoroutine :: Env -> (a -> Interpreter ()) -> Interpreter (Coroutine a)
newCoroutine env k = do
  return $ Coroutine env k

scheduleCoroutine :: Coroutine () -> Interpreter ()
scheduleCoroutine coroutine =
  gets isCoroutines >>= enqueue coroutine

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
  s@(DoWhile b) -> traverse_ execute b >> execute s

awaitTermination :: Interpreter ()
awaitTermination = do
  coroutines <- readIORef =<< gets isCoroutines
  unless (PQ.null coroutines) $ yield >> awaitTermination

type Program = [Stmt]

scheduleProgram :: Program -> Interpreter ()
scheduleProgram prog =
  --TODO builtinenv would go here
  scheduleCoroutine $ Coroutine M.empty $ \() -> do
    traverse_ execute prog
    runNextCoroutine

interpretProgs :: [Program] -> IO (Either String ())
interpretProgs programs = do
  state <- initInterpreterState
  retVal <- flip evalStateT state
    . flip runContT return
    . runExceptT
    . runInterpreter
    $ do
      traverse_ scheduleProgram programs
      liftIO $ print "Starting coroutines"
      runNextCoroutine >> awaitTermination
  case retVal of
    Left (RuntimeError err) -> return $ Left err
    Left (Return _) -> return $ Left "Cannot return from outside functions"
    Left CoroutineQueueEmpty -> return $ Right ()
    Right _ -> return $ Right ()


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

p1zz = [PrintStmt "foo", YieldStmt
       ,PrintStmt "fooooz", YieldStmt
       ,PrintStmt "faaaz", YieldStmt]

p2yy = [PrintStmt "bar", YieldStmt
       ,PrintStmt "baz", YieldStmt
       ,PrintStmt "boof", YieldStmt]

p1y = [PrintStmt "quux", YieldStmt]

--interleaving two prints now work with interpretProgs [p1w, p2w]
p1w = [DoWhile [PrintStmt "foo", YieldStmt]]
p2w = [DoWhile [PrintStmt "bar", YieldStmt]]

--TODO scheme to run all coroutines till yield, not till awaitTermination
--TODO give example of running this in an AppM stack with a larger state record and plumb IORefs for a script to touch
