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
  , isReadyCoroutines :: Queue (Coroutine ())
  , isPausedCoroutines :: Queue (Paused (Coroutine ())) --Yieled coroutines go here till the ready ones finish
  }

newtype Paused a = Paused a --Is this necessary?

mkPaused :: Coroutine a -> Paused (Coroutine a)
mkPaused = Paused

data Exception
  = Return Value
  | RuntimeError String
  | ReadyCoroutineQueueEmpty

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
initInterpreterState = InterpreterState <$> builtinEnv <*> newQueue <*> newQueue

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

scheduleReadyCoroutine :: Coroutine () -> Interpreter ()
scheduleReadyCoroutine coroutine =
  gets isReadyCoroutines >>= enqueue coroutine

runNextReadyCoroutine :: Interpreter ()
runNextReadyCoroutine =
  gets isReadyCoroutines >>= dequeue >>= \case
    Nothing -> throwError ReadyCoroutineQueueEmpty
    Just Coroutine {..} -> do
      --setEnv corEnv
      corCont ()

schedulePausedCoroutine :: Paused (Coroutine ()) -> Interpreter ()
schedulePausedCoroutine (Paused coroutine) =
  gets isPausedCoroutines >>= enqueue (mkPaused coroutine)

--Move paused coroutines onto ready coroutine queue
unpauseFrame :: Interpreter ()
unpauseFrame = do
  readies <- gets isReadyCoroutines
  gets isPausedCoroutines >>= dequeue >>= \case
    Nothing -> return ()
    Just (Paused c) -> do
      enqueue c readies
      unpauseFrame

yield :: Interpreter ()
yield = do
  env <- gets isEnv
  callCC $ \cont -> do
    newCoroutine env cont >>= schedulePausedCoroutine . mkPaused
    runNextReadyCoroutine

execute :: Stmt -> Interpreter ()
execute = \case
  PrintStmt s -> liftIO $ print s
  YieldStmt -> yield
  --SleepStmt t -> undefined --TODOLANGFAR
  s@(DoWhile b) -> traverse_ execute b >> execute s

awaitFrame :: Interpreter ()
awaitFrame = do --TODO run till ready coroutines are exhausted
  readyCoroutines <- readIORef =<< gets isReadyCoroutines
  unless (PQ.null readyCoroutines) $ yield

type Program = [Stmt]

scheduleProgram :: Program -> Interpreter ()
scheduleProgram prog =
  --TODO builtinenv would go here
  scheduleReadyCoroutine $ Coroutine M.empty $ \() -> do
    traverse_ execute prog
    runNextReadyCoroutine

bothQueuesEmpty :: InterpreterState -> IO Bool
bothQueuesEmpty is = do
  rq <- readIORef $ isReadyCoroutines is
  pq <- readIORef $ isPausedCoroutines is
  return $ PQ.null rq && PQ.null pq

runInterpreterT :: InterpreterState -> Interpreter () -> IO (Either Exception (), InterpreterState)
runInterpreterT s = flip runStateT s
                  . flip runContT return
                  . runExceptT
                  . runInterpreter

-- Initializes the interpreter state and schedules the programs before running the progs
interpretProgs :: [Program] -> IO (Either String ())
interpretProgs programs = do
  s <- initInterpreterState
  (_, s') <- runInterpreterT s $ do
        traverse_ scheduleProgram programs
  interpretProgs' programs s'

-- Runs ready coroutines till completion then runs the interpretProgs' again
-- We can adapt this to the game easier this way
interpretProgs' :: [Program] -> InterpreterState -> IO (Either String ())
interpretProgs' programs state = do
  (retVal, s') <- runInterpreterT state
    $ do
      liftIO $ putStrLn "Starting coroutines"
      unpauseFrame >> runNextReadyCoroutine -- >> awaitFrame >> unpauseFrame
  case retVal of
    Left (RuntimeError err) -> return $ Left err
    Left (Return _) -> return $ Left "Cannot return from outside functions"
    Left ReadyCoroutineQueueEmpty -> do
      finished <- bothQueuesEmpty s'
      if not finished
      then interpretProgs' programs s'
      else return $ Right ()
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

--TODO give example of running this in an AppM stack with a larger state record and plumb IORefs for a script to touch
