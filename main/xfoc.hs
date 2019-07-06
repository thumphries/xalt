{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Chronos (Time, Timespan (..))
import qualified Chronos

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TMVar (TMVar)
import qualified Control.Concurrent.STM.TMVar as TMVar

import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text.IO as T

import qualified System.Clock as Clock


main :: IO ()
main = do
  api <- newAPI

  _ <-
    apiSubmit api Task {
        taskName = TaskName "Second Task"
      , taskDuration = Chronos.minute
      }

  subscribe api

subscribe :: API -> IO ()
subscribe api = do
  let
    poll = do
      delayTimespan Chronos.second
      StatusResponse task _started status <- fromJust <$> apiStatus api -- FIXME
      case status of
        StatusComplete ->
          pure ResultComplete
        StatusRunning elapsed -> do
          T.putStrLn $
               unTaskName (taskName task)
            <> ": "
            <> renderTimespan
                 (taskDuration task `timespanDifference` elapsed)
          poll
  result <- A.race poll (apiWait api)
  print result

-- ---------------------------------------------------------------------------
-- Stateful API

data API =
  API {
      apiSubmit :: Task -> IO (Maybe SubmitResponse)
    , apiStatus :: IO (Maybe StatusResponse)
    , apiWait :: IO (Maybe TaskResult)
--    , stop :: Reason -> IO ()
    }

data SubmitResponse =
  SubmitResponse {
      submitTask :: Task
    , submitTaskStarted :: Time
    } deriving (Eq, Ord, Show)

data StatusResponse =
  StatusResponse {
      statusTask :: Task
    , statusTaskStarted :: Time
    , statusTaskStatus :: TaskStatus
    } deriving (Eq, Ord, Show)

newAPI :: IO API
newAPI = do
  st <- newState
  pure API {
      apiSubmit = submit' st
    , apiStatus = status' st
    , apiWait = wait' st
    }

type State = TMVar (Maybe RunningTask)

newState :: IO State
newState =
  TMVar.newTMVarIO Nothing

submit' :: State -> Task -> IO (Maybe SubmitResponse)
submit' st t = do
  v <- STM.atomically $ TMVar.takeTMVar st
  case v of
    Just _task -> do
      STM.atomically $ TMVar.putTMVar st v
      pure Nothing
    Nothing -> do
      rt <- runTask t
      STM.atomically $ TMVar.putTMVar st (Just rt)
      pure . Just $
        SubmitResponse {
            submitTask = runningTask rt
          , submitTaskStarted = runningTaskStarted rt
          }

status' :: State -> IO (Maybe StatusResponse)
status' st = do
  mv <- STM.atomically $ TMVar.tryReadTMVar st
  case mv of
    Just (Just rt) -> do
      ts <- runningTaskPoll rt
      pure . Just $
        StatusResponse {
            statusTask = runningTask rt
          , statusTaskStarted = runningTaskStarted rt
          , statusTaskStatus = ts
          }
    _ ->
      pure Nothing

wait' :: State -> IO (Maybe TaskResult)
wait' st = do
  mv <- STM.atomically $ TMVar.tryReadTMVar st
  case mv of
    Just (Just rt) ->
      fmap Just (A.wait (runningTaskThread rt))
    _ ->
      pure Nothing

-- ---------------------------------------------------------------------------
-- Event loop



-- ---------------------------------------------------------------------------
-- Task

data Task =
  Task {
      taskName :: TaskName
    , taskDuration :: Timespan
    } deriving (Eq, Ord, Show)

newtype TaskName =
  TaskName {
      unTaskName :: Text
    } deriving (Eq, Ord, Show)

data TaskStatus =
    StatusRunning Timespan
  | StatusComplete
  deriving (Eq, Ord, Show)

data TaskResult =
    ResultComplete
  deriving (Eq, Ord, Show)

data RunningTask =
  RunningTask {
      runningTask :: Task
    , runningTaskStarted :: Time
    , runningTaskPoll :: IO TaskStatus
    , runningTaskThread :: Async TaskResult
    }

runTask :: Task -> IO RunningTask
runTask t@(Task _name duration) = do
  now <- Chronos.now

  t0 <- Clock.getTime Clock.Boottime
  let
    poll =
      checkElapsed duration t0 (pure . StatusRunning) (pure StatusComplete)

    tick = do
      checkElapsed duration t0
        (\elapsed -> do
           delayTimespan (duration `timespanDifference` elapsed)
           tick)
        (pure ResultComplete)

  thread <- A.async tick

  pure RunningTask {
      runningTask = t
    , runningTaskStarted = now
    , runningTaskPoll = poll
    , runningTaskThread = thread
    }

stopTask :: RunningTask -> IO ()
stopTask rt =
  A.cancel (runningTaskThread rt)

checkElapsed :: Timespan -> Clock.TimeSpec -> (Timespan -> IO a) -> IO a -> IO a
checkElapsed duration t0 running done = do
  t1 <- Clock.getTime Clock.Boottime
  let elapsed = specToSpan (t1 - t0)
  case elapsed >= duration of
    True ->
      done
    False ->
      running elapsed

specToSpan :: Clock.TimeSpec -> Timespan
specToSpan (Clock.TimeSpec secs nanos) =
  Timespan (nanos + (secs * 1000000000))

delayTimespan :: Timespan -> IO ()
delayTimespan (Timespan nanos) =
  threadDelay (fromIntegral (nanos `div` 1000))

timespanDifference :: Timespan -> Timespan -> Timespan
timespanDifference t0 t1 =
  Timespan $
    getTimespan t0 - getTimespan t1

renderTimespan :: Timespan -> Text
renderTimespan =
  Chronos.encodeTimespan (Chronos.SubsecondPrecisionFixed 0)
