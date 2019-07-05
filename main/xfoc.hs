{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Chronos (Time, Timespan (..))
import qualified Chronos

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as A
import           Control.Monad (forever)

import           Data.Text (Text)

import qualified System.Clock as Clock


main :: IO ()
main = do
  rt <-
    runTask $
      Task {
          taskName = TaskName "First Task"
        , taskDuration = Chronos.minute
        }

  poll <-
    A.async . forever $ do
      delayTimespan Chronos.second
      status <- runningTaskPoll rt
      print status

  result <- A.wait (runningTaskThread rt)
  A.cancel poll
  print result


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
