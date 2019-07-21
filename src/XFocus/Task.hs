{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module XFocus.Task (
    Task (..)
  , TaskName (..)
  , Duration (..)
  , StopReason (..)
  , TaskStatus (..)
  , TaskResult (..)
  , RunningTask (..)
  , runTask
  , stopTask
  ) where


import           Codec.Serialise (Serialise)

import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as A

import           Data.Text (Text)

import           GHC.Generics (Generic)

import           XTime (Time, Duration, Clock)
import qualified XTime as XT


data Task =
  Task {
      taskName :: TaskName
    , taskDuration :: Duration
    } deriving (Eq, Ord, Show, Generic)
instance Serialise Task

newtype TaskName =
  TaskName {
      unTaskName :: Text
    } deriving (Eq, Ord, Show, Generic)
instance Serialise TaskName

data StopReason =
    Abandon
  | ContextSwitch
  deriving (Eq, Ord, Show, Generic)
instance Serialise StopReason

data TaskStatus =
    StatusRunning Duration
  | StatusComplete
  deriving (Eq, Ord, Show, Generic)
instance Serialise TaskStatus

data TaskResult =
    ResultComplete
  deriving (Eq, Ord, Show, Generic)
instance Serialise TaskResult

data RunningTask =
  RunningTask {
      runningTask :: Task
    , runningTaskStarted :: Time
    , runningTaskPoll :: IO TaskStatus
    , runningTaskThread :: Async TaskResult
    }

runTask :: Task -> IO RunningTask
runTask t@(Task _name duration) = do
  now <- XT.now

  t0 <- XT.boot
  let
    poll =
      checkElapsed duration t0 (pure . StatusRunning) (pure StatusComplete)

    tick = do
      checkElapsed duration t0
        (\elapsed -> do
           XT.delayDuration (duration `XT.durationDifference` elapsed)
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

checkElapsed :: Duration -> Clock -> (Duration -> IO a) -> IO a -> IO a
checkElapsed duration t0 running done = do
  t1 <- XT.boot
  let elapsed = t1 `XT.clockDifference` t0
  case elapsed >= duration of
    True ->
      done
    False ->
      running elapsed
