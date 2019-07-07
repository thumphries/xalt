{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Chronos (Time, Timespan (..))
import qualified Chronos

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TMVar (TMVar)
import qualified Control.Concurrent.STM.TMVar as TMVar
import           Control.Monad (forever, when)

import           Data.Int (Int64)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified DBus.Client as DBus

import           XFocus.Task


main :: IO ()
main = do
  api <- newAPI
  client <- DBus.connectSession
  dbus client api
  forever $ threadDelay 3000000

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
-- DBus

dbus :: DBus.Client -> API -> IO ()
dbus client api = do
  -- Request a unique name on the bus.
  requestResult <- DBus.requestName client "me.utf8.xfoc" []
  when (requestResult /= DBus.NamePrimaryOwner) $
    fail "Another service owns the \"com.example.exporting\" bus name"

  DBus.export client "/xfoc"
    DBus.defaultInterface {
        DBus.interfaceName = "me.utf8.xfoc"
      , DBus.interfaceMethods = [
            DBus.autoMethod "Submit" (submit'' api)
          , DBus.autoMethod "Status" (status'' api)
          ]
      }

submit'' :: API -> Text -> Int64 -> IO ()
submit'' api tn tm = do
  putStrLn $ "submit " ++ show tn ++ show tm
  _ <- apiSubmit api (Task (TaskName tn) (mins tm))
  pure ()

status'' :: API -> IO (Text, Text, Text)
status'' api = do
  putStrLn $ "status"
  msr <- apiStatus api
  let
    name = maybe "" (unTaskName . taskName . statusTask) msr
    status = maybe "" (T.pack . show . statusTaskStatus) msr
    remaining = maybe "" printRemaining msr
  pure $ (name, status, remaining)

printRemaining :: StatusResponse -> Text
printRemaining sr =
  case statusTaskStatus sr of
    StatusRunning elapsed ->
      renderTimespan
        (taskDuration (statusTask sr) `timespanDifference` elapsed)
    _ ->
      ""

mins :: Int64 -> Timespan
mins m =
  Timespan (getTimespan Chronos.minute * fromIntegral m)

-- ---------------------------------------------------------------------------
-- Stateful API

data API =
  API {
      apiSubmit :: Task -> IO (Maybe SubmitResponse)
    , apiStatus :: IO (Maybe StatusResponse)
    , apiWait :: IO (Maybe TaskResult)
    , apiStop :: StopReason -> IO ()
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
    , apiStop = stop' st
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
    Just (Just rt) -> do
      e <- A.waitCatch (runningTaskThread rt)
      case e of
        Left _e ->
          pure Nothing
        Right r ->
          pure (Just r)
    _ ->
      pure Nothing

stop' :: State -> StopReason -> IO ()
stop' st _reason = do
  mv <- STM.atomically $ TMVar.tryTakeTMVar st
  case mv of
    Just (Just rt) -> do
      A.cancel (runningTaskThread rt)
      STM.atomically $ TMVar.putTMVar st Nothing
    _ ->
      pure ()

-- ---------------------------------------------------------------------------
-- Task
