module XFocus.API.Memory (
    newAPI
  ) where


import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TMVar (TMVar)
import qualified Control.Concurrent.STM.TMVar as TMVar

import           XFocus.API
import           XFocus.Task


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
