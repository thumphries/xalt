{-# LANGUAGE OverloadedStrings #-}
module XFocus.DBus.Server (
    fork
  , exec
  ) where


import           Chronos (Timespan (..))
import qualified Chronos

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, when)

import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T

import qualified DBus.Client as DBus

import           XFocus.API
import           XFocus.Task


exec :: DBus.Client -> API -> IO ()
exec client api = do
  fork client api
  forever (threadDelay 3000000)

fork :: DBus.Client -> API -> IO ()
fork client api = do
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
