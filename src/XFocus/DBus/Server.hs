{-# LANGUAGE OverloadedStrings #-}
module XFocus.DBus.Server (
    exec
  ) where


import qualified DBus.Client as DBus

import           IPC.DBus

import           XFocus.API
import           XFocus.DBus.Common


exec :: DBus.Client -> API -> IO ()
exec client api = do
  server client interface task [
      exporting submit (submit' api)
    , exporting status (status' api)
    ]

submit' :: API -> SubmitRequest -> IO (Maybe SubmitResponse)
submit' = apiSubmit

status' :: API -> StatusRequest -> IO (Maybe StatusResponse)
status' = apiStatus
