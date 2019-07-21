{-# LANGUAGE OverloadedStrings #-}
module XFocus.DBus.Client (
    status
  , submit
  ) where


import qualified DBus.Client as DBus

import           IPC.DBus

import           XFocus.API
import qualified XFocus.DBus.Common as C


status :: DBus.Client -> StatusRequest -> IO StatusResponse
status client req =
  call client C.interface C.task C.status req

submit :: DBus.Client -> SubmitRequest -> IO SubmitResponse
submit client req =
  call client C.interface C.task C.submit req
