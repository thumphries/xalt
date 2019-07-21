{-# LANGUAGE OverloadedStrings #-}
module XFocus.DBus.Server (
    fork
  , exec
  ) where


import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, when)

import qualified DBus.Client as DBus

import           XFocus.API
-- import           XFocus.Task


exec :: DBus.Client -> API -> IO ()
exec client api = do
  fork client api
  forever (threadDelay 3000000)

fork :: DBus.Client -> API -> IO ()
fork client _api = do
  -- Request a unique name on the bus.
  requestResult <- DBus.requestName client "me.utf8.xfocus" []
  when (requestResult /= DBus.NamePrimaryOwner) $
    fail "Another service owns the \"com.example.exporting\" bus name"
  pure ()
