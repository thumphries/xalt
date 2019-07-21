{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified DBus.Client as DBus

import           XFocus.API
import           XFocus.API.Memory
import           XFocus.DBus.Server
import           XFocus.Task

import           XTime (minute)


main :: IO ()
main = do
  api <- newAPI
  client <- DBus.connectSession
  _ <-
    apiSubmit api . SubmitRequest $
      Task {
          taskName = TaskName "Hello, world!"
        , taskDuration = minute
        }
  exec client api
