{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Concurrent (threadDelay)

import qualified DBus.Client as DBus

import           System.Environment (getArgs)

import           XNotify (Note (..))
import qualified XNotify


main :: IO ()
main = do
  (arg : _) <- getArgs
  client <- DBus.connectSession
  n0 <-
    XNotify.notify client XNotify.blankNote {
        appName = "xnotify"
      , body = Just (XNotify.Text arg)
      }
  threadDelay 5000000
  _n1 <-
    XNotify.replace client n0 XNotify.blankNote {
        appName = "xnotify"
      , body = Just (XNotify.Text "<redacted>")
      }
  pure ()
