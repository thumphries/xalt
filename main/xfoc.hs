{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified DBus.Client as DBus

import           XFocus.API.Memory
import           XFocus.DBus.Server


main :: IO ()
main = do
  api <- newAPI
  client <- DBus.connectSession
  exec client api
