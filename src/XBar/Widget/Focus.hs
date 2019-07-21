{-# LANGUAGE OverloadedStrings #-}
module XBar.Widget.Focus (
    widget
  ) where


import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import qualified DBus.Client as DBus

import           GI.Gtk (Widget)

import           System.Taffybar.Context (TaffyIO)
import qualified System.Taffybar.Widget.Generic.PollingLabel as PL

import qualified XFocus.API as XFocus
import qualified XFocus.DBus.Client as XFocus


widget :: TaffyIO Widget
widget = do
  client <- liftIO DBus.connectSession
  liftIO $
    PL.pollingLabelNew "" 1.0 $ do
      sr <- XFocus.status client XFocus.StatusRequest
      pure $ T.pack (show sr)
