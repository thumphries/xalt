{-# LANGUAGE OverloadedStrings #-}
module XBar.Widget.Focus (
    widget
  ) where


import           Control.Monad.IO.Class (liftIO)

import           Data.Text (Text)
import qualified Data.Text as T

import qualified DBus.Client as DBus

import           GI.Gtk (Widget)

import           System.Taffybar.Context (TaffyIO)
import qualified System.Taffybar.Widget.Generic.PollingLabel as PL

import qualified XFocus.API as XFocus
import qualified XFocus.DBus.Client as XFocus
import           XFocus.Task (Task (..), TaskStatus (..))

import qualified XTime as XT


widget :: TaffyIO Widget
widget = do
  client <- liftIO DBus.connectSession
  liftIO $
    PL.pollingLabelNew "" 1.0 $ do
      sr <- XFocus.status client XFocus.StatusRequest
      pure (renderStatusResponse sr)

renderStatusResponse :: XFocus.StatusResponse -> Text
renderStatusResponse (XFocus.StatusResponse task _started status) =
  case status of
    StatusRunning d ->
      let
        remaining =
          taskDuration task - d
      in
        T.unwords [
            boldSpan (XT.renderDuration remaining)
          ]

    StatusComplete ->
      T.unwords [
          boldSpan "COMPLETE"
        ]

boldSpan :: Text -> Text
boldSpan b =
  "<span font_weight=\"bold\">" <> b <> "</span>"

-- colorSpan :: Text -> Text -> Text
-- colorSpan c b =
--   "<span foreground=\"" <> c <> "\">" <> b <> "</span>"
