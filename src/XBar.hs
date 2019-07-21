{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module XBar where


import           GI.Gtk (Widget)

import qualified System.Taffybar as T
import           System.Taffybar.Context (TaffyIO)
import qualified System.Taffybar.SimpleConfig as SC
import qualified System.Taffybar.Widget.Layout as TL
import qualified System.Taffybar.Widget.SimpleClock as TC
import qualified System.Taffybar.Widget.Workspaces as TW

import qualified XBar.Widget.Battery as Battery
import qualified XBar.Widget.Focus as Focus


xbar :: IO ()
xbar =
  T.startTaffybar . SC.toTaffyConfig $ SC.defaultSimpleTaffyConfig {
      SC.barHeight = 32
    , SC.widgetSpacing = 16
    , SC.startWidgets = [
          workspaces
        , layout
        ]
    , SC.endWidgets = [
          clock
        , Battery.widget
        , Focus.widget
        ]
    }

workspaces :: TaffyIO Widget
workspaces =
  TW.workspacesNew TW.defaultWorkspacesConfig {
    --  TW.labelSetter = workspaceLabel
    -- Disable taffybar-2.0 icon nonsense
      TW.getWindowIconPixbuf = \_ _ -> return Nothing
    , TW.maxIcons = Just 0
    }

layout :: TaffyIO Widget
layout =
  TL.layoutNew TL.defaultLayoutConfig {
      TL.formatLayout = pure
    }

clock :: TaffyIO Widget
clock =
  TC.textClockNew Nothing ("<span font_weight=\"bold\">%a %b %d %Y %H:%M</span>") 60.0
