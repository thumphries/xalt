{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Main where


import           Graphics.UI.Gtk (Widget)

import qualified System.Taffybar as T
import           System.Taffybar.Context (TaffyIO)
import qualified System.Taffybar.SimpleConfig as SC
import qualified System.Taffybar.Widget.Battery as TB
import qualified System.Taffybar.Widget.Layout as TL
import qualified System.Taffybar.Widget.MPRIS2 as TM
import qualified System.Taffybar.Widget.SimpleClock as TC
import qualified System.Taffybar.Widget.Systray as TS
import qualified System.Taffybar.Widget.Workspaces as TW

import           Text.Printf (printf)


main :: IO ()
main = do
  T.startTaffybar . SC.toTaffyConfig $ SC.defaultSimpleTaffyConfig {
      SC.startWidgets = [
          workspaces
        , layout
        ]
    , SC.endWidgets = [
          systray
        , clock
        , battery
        , music
        ]
    }

workspaces :: TaffyIO Widget
workspaces =
  TW.workspacesNew TW.defaultWorkspacesConfig {
      TW.labelSetter = workspaceLabel
    -- Disable taffybar-2.0 icon nonsense
    , TW.getWindowIconPixbuf = \_ _ -> return Nothing
    , TW.maxIcons = Just 0
    }

workspaceLabel :: TW.Workspace -> TW.WorkspacesIO String
workspaceLabel (TW.Workspace _idx name _state _windows) =
  return $ iconWorkspace name

iconWorkspace :: String -> String
iconWorkspace ws =
  case ws of
    "web" -> iconWeb
    "code" -> iconCode
    a -> a

layout :: TaffyIO Widget
layout =
  TL.layoutNew TL.defaultLayoutConfig {
      TL.formatLayout = pure
    }

systray :: TaffyIO Widget
systray =
  TS.systrayNew

clock :: TaffyIO Widget
clock =
  TC.textClockNew Nothing (iconClock ++ " %a %b %d %Y %H:%M") 60.0

battery :: TaffyIO Widget
battery =
  TB.textBatteryNew (iconBattery ++ " $percentage$%")

music :: TaffyIO Widget
music =
  TM.mpris2New

-- -----------------------------------------------------------------------------

fontAwesome :: String -> String
fontAwesome = printf "<span font_desc='Font Awesome 5 Free' font_size='large'>%s</span>"

iconWeb :: String
iconWeb = fontAwesome "\xf268"

iconCode :: String
iconCode = fontAwesome "\xf121"

iconClock :: String
iconClock = fontAwesome "\xf017"

iconBattery :: String
iconBattery = fontAwesome "\xf242"
