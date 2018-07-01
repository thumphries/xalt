module Main where


import           Graphics.UI.Gtk (Widget)

import qualified System.Taffybar as T
import qualified System.Taffybar.Battery as TB
import qualified System.Taffybar.MPRIS2 as TM
import qualified System.Taffybar.Pager as TP
import qualified System.Taffybar.SimpleClock as TC
import qualified System.Taffybar.Systray as TS
import qualified System.Taffybar.TaffyPager as TP

import           Text.Printf (printf)


main :: IO ()
main = do
  T.taffybarMain T.defaultTaffybarConfig {
      T.startWidgets = [
          pager
        ]
    , T.endWidgets = [
          systray
        , clock
        , battery
        , music
        ]
    }

pager :: IO Widget
pager =
  TP.taffyPagerNew TP.PagerConfig {
      TP.activeWindow     = TP.escape . TP.shorten 200
    , TP.activeLayout     = TP.escape
    , TP.activeWorkspace  = TP.colorize "yellow" "" . TP.escape . iconWorkspace
    , TP.hiddenWorkspace  = TP.escape . iconWorkspace
    , TP.emptyWorkspace   = TP.escape . iconWorkspace
    , TP.visibleWorkspace = TP.wrap "(" ")" . TP.escape
    , TP.urgentWorkspace  = TP.colorize "red" "yellow" . TP.escape . iconWorkspace
    , TP.widgetSep        = " : "
    }

iconWorkspace :: String -> String
iconWorkspace ws =
  case ws of
    "web" -> iconWeb
    "code" -> iconCode
    a -> a

systray :: IO Widget
systray =
  TS.systrayNew

clock :: IO Widget
clock =
  TC.textClockNew Nothing (fontAwesome iconClock ++ " %a %b %d %Y %H:%M") 60.0

battery :: IO Widget
battery =
  TB.textBatteryNew (iconBattery ++ " $percentage$%") 60.0

music :: IO Widget
music =
  TM.mpris2New

-- -----------------------------------------------------------------------------

fontAwesome :: String -> String
fontAwesome = printf "<span font_desc='Font Awesome 5 Free'>%s</span>"

iconWeb :: String
iconWeb = "\xf268"

iconCode :: String
iconCode = "\xf121"

iconClock :: String
iconClock = "\xf017"

iconBattery :: String
iconBattery = "\xf242"
