module Main where


import           Graphics.UI.Gtk (Widget)

import qualified System.Taffybar as TM
import qualified System.Taffybar.Battery as TB
import qualified System.Taffybar.SimpleClock as TC
import qualified System.Taffybar.TaffyPager as TP


main :: IO ()
main = do
  TM.taffybarMain TM.defaultTaffybarConfig {
      TM.startWidgets = [
          pager
        ]
    , TM.endWidgets = [
          clock
        , battery
        ]
    }

pager :: IO Widget
pager =
  TP.taffyPagerNew TP.PagerConfig {
      activeWindow     = TP.escape . TP.shorten 140
    , activeLayout     = TP.escape
    , activeWorkspace  = TP.colorize "yellow" "" . TP.wrap "[" "]" . TP.escape
    , hiddenWorkspace  = TP.escape
    , emptyWorkspace   = TP.escape
    , visibleWorkspace = TP.wrap "(" ")" . TP.escape
    , urgentWorkspace  = TP.colorize "red" "yellow" . TP.escape
    , widgetSep        = " : "
    }

clock :: IO Widget
clock =
  TC.textClockNew Nothing "%a %b %d %Y %H:%M" 60.0

battery :: IO Widget
battery =
  TB.textBatteryNew "$percentage$%" 60.0
