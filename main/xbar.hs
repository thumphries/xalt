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
  TP.taffyPagerNew TP.defaultPagerConfig

clock :: IO Widget
clock =
  TC.textClockNew Nothing "%a %b %d %Y %H:%M" 60.0

battery :: IO Widget
battery =
  TB.textBatteryNew "$percentage$%" 60.0
