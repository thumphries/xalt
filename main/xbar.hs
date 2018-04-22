module Main where


import           Graphics.UI.Gtk (Widget)

import qualified System.Taffybar as TB
import qualified System.Taffybar.SimpleClock as TC
import qualified System.Taffybar.TaffyPager as TP


main :: IO ()
main = do
  TB.taffybarMain TB.defaultTaffybarConfig {
      TB.startWidgets = [
          pager
        ]
    , TB.endWidgets = [
          clock
        ]
    }

pager :: IO Widget
pager =
  TP.taffyPagerNew TP.defaultPagerConfig

clock :: IO Widget
clock =
  TC.textClockNew Nothing "%a %b %d %Y %H:%M " 60.0
