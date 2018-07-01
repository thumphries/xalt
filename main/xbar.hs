{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Main where


import           Control.Monad (void)
import           Control.Monad.Reader (ask, runReaderT)
import           Control.Monad.IO.Class (liftIO)

import           Data.GI.Gtk.Threading (postGUIASync)
import           Data.Text (Text)
import qualified Data.Text as T

import qualified GI.Gtk as GI

import           Graphics.UI.Gtk (Widget)

import qualified System.Taffybar as T
import           System.Taffybar.Compat.GtkLibs (fromGIWidget)
import           System.Taffybar.Context (TaffyIO)
import qualified System.Taffybar.Information.Battery as IB
import qualified System.Taffybar.SimpleConfig as SC
import qualified System.Taffybar.Widget.Generic.ChannelWidget as CW
import qualified System.Taffybar.Widget.Layout as TL
import qualified System.Taffybar.Widget.MPRIS2 as TM
import qualified System.Taffybar.Widget.SimpleClock as TC
import qualified System.Taffybar.Widget.Systray as TS
import qualified System.Taffybar.Widget.Workspaces as TW

import           Text.Printf (printf)


main :: IO ()
main = do
  T.startTaffybar . SC.toTaffyConfig $ SC.defaultSimpleTaffyConfig {
      SC.barHeight = 30
    , SC.widgetSpacing = 30
    , SC.startWidgets = [
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
  TC.textClockNew Nothing ("%a %b %d %Y %H:%M") 60.0

music :: TaffyIO Widget
music =
  TM.mpris2New

-- -----------------------------------------------------------------------------

battery :: TaffyIO Widget
battery =
  fromGIWidget =<< do
    chan <- IB.getDisplayBatteryChan
    ctx <- ask
    let getBatteryInfoIO = runReaderT IB.getDisplayBatteryInfo ctx
    liftIO $ do
      label <- batteryFormat <$> getBatteryInfoIO >>= GI.labelNew . Just
      let setMarkup = postGUIASync . GI.labelSetMarkup label
          updateWidget = setMarkup . batteryFormat
      void $ GI.onWidgetRealize label (getBatteryInfoIO >>= updateWidget)
      GI.toWidget =<< CW.channelWidgetNew label chan updateWidget

batteryFormat :: IB.BatteryInfo -> Text
batteryFormat info =
  let
    battPctNum :: Int
    battPctNum =
      floor (IB.batteryPercentage info)

{--
    battState :: IB.BatteryState
    battState =
      IB.batteryState info

    battTime :: Maybe Int64
    battTime =
      case battState of
        IB.BatteryStateCharging ->
          Just (IB.batteryTimeToFull info)
        IB.BatteryStateDischarging ->
          Just (IB.batteryTimeToEmpty info)
--}
  in
    T.pack $ case battPctNum of
      pct | pct >= 95 -> iconBatteryFull
          | pct >= 66 -> iconBatteryThreeQuarter
          | pct >= 33 -> iconBatteryHalf
          | pct >= 10 -> iconBatteryQuarter
          | otherwise -> iconBatteryEmpty

-- -----------------------------------------------------------------------------

fontAwesome :: String -> String
fontAwesome = printf "<span font_desc='Font Awesome 5 Free' font_size='large'>%s</span>"

iconWeb :: String
iconWeb = fontAwesome "\xf268"

iconCode :: String
iconCode = fontAwesome "\xf121"

iconClock :: String
iconClock = fontAwesome "\xf017"

iconBatteryEmpty :: String
iconBatteryEmpty = fontAwesome "\xf244"

iconBatteryQuarter :: String
iconBatteryQuarter = fontAwesome "\xf243"

iconBatteryHalf :: String
iconBatteryHalf = fontAwesome "\xf242"

iconBatteryThreeQuarter :: String
iconBatteryThreeQuarter = fontAwesome "\xf241"

iconBatteryFull :: String
iconBatteryFull = fontAwesome "\xf240"
