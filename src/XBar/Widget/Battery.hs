{-# LANGUAGE OverloadedStrings #-}
module XBar.Widget.Battery (
    widget
  ) where


import           Control.Monad (void)
import           Control.Monad.Reader (ask, runReaderT)
import           Control.Monad.IO.Class (liftIO)

import           Data.GI.Gtk.Threading (postGUIASync)
import           Data.Int (Int64)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           GI.Gtk (Widget)
import qualified GI.Gtk as GI

import           System.Taffybar.Context (TaffyIO)
import qualified System.Taffybar.Information.Battery as IB
import qualified System.Taffybar.Widget.Generic.ChannelWidget as CW

import           Text.Printf (printf)


widget :: TaffyIO Widget
widget = do
  chan <- IB.getDisplayBatteryChan
  ctx <- ask
  let getBatteryInfoIO = runReaderT IB.getDisplayBatteryInfo ctx
  liftIO $ do
    label <- fst . batteryFormat <$> getBatteryInfoIO >>= GI.labelNew . Just
    let setMarkup = postGUIASync . GI.labelSetMarkup label
        setTooltip = postGUIASync . GI.widgetSetTooltipMarkup label . Just
        updateWidget info = do
          let (w, t) = batteryFormat info
          setMarkup w
          setTooltip t
    void $ GI.onWidgetRealize label (getBatteryInfoIO >>= updateWidget)
    GI.toWidget =<< CW.channelWidgetNew label chan updateWidget

batteryFormat :: IB.BatteryInfo -> (Text, Text)
batteryFormat info =
  let
    battPctNum :: Int
    battPctNum =
      floor (IB.batteryPercentage info)

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
        IB.BatteryStateEmpty ->
          Just (IB.batteryTimeToEmpty info)
        IB.BatteryStateFullyCharged ->
          Just (IB.batteryTimeToEmpty info)
        _ ->
          Nothing

    state =
      case battState of
        IB.BatteryStateCharging ->
          "Charging"
        IB.BatteryStateDischarging ->
          "Discharging"
        IB.BatteryStateEmpty ->
          "Empty"
        IB.BatteryStateFullyCharged ->
          "Fully Charged"
        IB.BatteryStatePendingCharge ->
          "Pending Charge"
        IB.BatteryStatePendingDischarge ->
          "Pending Discharge"
        IB.BatteryStateUnknown ->
          "Unknown"

    widget_ =
      T.pack $ case battPctNum of
        pct | pct >= 95 -> iconBatteryFull
            | pct >= 66 -> iconBatteryThreeQuarter
            | pct >= 33 -> iconBatteryHalf
            | pct >= 10 -> iconBatteryQuarter
            | otherwise -> iconBatteryEmpty

    tooltip =
          T.pack (state <> " " <> show battPctNum <> "%") <> "\n"
       <> T.pack (formatDuration battTime <> " remaining")

  in
    (widget_, tooltip)

-- | Format a duration expressed as seconds to hours and minutes
formatDuration :: Maybe Int64 -> String
formatDuration Nothing = ""
formatDuration (Just secs) =
  let minutes = secs `div` 60
      hours = minutes `div` 60
      minutes' = minutes `mod` 60
  in printf "%02d:%02d" hours minutes'

-- -----------------------------------------------------------------------------

fontAwesome :: String -> String
fontAwesome = printf "<span font_desc='Font Awesome 5 Free' font_size='large'>%s</span>"

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
