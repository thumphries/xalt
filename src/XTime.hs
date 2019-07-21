-- | Synthesise Chronos and Clock into something coherent.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module XTime (
  -- * Time
    Time (..)
  , now

  -- * Durations
  , Duration (..)
  , renderDuration
  , delayDuration
  , durationDifference
  , minute

  -- * Clocks
  , Clock (..)
  , boot
  , clockDifference
  ) where


import qualified Chronos

import           Codec.Serialise (Serialise)

import           Control.Concurrent (threadDelay)

import           Data.Int (Int64)
import           Data.Text (Text)

import           GHC.Generics (Generic)

import qualified System.Clock as Clock


newtype Time =
  Time {
      unTime :: Int64
    } deriving (Eq, Ord, Show, Generic)
instance Serialise Time

now :: IO Time
now =
  fmap (Time . Chronos.getTime) Chronos.now

-- ---------------------------------------------------------------------------

newtype Duration =
  Duration {
      unDuration :: Int64
    } deriving (Eq, Ord, Show, Num, Generic)
instance Serialise Duration

renderDuration :: Duration -> Text
renderDuration =
  Chronos.encodeTimespan (Chronos.SubsecondPrecisionFixed 0)
    . Chronos.Timespan
    . unDuration

delayDuration :: Duration -> IO ()
delayDuration (Duration nanos) =
  threadDelay (fromIntegral (nanos `div` 1000))

durationDifference :: Duration -> Duration -> Duration
durationDifference = (-)

minute :: Duration
minute =
  Duration 60000000000

-- ---------------------------------------------------------------------------

newtype Clock =
  Clock {
      unClock :: Int64
    } deriving (Eq, Ord, Show, Generic)
instance Serialise Clock

boot :: IO Clock
boot =
  fmap unspec (Clock.getTime Clock.Boottime)

unspec :: Clock.TimeSpec -> Clock
unspec (Clock.TimeSpec secs nanos) =
  Clock (nanos + (secs * 1000000000))

clockDifference :: Clock -> Clock -> Duration
clockDifference c1 c2 =
  Duration $
    unClock c1 - unClock c2
