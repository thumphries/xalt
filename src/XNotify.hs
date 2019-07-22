-- | FreeDesktop.org Desktop Notifications.
--
-- Forked from fdo-notify 2019-07-21.
--
-- Copyright (c) 2009, Max Rabkin
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# LANGUAGE OverloadedStrings #-}
module XNotify
    (
    -- * Usage
    -- $usage

    -- * Displaying notifications
      notify
    , replace
    , Notification
    , connectSession
    , Client
    -- * Constructing notifications
    , blankNote
    , Note (..)
    , Body (..)
    , URL
    , Timeout (..)
    , Action (..)
    , Image
    , Icon (..)
    , Category (..)
    , UrgencyLevel (..)
    , Hint (..)
    , ClosedReason (..)
    , NotificationEvent (..)
    -- * Capabilities
    , getCapabilities
    , Capability (..)
    ) where


import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Int (Int32)
import           Data.Word (Word32, Word8)

import           DBus
import           DBus.Client


-- |Contents of a notification
data Note =
  Note {
      appName :: String
    , appImage :: Maybe Icon
    , summary :: String
    , body :: Maybe Body
    , actions :: [(Action, String)]
    , hints :: [Hint]
    , expiry :: Timeout
    } deriving (Eq, Show)

-- |A 'Note' with default values.
-- All fields are blank except for 'expiry', which is 'Dependent'.
blankNote :: Note
blankNote =
  Note {
      appName = ""
    , appImage = Nothing
    , summary = ""
    , body = Nothing
    , actions = []
    , hints = []
    , expiry = Dependent
    }

-- |Message bodies may contain simple markup.
-- NotifyOSD doesn't support any markup.
data Body =
    Text String
  | Bold Body
  | Italic Body
  | Underline Body
  | Hyperlink URL Body
  | Img URL String
  | Concat Body Body
    deriving (Eq, Show)

type URL = String

-- | Length of time to display notifications. NotifyOSD seems to ignore these.
data Timeout =
    Never
  -- ^ Wait to be dismissed by user
  | Dependent
  -- ^ Let the notification service decide
  | Milliseconds Int32 {- FIXME Natural -}
  -- ^ Show notification for a fixed duration
  deriving (Eq, Show)

newtype Action =
  Action {
      actionName :: String
    } deriving (Eq, Show)

-- | Images are not yet supported
newtype Image =
  Image {
      bitmap :: String
    } deriving (Eq, Show)

-- | An Icon is either a path to an image, or a name in an icon theme
data Icon =
    File FilePath
  | Icon String
  deriving (Eq, Show)

iconString :: Icon -> String
iconString icon =
  case icon of
    File fp ->
      "file://" ++ fp
    Icon name ->
      name

-- | Urgency of the notification. Notifications may be prioritised by urgency.
data UrgencyLevel =
    Low
  | Normal
  | Critical
  deriving (Eq, Ord, Enum, Show)

-- | Various hints about how the notification should be displayed
data Hint =
    Urgency UrgencyLevel
  | Category Category
  -- DesktopEntry ApplicationDesktopID
  | ImageData Image
  | ImagePath Icon
  | SoundFile FilePath
  | SuppressSound Bool
  | X Int32
  | Y Int32
  deriving (Eq, Show)

-- | Categorisation of (some) notifications
data Category =
    Device
  | DeviceAdded
  | DeviceError
  | DeviceRemoved
  | Email
  | EmailArrived
  | EmailBounced
  | Im
  | ImError
  | ImReceived
  | Network
  | NetworkConnected
  | NetworkDisconnected
  | NetworkError
  | Presence
  | PresenceOffline
  | PresenceOnline
  | Transfer
  | TransferComplete
  | TransferError
  deriving (Eq, Show)

data ClosedReason =
    Expired
  | Dismissed
  | CloseNotificationCalled
  deriving (Eq, Show)

data NotificationEvent =
    ActionInvoked Action
  | Closed ClosedReason
  deriving (Eq, Show)

-- | A handle on a displayed notification.
--
-- The notification may not have reached the screen yet, and may already have
-- been closed.
data Notification =
  Notification {
      notificationId :: Word32
    } deriving (Eq, Show)

-- | Display a notification.
--
-- Returns a handle which can be used to replace the notification.
notify :: Client -> Note -> IO Notification
notify cl =
  replace cl (Notification { notificationId = 0 })

callNotificationMethod :: Client -> MemberName -> [Variant] -> IO MethodReturn
callNotificationMethod client methodName_ args =
  call_ client $
    (methodCall path iface methodName_) {
        methodCallDestination = Just busname
      , methodCallBody = args
      }
  where
    busname = "org.freedesktop.Notifications"
    path = "/org/freedesktop/Notifications"
    iface = "org.freedesktop.Notifications"

-- | Replace an existing notification.
--
-- If the notification has already been closed, a new one will be created.
replace :: Client -> Notification -> Note -> IO Notification
replace cl (Notification { notificationId=replaceId }) note =
  -- FIXME just brutal stuff
  Notification . fromJust . fromVariant . head . methodReturnBody <$>
      callNotificationMethod cl "Notify" args
  where
    args =
      map ($ note) [
          toVariant . appName
        , const $ toVariant (replaceId::Word32)
        , toVariant . fromMaybe "" . fmap iconString . appImage
        , toVariant . summary
        , toVariant . fromMaybe "" . fmap flattenBody . body
        , toVariant . actionsArray . actions
        , toVariant . hintsDict . hints
        , toVariant . timeoutInt . expiry
        ]

data Capability =
    ActionsCap
  | BodyCap
  | BodyHyperlinksCap
  | BodyImagesCap
  | BodyMarkupCap
  | IconMultiCap
  | IconStaticCap
  | SoundCap
  | UnknownCap String
  deriving (Eq, Read, Show)

-- | Determine the server's capabilities
getCapabilities :: Client -> IO [Capability]
getCapabilities cl =
  -- FIXME double double animal style
  map readCapability . fromJust . fromVariant . head . methodReturnBody
    <$> callNotificationMethod cl "GetCapabilities" []

readCapability :: String -> Capability
readCapability str =
  case str of
    "actions" ->
      ActionsCap
    "body" ->
      BodyCap
    "body-hyperlinks" ->
      BodyHyperlinksCap
    "body-images" ->
      BodyImagesCap
    "body-markup" ->
      BodyMarkupCap
    "icon-multi" ->
      IconMultiCap
    "icon-static" ->
      IconStaticCap
    "sound" ->
      SoundCap
    s ->
      UnknownCap s

timeoutInt :: Timeout -> Int32
timeoutInt to =
  case to of
    Never ->
      0
    Dependent ->
      -1
    Milliseconds n ->
      n

escape :: String -> String
escape str =
  concatMap escape_ str
  where
    escape_ '>' = "&gt;"
    escape_ '<' = "&lt;"
    escape_ '&' = "&amp;"
    escape_ x = [x]

flattenBody :: Body -> String
flattenBody bdy =
  case bdy of
    Text s ->
      escape s
    Bold b ->
      "<b>" ++ flattenBody b ++ "</b>"
    Italic b ->
      "<i>" ++ flattenBody b ++ "</i>"
    Underline b ->
      "<u>" ++ flattenBody b ++ "</u>"
    Hyperlink h b ->
      "<a href=\"" ++ h ++ "\">" ++ flattenBody b ++ "</a>"
    Img h alt ->
      "<img src=\"" ++ h ++ "\" alt=\"" ++ alt ++ "\"/>"
    Concat b1 b2 ->
      flattenBody b1 ++ flattenBody b2

actionsArray :: [(Action, String)] -> [String]
actionsArray =
  concatMap (\(a, b) -> [actionName a, b])

hintsDict :: [Hint] -> M.Map String Variant
hintsDict hints_ =
  M.fromList . flip fmap hints_ $ \hint ->
    case hint of
       Urgency u ->
         ("urgency", toVariant (fromIntegral $ fromEnum u :: Word8))
       Category c ->
         ("category", toVariant (catName c))
       ImagePath p ->
         ("image-path", toVariant (iconString p))
       ImageData i ->
         ("image-data", toVariant (bitmap i))
       SoundFile s ->
         ("sound-file", toVariant s)
       SuppressSound b ->
         ("suppress-sound", toVariant b)
       X x ->
         ("x", toVariant x)
       Y y ->
         ("x", toVariant y)

catName :: Category -> String
catName cat =
  case cat of
    Device ->
      "device"
    DeviceAdded ->
      "device.added"
    DeviceError ->
      "device.error"
    DeviceRemoved ->
      "device.removed"
    Email ->
      "email"
    EmailArrived ->
      "email.arrived"
    EmailBounced ->
      "email.bounced"
    Im ->
      "im"
    ImError ->
      "im.error"
    ImReceived ->
      "im.received"
    Network ->
      "network"
    NetworkConnected ->
      "network.connected"
    NetworkDisconnected ->
      "network.disconnected"
    NetworkError ->
      "network.error"
    Presence ->
      "presence"
    PresenceOffline ->
      "presence.offline"
    PresenceOnline ->
      "presence.online"
    Transfer ->
      "transfer"
    TransferComplete ->
      "transfer.complete"
    TransferError ->
      "transfer.error"
