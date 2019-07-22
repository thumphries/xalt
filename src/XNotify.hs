-- | FreeDesktop.org Desktop Notifications.
--
-- Forked from fdo-notify.
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


import           Data.Bifunctor (second)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Int
import           Data.Word
import           Data.Char (isLower, toLower)

import           DBus
import           DBus.Client



-- |Contents of a notification
data Note = Note { appName :: String
                 , appImage :: Maybe Icon
                 , summary :: String
                 , body :: Maybe Body
                 , actions :: [(Action, String)]
                 , hints :: [Hint]
                 , expiry :: Timeout
                 }
    deriving (Eq, Show)

-- |A 'Note' with default values.
-- All fields are blank except for 'expiry', which is 'Dependent'.
blankNote :: Note
blankNote = Note { appName=""
                   , appImage=Nothing
                   , summary=""
                   , body=Nothing
                   , actions=[]
                   , hints=[]
                   , expiry=Dependent
                   }

-- |Message bodies may contain simple markup.
-- NotifyOSD doesn't support any markup.
data Body =   Text String
            | Bold Body
            | Italic Body
            | Underline Body
            | Hyperlink URL Body
            | Img URL String
            | Concat Body Body
    deriving (Eq, Show)

type URL = String

-- |Length of time to display notifications. NotifyOSD seems to ignore these.
data Timeout =   Never              -- ^Wait to be dismissed by user
               | Dependent          -- ^Let the notification service decide
               | Milliseconds Int32 -- ^Show notification for a fixed duration
                                    -- (must be positive)
    deriving (Eq, Show)

newtype Action = Action { actionName :: String }
    deriving (Eq, Show)

-- |Images are not yet supported
newtype Image = Image { bitmap :: String }
    deriving (Eq, Show)

-- |An Icon is either a path to an image, or a name in an icon theme
data Icon = File FilePath | Icon String
    deriving (Eq, Show)

iconString :: Icon -> String
iconString (File fp) = "file://" ++ fp
iconString (Icon name) = name

-- |Urgency of the notification. Notifications may be prioritised by urgency.
data UrgencyLevel =   Low
                    | Normal
                    | Critical -- ^Critical notifications require user attention
    deriving (Eq, Ord, Enum, Show)

-- |Various hints about how the notification should be displayed
data Hint =   Urgency UrgencyLevel
            | Category Category
            -- DesktopEntry ApplicationDesktopID
            | ImageData Image
            | ImagePath Icon
            | SoundFile FilePath
            | SuppressSound Bool
            | X Int32
            | Y Int32
    deriving (Eq, Show)

-- |Categorisation of (some) notifications
data Category =   Device | DeviceAdded | DeviceError | DeviceRemoved
                | Email | EmailArrived | EmailBounced
                | Im | ImError | ImReceived
                | Network | NetworkConnected | NetworkDisconnected | NetworkError
                | Presence | PresenceOffline | PresenceOnline
                | Transfer | TransferComplete | TransferError
    deriving (Eq, Show)

data ClosedReason = Expired | Dismissed | CloseNotificationCalled
data NotificationEvent = ActionInvoked Action | Closed ClosedReason

-- |A handle on a displayed notification
-- The notification may not have reached the screen yet, and may already have
-- been closed.
data Notification = Notification { notificationId :: Word32 }

-- |Display a notification.
-- Return a handle which can be used to replace the notification.
notify :: Client -> Note -> IO Notification
notify cl = replace cl (Notification { notificationId=0 })

callNotificationMethod :: Client -> MemberName -> [Variant] -> IO MethodReturn
callNotificationMethod client methodName_ args =
    call_ client $ (methodCall path iface methodName_)
        { methodCallDestination=Just busname
        , methodCallBody=args
        }
    where
        busname = "org.freedesktop.Notifications"
        path = "/org/freedesktop/Notifications"
        iface = "org.freedesktop.Notifications"

-- |Replace an existing notification.
-- If the notification has already been closed, a new one will be created.
replace :: Client -> Notification -> Note -> IO Notification
replace cl (Notification { notificationId=replaceId }) note =
    Notification . fromJust . fromVariant . head . methodReturnBody <$>
        callNotificationMethod cl "Notify" args
    where
        args = map ($ note)
            [ toVariant . appName
               , const $ toVariant (replaceId::Word32)
               , toVariant . fromMaybe "" . fmap iconString . appImage
               , toVariant . summary
               , toVariant . fromMaybe "" . fmap flattenBody . body
               , toVariant . actionsArray . actions
               , toVariant . hintsDict . hints
               , toVariant . timeoutInt . expiry
               ]

data Capability =   ActionsCap | BodyCap | BodyHyperlinksCap | BodyImagesCap
                  | BodyMarkupCap | IconMultiCap | IconStaticCap | SoundCap
                  | UnknownCap String
    deriving (Eq, Read, Show)

-- |Determine the server's capabilities
getCapabilities :: Client -> IO [Capability]
getCapabilities cl = map readCapability . fromJust
                    . fromVariant . head . methodReturnBody
                    <$> callNotificationMethod cl "GetCapabilities" []

readCapability :: String -> Capability
readCapability str = case str of
                    "actions" -> ActionsCap
                    "body" -> BodyCap
                    "body-hyperlinks" -> BodyHyperlinksCap
                    "body-images" -> BodyImagesCap
                    "body-markup" -> BodyMarkupCap
                    "icon-multi" -> IconMultiCap
                    "icon-static" -> IconStaticCap
                    "sound" -> SoundCap
                    s -> UnknownCap s

timeoutInt :: Timeout -> Int32
timeoutInt Never = 0
timeoutInt Dependent = -1
timeoutInt (Milliseconds n)
    | n > 0     = n
    | otherwise = error "notification timeout not positive"

flattenBody :: Body -> String
flattenBody (Text s) = concatMap escape s
    where
        escape '>' = "&gt;"
        escape '<' = "&lt;"
        escape '&' = "&amp;"
        escape x = [x]
flattenBody (Bold b) = "<b>" ++ flattenBody b ++ "</b>"
flattenBody (Italic b) = "<i>" ++ flattenBody b ++ "</i>"
flattenBody (Underline b) = "<u>" ++ flattenBody b ++ "</u>"
flattenBody (Hyperlink h b) = "<a href=\"" ++ h ++ "\">" ++ flattenBody b ++ "</a>"
flattenBody (Img h alt) = "<img src=\"" ++ h ++ "\" alt=\"" ++ alt ++ "\"/>"
flattenBody (Concat b1 b2) = flattenBody b1 ++ flattenBody b2

actionsArray :: [(Action, String)] -> [String]
actionsArray = concatMap pairList
    where
        pairList (a, b) = [actionName a, b]

hintsDict :: [Hint] -> M.Map String Variant
hintsDict = M.fromList . map hint
    where
        hint :: Hint -> (String, Variant)
        hint (Urgency u) = ("urgency", toVariant (fromIntegral $ fromEnum u :: Word8))
        hint (Category c) = ("category", toVariant $ catName c)
        hint (ImagePath p) = ("image-path", toVariant $ iconString p)
        hint (ImageData i) = ("image-data", toVariant $ bitmap i)
        hint (SoundFile s) = ("sound-file", toVariant s)
        hint (SuppressSound b) = ("suppress-sound", toVariant b)
        hint (X x) = ("x", toVariant x)
        hint (Y y) = ("x", toVariant y)

-- HACK: Assumes the constructor for category foo.bar is FooBar and
-- categories have no capital letters
catName :: Category -> String
catName cat = catName' (show cat)
    where
        catName' (c:cs) = map toLower $ c: (uncurry (++) . second ('.':) . span isLower $ cs)
        catName' [] = []
