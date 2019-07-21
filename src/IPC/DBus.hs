{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module IPC.DBus where


import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as S

import           Control.Concurrent (threadDelay)
import           Control.Monad (forever, when)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except

import           Data.ByteString.Lazy (ByteString)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T

import qualified DBus
import qualified DBus.Client as DBus

import           GHC.Generics (Generic)


newtype InterfaceName =
  InterfaceName {
      unInterfaceName :: Text
    } deriving (Eq, Ord, Show, Generic)

newtype ResourceName =
  ResourceName {
      unResourceName :: Text
    } deriving (Eq, Ord, Show, Generic)

newtype MethodName =
  MethodName {
      unMethodName :: Text
    } deriving (Eq, Ord, Show, Generic)

data Method a b =
  Method {
      methodName :: MethodName
    }

data BinaryCodec a =
  BinaryCodec {
      encoder :: a -> ByteString
    , decoder :: ByteString -> Maybe a
    }

mkCodec :: Serialise a => BinaryCodec a
mkCodec =
  BinaryCodec {
      encoder = S.serialise
    , decoder = Just . S.deserialise -- FIXME
    }

data ExportedMethod =
  forall a b.
    ExportedMethod {
        exportedMethod :: Method a b
      , requestCodec :: BinaryCodec a
      , responseCodec :: BinaryCodec b
      , handler :: a -> IO (Maybe b)
      }

exporting ::
     (Serialise a, Serialise b)
  => Method a b
  -> (a -> IO (Maybe b))
  -> ExportedMethod
exporting method k =
  ExportedMethod {
      exportedMethod = method
    , requestCodec = mkCodec
    , responseCodec = mkCodec
    , handler = k
    }

server ::
     DBus.Client
  -> InterfaceName
  -> ResourceName
  -> [ExportedMethod]
  -> IO ()
server client iface resource methods = do
  reserveName client iface
  export client iface resource methods
  forever (threadDelay 3000000)

reserveName :: DBus.Client -> InterfaceName -> IO ()
reserveName client iface = do
  requestResult <- DBus.requestName client (busName iface) []
  when (requestResult /= DBus.NamePrimaryOwner) $
    fail $
      "Another service owns the \""
        <> T.unpack (unInterfaceName iface)
        <> "\" bus name"


export ::
     DBus.Client
  -> InterfaceName
  -> ResourceName
  -> [ExportedMethod]
  -> IO ()
export client iface resource methods =
  DBus.export client (objectPath resource)
    DBus.defaultInterface {
        DBus.interfaceName = interfaceName iface
      , DBus.interfaceMethods = fmap packMethod methods
      }

packMethod :: ExportedMethod -> DBus.Method
packMethod method =
  case method of
    ExportedMethod (Method n) req rsp k ->
      DBus.autoMethod
        (memberName n)
        (\aa -> runExceptT $ do
           a <- maybe (throwE (DBus.ReplyError DBus.errorInvalidParameters [])) pure (decoder req aa)
           b <- ExceptT (fmap (maybe (Left (DBus.ReplyError DBus.errorFailed [])) (Right . encoder rsp)) (k a))
           pure b)

call ::
     (MonadIO m, Serialise a, Serialise b)
  => DBus.Client
  -> InterfaceName
  -> ResourceName
  -> Method a b
  -> a
  -> m b
call client iface resource method arg = do
  eret <-
    liftIO $
      DBus.call client
        ((DBus.methodCall
          (objectPath resource)
          (interfaceName iface)
          (memberName (methodName method))) {
              DBus.methodCallDestination = Just (busName iface)
            , DBus.methodCallBody = [DBus.toVariant (S.serialise arg)]
            })
  ret <- either (fail . show) pure eret -- FIXME
  case DBus.methodReturnBody ret of
    [fmap S.deserialise . DBus.fromVariant -> Just thing] ->
      pure thing
    _ ->
      fail "No payload" -- FIXME

objectPath :: ResourceName -> DBus.ObjectPath
objectPath =
  fromString . T.unpack . unResourceName

interfaceName :: InterfaceName -> DBus.InterfaceName
interfaceName =
  fromString . T.unpack . unInterfaceName

busName :: InterfaceName -> DBus.BusName
busName =
  fromString . T.unpack . unInterfaceName

memberName :: MethodName -> DBus.MemberName
memberName =
  fromString . T.unpack . unMethodName
