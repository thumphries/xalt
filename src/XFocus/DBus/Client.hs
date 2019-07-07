{-# LANGUAGE OverloadedStrings #-}
module XFocus.DBus.Client (
    status
  ) where


import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)

import qualified DBus as DBus
import qualified DBus.Client as DBus


status :: DBus.Client -> IO Text
status client = do
  e <-
    DBus.call client ((DBus.methodCall "/xfoc" "me.utf8.xfoc" "Status") {
        DBus.methodCallDestination = Just "me.utf8.xfoc"
      })
  pure $ case e of
    Left _err ->
      ""
    Right rsp ->
      fromMaybe "" $ do
        params <-
          traverse DBus.fromVariant (L.take 3 (DBus.methodReturnBody rsp)) :: Maybe [Text]
        pure $ case params of
          [name, status_, remaining] ->
            case status_ of
              "StatusComplete" ->
                ""
              _ ->
                name <> ": " <> remaining
          _ ->
            ""
