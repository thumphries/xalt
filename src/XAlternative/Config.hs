{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module XAlternative.Config (
    Config (..)
  , General (..)
  , KeyMap (..)
  , Command (..)
  , parseConfigFile
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Traversable (for)

import           XAlternative.Config.Validation


data Config = Config {
    general :: General
  , keyMap :: KeyMap
  } deriving (Eq, Ord, Show)

data General = General {
    terminal :: Text
  , borderWidth :: Integer
  } deriving (Eq, Ord, Show)

newtype KeyMap = KeyMap {
    unKeyMap :: Map Text Command
  } deriving (Eq, Ord, Show, Monoid)

data Command =
    Spawn Text
  | Restart
  deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------

parseConfigFile :: FilePath -> IO (Either ConfigError Config)
parseConfigFile fp =
  validateFile fp validateConfig

validateConfig :: Value -> Validation Config
validateConfig v =
  Config
    <$> validateGeneral v
    <*> validateKeyMap v

validateGeneral :: Value -> Validation General
validateGeneral v =
  section "general" v $ \s ->
    General
      <$> section "terminal" s text
      <*> section "border-width" s integer

validateKeyMap :: Value -> Validation KeyMap
validateKeyMap v =
  section "keymap" v . list $ \vs ->
    fmap (KeyMap . M.fromList) . for vs $ \kb ->
      (,) <$> section "keybind" kb text
          <*> section "command" kb validateCommand

validateCommand :: Value -> Validation Command
validateCommand v =
       (Spawn <$> section "spawn" v text)
  <||> (Restart <$ atomConst "restart" v)
