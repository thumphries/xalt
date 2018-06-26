{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module XAlternative.Config (
    Config (..)
  , General (..)
  , KeyMap (..)
  , Command (..)
  , Rules (..)
  , Selector (..)
  , Action (..)
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
  , rules :: Rules
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
  | Promote
  deriving (Eq, Ord, Show)

newtype Rules = Rules {
    unRules :: Map Selector Action
  } deriving (Eq, Ord, Show, Monoid)

data Selector =
    Role Text
  | Name Text
  | Class Text
  deriving (Eq, Ord, Show)

data Action =
    Rect Rational Rational Rational Rational
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
    <*> validateRules v

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
  <||> (Promote <$ atomConst "promote" v)

validateRules :: Value -> Validation Rules
validateRules v =
  section "rules" v . list $ \rs ->
    fmap (Rules . M.fromList) . for rs $ \sa ->
      (,) <$> section "selector" sa validateSelector
          <*> section "action" sa validateAction

validateSelector :: Value -> Validation Selector
validateSelector v =
       (Role <$> section "role" v text)
  <||> (Name <$> section "name" v text)
  <||> (Class <$> section "class" v text)

validateAction :: Value -> Validation Action
validateAction v =
  section "rect" v $ \r ->
    Rect
      <$> section "x" r rational
      <*> section "y" r rational
      <*> section "w" r rational
      <*> section "h" r rational
