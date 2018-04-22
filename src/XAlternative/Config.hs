{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module XAlternative.Config (
    Config (..)
  , General (..)
  , parseConfigFile
  ) where


import           Data.Text (Text)

import           XAlternative.Config.Validation


data Config = Config {
    general :: General
  } deriving (Eq, Ord, Show)

data General = General {
    terminal :: Text
  , borderWidth :: Integer
  } deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------

parseConfigFile :: FilePath -> IO (Either ConfigError Config)
parseConfigFile fp =
  validateFile fp validateConfig

validateConfig :: Value -> Validation Config
validateConfig v =
  Config <$> validateGeneral v

validateGeneral :: Value -> Validation General
validateGeneral v =
  section "general" v $ \s ->
    General
      <$> section "terminal" s text
      <*> section "border-width" s integer
