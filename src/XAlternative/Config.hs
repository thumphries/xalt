{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module XAlternative.Config (
    Config (..)
  , General (..)
  , ConfigError (..)
  , parseConfig
  , parseFile
  ) where


import           Control.Applicative.Lift (Errors, runErrors, failure)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import qualified Control.Monad.Trans.Reader as RT

import           Data.Bifunctor (first)
import           Data.Text (Text)
import qualified Data.Text.IO as T

import qualified Config as CV
import qualified Config.Lens as CL

import           Lens.Micro ((^?))


data Config = Config {
    general :: General
  } deriving (Eq, Ord, Show)

data General = General {
    terminal :: Text
  , borderWidth :: Integer
  } deriving (Eq, Ord, Show)


-- -----------------------------------------------------------------------------

data ConfigError =
    FileParseError CV.ParseError
  | ValidationError [ValidationError]
  deriving (Show)

parseConfig :: Text -> Either ConfigError Config
parseConfig t = do
  v <- first FileParseError $ CV.parse t
  first ValidationError . runValidation $
    validateConfig v

parseFile :: FilePath -> IO (Either ConfigError Config)
parseFile fp =
  parseConfig <$> T.readFile fp

-- -----------------------------------------------------------------------------

validateConfig :: Value -> Validation Config
validateConfig v =
  Config <$> validateGeneral v

validateGeneral :: Value -> Validation General
validateGeneral v =
  section "general" v $ \s ->
    General
      <$> section "terminal" s text
      <*> section "border-width" s integer

-- -----------------------------------------------------------------------------

data ValidationError =
    MissingField CV.Position Text
  | ExpectedText CV.Position Value
  | ExpectedNumber CV.Position Value
  | ErrorWithContext [Text] ValidationError
  deriving (Show)

newtype Validation a = Validation {
    unValidation :: ReaderT [Text] (Errors [ValidationError]) a
  } deriving (Functor, Applicative)

type Value = CV.Value CV.Position

runValidation :: Validation a -> Either [ValidationError] a
runValidation f =
  runValidationCtx f []

runValidationCtx :: Validation a -> [Text] -> Either [ValidationError] a
runValidationCtx f ctx =
  runErrors $ runReaderT (unValidation f) ctx

bindValidation :: Validation a -> (a -> Validation b) -> Validation b
bindValidation f k =
  Validation . ReaderT $ \ctx ->
    case runValidationCtx f ctx of
      Right a ->
        runReaderT (unValidation (k a)) ctx
      Left es ->
        failure es

throwV :: ValidationError -> Validation a
throwV err =
  Validation . ReaderT $ \ctx ->
    failure . pure $ case ctx of
      [] -> err
      xs -> (ErrorWithContext xs err)

withContext :: Text -> Validation a -> Validation a
withContext v (Validation k) =
  Validation $ RT.local (v:) k

section :: Text -> Value -> (Value -> Validation a) -> Validation a
section name val k =
  bindValidation (key name val) $ \v ->
    withContext name (k v)

key :: Text -> Value -> Validation Value
key k val =
  noteV (MissingField (CV.valueAnn val) k) (val ^? CL.key k)

text :: Value -> Validation Text
text val =
  noteV (ExpectedText (CV.valueAnn val) val) (val ^? CL.text)

integer :: Value -> Validation Integer
integer val =
  noteV (ExpectedNumber (CV.valueAnn val) val) (val ^? CL.number)

noteV :: ValidationError -> Maybe a -> Validation a
noteV x =
  maybe (throwV x) pure
