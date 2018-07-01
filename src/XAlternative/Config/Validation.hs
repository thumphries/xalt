{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module XAlternative.Config.Validation (
    ConfigError (..)
  , validate
  , validateFile
  -- * Validation primitives
  , Validation
  , (<||>)
  , choice
  , Value
  , section
  , text
  , atom
  , atomConst
  , integer
  , float
  , rational
  , list
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


data ConfigError =
    FileParseError CV.ParseError
  | ValidationError [ValidationError]
  deriving (Show)

validate :: Text -> (Value -> Validation a) -> Either ConfigError a
validate t f = do
  v <- first FileParseError $ CV.parse t
  first ValidationError . runValidation $ f v

validateFile :: FilePath -> (Value -> Validation a) -> IO (Either ConfigError a)
validateFile fp k =
  flip validate k <$> T.readFile fp

-- -----------------------------------------------------------------------------

data ValidationError =
    MissingField CV.Position Text
  | ExpectedAtomConst CV.Position Text Text
  | ExpectedAtom CV.Position Value
  | ExpectedText CV.Position Value
  | ExpectedNumber CV.Position Value
  | ExpectedFloating CV.Position Value
  | ExpectedList CV.Position Value
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

choice :: Validation a -> Validation a -> Validation a
choice va vb =
  Validation . ReaderT $ \ctx ->
    case runValidationCtx va ctx of
      Right a ->
        pure a
      Left _es ->
        case runValidationCtx vb ctx of
          Right b ->
            pure b
          Left fs ->
            failure fs

(<||>) :: Validation a -> Validation a -> Validation a
(<||>) = choice

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

atom :: Value -> Validation Text
atom val =
  CV.atomName <$> noteV (ExpectedAtom (CV.valueAnn val) val) (val ^? CL.atom)

atomConst :: Text -> Value -> Validation Text
atomConst expect val =
  bindValidation (atom val) $ \name ->
    if name == expect
      then pure name
      else throwV (ExpectedAtomConst (CV.valueAnn val) expect name)


integer :: Value -> Validation Integer
integer val =
  noteV (ExpectedNumber (CV.valueAnn val) val) (val ^? CL.number)

float :: Value -> Validation Float
float val =
  case val of
    CV.Floating _ann coef expn ->
      pure (fromIntegral coef * 10.0 ^^ expn)
    _ ->
      throwV (ExpectedFloating (CV.valueAnn val) val)

rational :: Value -> Validation Rational
rational =
  fmap toRational . float

list :: ([Value] -> Validation a) -> Value -> Validation a
list k val =
  bindValidation
    (noteV (ExpectedList (CV.valueAnn val) val) (val ^? CL.list))
    k

noteV :: ValidationError -> Maybe a -> Validation a
noteV x =
  maybe (throwV x) pure
