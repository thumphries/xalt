module XAlternative.Config (
    Config (..)
  , ParseError (..)
  , parseConfig
  ) where


import           Data.Bifunctor (first)
import           Data.Text (Text)

import           Config ()
import qualified Config as CV
import qualified Config.Lens as CL

import           Lens.Family ((^.))


data Config = Config {
    terminal :: String
  } deriving (Eq, Ord, Show)


-- -----------------------------------------------------------------------------

data ParseError =
    FileParseError CV.ParseError
  deriving (Eq, Ord, Show)

parseConfig :: Text -> Either ParseError Config
parseConfig t = do
  v <- first FileParseError $ CV.parse t
  undefined
