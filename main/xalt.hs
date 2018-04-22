{-# LANGUAGE LambdaCase #-}

import           Control.Applicative ((<**>))

import           Data.Monoid ((<>))

import qualified Options.Applicative as O

import           System.Exit (ExitCode (..), exitWith)
import qualified System.IO as IO

import           XAlternative
import qualified XAlternative.Config as C

main :: IO ()
main = do
  cfgf <- optparse
  C.parseFile cfgf >>= \case
    Right cfg ->
      xAlternative cfg
    Left errs -> do
      IO.hPutStrLn IO.stderr (show errs)
      exitWith (ExitFailure 1)

optparse :: IO FilePath
optparse =
  O.execParser (O.info (opts <**> O.helper) (O.header "xalt window manager"))

opts :: O.Parser FilePath
opts =
  O.option O.str $
        O.long "config"
     <> O.short 'c'
     <> O.metavar "CONFIG_FILE"
     <> O.help "Path to config file"
