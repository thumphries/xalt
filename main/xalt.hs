
import           Control.Applicative ((<**>))

import           Data.Monoid ((<>))

import qualified Options.Applicative as O

import           System.Exit (ExitCode (..), exitWith)
import qualified System.IO as IO

import           XAlternative
import qualified XAlternative.Config as C

main :: IO ()
main = do
  opt <- optparse
  ecfg <- C.parseConfigFile (configFile opt)
  case (ecfg, runType opt) of
    (Left errs, _) -> do
      logg (show errs)
      die

    (Right cfg, DryRun) -> do
      logg (show cfg)
      pure ()

    (Right cfg, RealRun) ->
      xAlternative cfg

logg :: String -> IO ()
logg =
  IO.hPutStrLn IO.stderr

die :: IO ()
die =
  exitWith (ExitFailure 1)

optparse :: IO Opts
optparse =
  O.execParser (O.info (opts <**> O.helper) (O.header "xalt window manager"))

data RunType = RealRun | DryRun

data Opts =
  Opts {
      configFile :: FilePath
    , runType :: RunType
    }

opts :: O.Parser Opts
opts =
  Opts
    <$> (O.option O.str $
             O.long "config"
          <> O.short 'c'
          <> O.metavar "CONFIG_FILE"
          <> O.help "Path to config file")
    <*> (O.flag RealRun DryRun $
             O.long "dry-run"
          <> O.help "Validate config files without running.")
