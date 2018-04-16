{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module XAlternative where


import           Control.Exception (IOException, handle)
import           Control.Monad (liftM2)

import           Data.Bits ((.|.))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))
import qualified Data.Text as T

import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Types

import           System.Directory (removeFile)
import           System.IO (Handle, hPutStrLn)

import           XAlternative.Config (Config)
import qualified XAlternative.Config as C

import           XMonad (X, XConfig (..), Layout, KeyMask, KeySym)
import qualified XMonad as X
import           XMonad.Layout (Choose, Tall, Mirror, Full)

import           XMonad.Actions.DwmPromote (dwmpromote)
import           XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP)
import           XMonad.Hooks.ManageDocks (AvoidStruts, ToggleStruts (..))
import qualified XMonad.Hooks.ManageDocks as Docks
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import           XMonad.Prompt.XMonad (xmonadPrompt)
import           XMonad.Util.CustomKeys (customKeys)
import           XMonad.Util.Run (spawnPipe)


xAlternative :: Config -> IO ()
xAlternative cfg =
  X.launch =<< yabar (xConfig cfg)

type Layouts = Choose Tall (Choose (Mirror Tall) Full)

xConfig :: Config -> XConfig Layouts
xConfig (C.Config (C.General term bWidth)) =
  X.def {
      terminal = T.unpack term
    , modMask = mod4Mask
    , borderWidth = fromIntegral bWidth
    , keys = xKeys
    }

xKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
xKeys =
  customKeys (const []) $ \(XConfig {modMask = mm}) -> [
      ((mm, xF86XK_MonBrightnessDown), X.spawn "backlight down")
    , ((mm, xF86XK_MonBrightnessUp), X.spawn "backlight up")
    , ((mm .|. shiftMask, xK_r), X.restart "xalt" True)
    , ((mm, xK_Return), dwmpromote)
    , ((mm, xK_r), runOrRaisePrompt X.def)
    , ((mm, xK_x), xmonadPrompt X.def)
    ]

-- -----------------------------------------------------------------------------
-- Yabar

yabar :: XConfig Layouts -> IO (XConfig (ModifiedLayout AvoidStruts Layouts))
yabar cfg = do
  justDo_ (removeFile sockFile)
  pipe <- spawnPipe (socat sockFile)
  return $ Docks.docks cfg {
      logHook = dynamicLogWithPP (yabarPP pipe)
    , layoutHook = Docks.avoidStruts (layoutHook cfg)
    , keys = registerStrutsKey cfg
    }
  where
    registerStrutsKey cnfg =
      liftM2
        (<>)
        (\km -> (flip M.singleton (X.sendMessage ToggleStruts)) (toggleStrutsKey km))
        (keys cnfg)

yabarPP :: Handle -> PP
yabarPP h =
  X.def {
      ppCurrent = yabarColor "yellow" . wrap "[" "]"
    , ppTitle   = yabarColor "green"
    , ppVisible = wrap "(" ")"
    , ppUrgent  = yabarColor "red"
    , ppOutput  = hPutStrLn h
    }
  where
    wrap l r t = l <> t <> r
    yabarColor fg msg = mconcat [
        "<span foreground=\"" <> fg <> "\">"
      , msg
      , "</span>"
      ]

socat :: FilePath -> String
socat sockPath =
  "socat unix-listen:" <> sockPath <> ",fork,reuseaddr stdio"

sockFile :: FilePath
sockFile = "/tmp/xmonad.sock"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

justDo :: (IOException -> IO a) -> IO a -> IO a
justDo = handle

justDo_ :: IO () -> IO ()
justDo_ = justDo (const (return ()))
