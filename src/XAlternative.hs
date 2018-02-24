{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module XAlternative where


import           Data.Bits ((.|.))
import           Data.Map.Strict (Map)

import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Types

import           XMonad (X, XConfig (..), Layout, KeyMask, KeySym)
import qualified XMonad as X
import           XMonad.Layout (Choose, Tall, Mirror, Full)

import           XMonad.Hooks.DynamicLog (xmobar)
import           XMonad.Util.CustomKeys (customKeys)


xAlternative :: IO ()
xAlternative =
  X.launch =<< xmobar xConfig

type Layouts = Choose Tall (Choose (Mirror Tall) Full)

xConfig :: XConfig Layouts
xConfig =
  X.def {
      terminal = "urxvtc"
    , modMask = mod4Mask
    , borderWidth = 3
    , keys = xKeys
    }

xKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
xKeys =
  customKeys (const []) $ \(XConfig {modMask = mm}) -> [
      ((mm, xF86XK_MonBrightnessDown), X.spawn "backlight down")
    , ((mm, xF86XK_MonBrightnessUp), X.spawn "backlight up")
    , ((mm .|. shiftMask, xK_r), X.restart "xalt" True)
    ]
