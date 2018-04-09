{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module XAlternative where


import           Data.Bits ((.|.))
import           Data.Map.Strict (Map)
import           Data.Semigroup ((<>))

import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Types

import           XMonad (X, XConfig (..), Layout, KeyMask, KeySym)
import qualified XMonad as X
import           XMonad.Layout (Choose, Tall, Mirror, Full)

import           XMonad.Actions.DwmPromote (dwmpromote)
import           XMonad.Hooks.DynamicLog (PP (..), statusBar)
import           XMonad.Hooks.ManageDocks (AvoidStruts)
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import           XMonad.Prompt.XMonad (xmonadPrompt)
import           XMonad.Util.CustomKeys (customKeys)


xAlternative :: IO ()
xAlternative =
  X.launch =<< yabar xConfig

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
    , ((mm, xK_Return), dwmpromote)
    , ((mm, xK_r), runOrRaisePrompt X.def)
    , ((mm, xK_x), xmonadPrompt X.def)
    ]

-- -----------------------------------------------------------------------------
-- Yabar

yabar :: XConfig Layouts -> IO (XConfig (ModifiedLayout AvoidStruts Layouts))
yabar = do
  statusBar socat yabarPP toggleStrutsKey

yabarPP :: PP
yabarPP =
  X.def {
      ppCurrent = yabarColor "yellow" . wrap "[" "]"
    , ppTitle   = yabarColor "green"
    , ppVisible = wrap "(" ")"
    , ppUrgent  = yabarColor "red"
    }
  where
    wrap l r t = l <> t <> r
    yabarColor fg msg = mconcat [
        "<span foreground=\"" <> fg <> "\">"
      , msg
      , "</span>"
      ]

socat :: String
socat =
  "socat unix-listen:/tmp/xmonad.sock,fork,reuseaddr stdio"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
