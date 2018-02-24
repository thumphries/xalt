{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module XAlternative where


import           XMonad (XConfig (..))
import qualified XMonad
import           XMonad.Layout (Choose, Tall, Mirror, Full)
import           XMonad.Hooks.DynamicLog (xmobar)


xAlternative :: IO ()
xAlternative =
  XMonad.launch =<< xmobar xConfig

type Layout = Choose Tall (Choose (Mirror Tall) Full)

xConfig :: XConfig Layout
xConfig =
  XMonad.def {
      terminal = "urxvtc"
    , modMask = XMonad.mod4Mask
    , borderWidth = 3
    }
