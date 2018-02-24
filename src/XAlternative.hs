{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module XAlternative where


import qualified XMonad
import           XMonad.Hooks.DynamicLog (xmobar)

xAlternative :: IO ()
xAlternative =
  XMonad.launch =<< xmobar XMonad.def
