{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
module XAlternative where


import           Control.Monad (liftM2)

import           Data.Bifunctor (bimap)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import           Data.Ratio ((%))
import           Data.Text (Text)
import qualified Data.Text as T

import           Graphics.X11.Types

import qualified System.Taffybar.Support.PagerHints as TP

import           XAlternative.Config (Config)
import qualified XAlternative.Config as C

import           XMonad (X, XConfig (..), Layout, KeyMask, KeySym)
import qualified XMonad as X
import           XMonad.Layout ((|||), Choose, Tall (..), Full (..), Mirror (..))
import           XMonad.Layout.Grid (Grid (..))
import           XMonad.ManageHook ((=?), (-->))
import qualified XMonad.ManageHook as MH
import           XMonad.StackSet (RationalRect (..))

import           XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import           XMonad.Actions.DwmPromote (dwmpromote)
import qualified XMonad.Hooks.EwmhDesktops as EWMH
import           XMonad.Hooks.ManageDocks (AvoidStruts, ToggleStruts (..))
import qualified XMonad.Hooks.ManageDocks as Docks
import           XMonad.Layout.CenteredMaster (CenteredMaster, centerMaster)
import           XMonad.Layout.Decoration (Decoration, DefaultShrinker, shrinkText)
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.Reflect (Reflect, reflectHoriz)
import           XMonad.Layout.Renamed (Rename (..), renamed)
import           XMonad.Layout.Simplest (Simplest)
import           XMonad.Layout.Tabbed (TabbedDecoration, tabbedAlways)
import qualified XMonad.Layout.Tabbed as Tabbed
import           XMonad.Layout.ThreeColumns (ThreeCol (..))
import           XMonad.Util.CustomKeys (customKeys)
import qualified XMonad.Util.EZConfig as EZ
import qualified XMonad.Util.NamedScratchpad as SP


xAlternative :: Config -> IO ()
xAlternative cfg = do
  X.launch $ taffybar (xConfig cfg)

xConfig :: Config -> XConfig Layouts
xConfig cfg@(C.Config (C.General term bWidth nBorder fBorder) _keymap _rules) =
  X.def {
      terminal = T.unpack term
    , modMask = mod4Mask
    , borderWidth = fromIntegral bWidth
    , normalBorderColor = T.unpack nBorder
    , focusedBorderColor = T.unpack fBorder
    , keys = xKeys cfg
    , layoutHook = xLayoutHook
    , manageHook = xManageHook cfg
    , workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    }

xKeys :: Config -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
xKeys (C.Config (C.General term _b _n _f) keymap _rules) c =
  let
    ckeys =
      customKeys (const []) (\(XConfig {modMask = mm}) -> [
          ((mm, xK_grave), SP.namedScratchpadAction (scratchpads term) "terminal")
        ]) c
    ezkeys =
      EZ.mkKeymap c (fmap (bimap T.unpack xCmd) (M.toList (C.unKeyMap keymap)))
  in
    ezkeys <> ckeys

xCmd :: C.Command -> X ()
xCmd cmd =
  case cmd of
    C.Spawn x ->
      X.spawn (T.unpack x)
    C.Restart ->
      X.restart "xalt" True
    C.Promote ->
      dwmpromote
    C.Pin ->
      X.windows copyToAll
    C.Unpin ->
      killAllOtherCopies

-- -----------------------------------------------------------------------------
-- LayoutHook

type (|||) = Choose
infixr 5 |||



type Layouts =
      Split
  ||| TileLeft
  ||| TileRight
  ||| Lane
  ||| Tile
  ||| Pile
  ||| Tabbed
  ||| Full

type Renamed l =
  ModifiedLayout Rename l

type Split =
  Renamed (Mirror Tall)

type TileLeft =
  Renamed Tall

type TileRight =
  Renamed (ModifiedLayout Reflect TileLeft)

type Lane =
  Renamed ThreeCol

type Tile =
  Renamed Grid

type Pile =
  Renamed (ModifiedLayout CenteredMaster Grid)

type Tabbed =
  Renamed (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest)


xLayoutHook :: Layouts Window
xLayoutHook =
  let
    rename x = renamed [Replace x]

    splt = rename "Split" $ Mirror (Tall 2 (2 % 100) (4 % 5))
    sptl = rename "Left" $ Tall 1 (2 % 100) (7 % 10)
    sptr = rename "Right" $ reflectHoriz sptl
    lane = rename "Middle" $ ThreeColMid 1 (3 % 100) (1 % 2)
    tile = rename "Tile" Grid
    tabs = rename "Tabs" $ tabbedAlways shrinkText tabsTheme
    magn = rename "Stack" $ centerMaster Grid
    full = Full
  in
    splt ||| sptl ||| sptr ||| lane ||| tile ||| magn ||| tabs ||| full

tabsTheme :: Tabbed.Theme
tabsTheme =
  X.def {
      Tabbed.fontName = "xft:Source Sans Pro:pixelsize=22"
    }

-- -----------------------------------------------------------------------------
-- ManageHook

xManageHook :: Config -> X.ManageHook
xManageHook (C.Config (C.General term _bWidth _bNorm _bFoc) _keymap rules) =
  MH.composeAll [
      rulesHook rules
    , SP.namedScratchpadManageHook (scratchpads term)
    ]

rulesHook :: C.Rules -> X.ManageHook
rulesHook =
  MH.composeAll . fmap (uncurry rule) . M.toList . C.unRules

rule :: C.Selector -> C.Action -> X.ManageHook
rule sel act =
  selector sel --> action act

selector :: C.Selector -> X.Query Bool
selector sel =
  case sel of
    C.Role r ->
      role =? T.unpack r
    C.Name n ->
      MH.title =? T.unpack n
    C.Class c ->
      X.className =? T.unpack c

action :: C.Action -> X.ManageHook
action act =
  case act of
    C.Rect x y w h ->
      rect x y w h

role :: X.Query String
role = MH.stringProperty "WM_WINDOW_ROLE"

rect :: Rational -> Rational -> Rational -> Rational -> X.ManageHook
rect x y w h = SP.customFloating (RationalRect x y w h)

-- -----------------------------------------------------------------------------
-- Scratchpads

scratchpads :: Text -> [SP.NamedScratchpad]
scratchpads term = [
    SP.NS {
        SP.name = "terminal"
         -- FIX this role selector only works for xterm/termite
      , SP.cmd = T.unpack term <> " --role=scratchpad"
      , SP.query = role =? "scratchpad"
      , SP.hook = rect 0.1 0.1 0.8 0.33
      }
  ]

-- -----------------------------------------------------------------------------
-- Taffybar

taffybar ::
     XConfig Layouts
  -> XConfig (ModifiedLayout AvoidStruts Layouts)
taffybar cfg = do
  ewmh . TP.pagerHints $ Docks.docks cfg {
      layoutHook = Docks.avoidStruts (layoutHook cfg)
    , keys = liftM2 (<>) setStrutsKey (keys cfg)
    }

ewmh :: XConfig l -> XConfig l
ewmh cfg =
  cfg {
      startupHook = startupHook cfg <> EWMH.ewmhDesktopsStartup
    , handleEventHook = handleEventHook cfg <> EWMH.ewmhDesktopsEventHook
    , logHook = logHook cfg <> logHookNSP
    }
  where
    logHookNSP =
      EWMH.ewmhDesktopsLogHookCustom SP.namedScratchpadFilterOutWorkspace

setStrutsKey :: XConfig a -> Map (KeyMask, KeySym) (X ())
setStrutsKey =
  (`M.singleton` X.sendMessage ToggleStruts) . toggleStrutsKey

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
