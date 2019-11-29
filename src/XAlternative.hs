{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
module XAlternative where


import           Control.Monad (liftM2)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import           Data.Ratio ((%))
import           Data.Text (Text)
import qualified Data.Text as T

import           Graphics.X11.Types

import qualified System.Taffybar.Support.PagerHints as TP

import           Text.Read (readMaybe)

import           XAlternative.Config (Config)
import qualified XAlternative.Config as C

import           XMonad (X, XConfig, Layout, KeyMask, KeySym)
import qualified XMonad as X
import           XMonad.Layout ((|||), Choose, Tall (..), Full (..), Mirror (..))
import           XMonad.Layout.Grid (Grid (..))
import           XMonad.ManageHook ((=?), (-->))
import qualified XMonad.ManageHook as MH
import           XMonad.StackSet (RationalRect (..))
import qualified XMonad.StackSet as W

import           XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import           XMonad.Actions.DwmPromote (dwmpromote)
import qualified XMonad.Actions.FloatSnap as Snap
import qualified XMonad.Hooks.EwmhDesktops as EWMH
import           XMonad.Hooks.ManageDocks (AvoidStruts, ToggleStruts (..))
import qualified XMonad.Hooks.ManageDocks as Docks
import           XMonad.Layout.BinarySpacePartition (BinarySpacePartition)
import qualified XMonad.Layout.BinarySpacePartition as BSP
import           XMonad.Layout.CenteredMaster (CenteredMaster, centerMaster)
import           XMonad.Layout.Decoration (Decoration, DefaultShrinker, shrinkText)
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.NoBorders (Ambiguity (..), ConfigurableBorder, lessBorders)
import           XMonad.Layout.Reflect (Reflect, reflectHoriz)
import           XMonad.Layout.Renamed (Rename (..), renamed)
import           XMonad.Layout.Simplest (Simplest)
import           XMonad.Layout.Spacing (Spacing, Border (..), spacingRaw)
import           XMonad.Layout.Tabbed (TabbedDecoration, tabbedAlways)
import qualified XMonad.Layout.Tabbed as Tabbed
import           XMonad.Layout.ThreeColumns (ThreeCol (..))
import           XMonad.Util.CustomKeys (customKeys)
import qualified XMonad.Util.EZConfig as EZ
import qualified XMonad.Util.NamedScratchpad as SP
import           XMonad.Util.Run (runProcessWithInput)


xAlternative :: Config -> IO ()
xAlternative cfg = do
  X.launch $ taffybar (xConfig cfg)

xConfig :: Config -> XConfig Layouts
xConfig cfg@(C.Config g@(C.General term bWidth nBorder fBorder _gaps) _keymap _rules _pads) =
  X.def {
      X.terminal = T.unpack term
    , X.modMask = mod4Mask
    , X.borderWidth = fromIntegral bWidth
    , X.normalBorderColor = T.unpack nBorder
    , X.focusedBorderColor = T.unpack fBorder
    , X.keys = xKeys cfg
    , X.mouseBindings = xMouseBindings
    , X.layoutHook = xLayoutHook g
    , X.manageHook = xManageHook cfg
    , X.workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    }

thing :: [C.Scratchpad] -> C.Keymap -> X ()
thing pads keymap = do
  let
    keyss :: [C.Keybind]
    keyss =
      C.unKeymap keymap

    render :: C.Keybind -> Text
    render (C.Keybind keys cmd mdesc) =
      T.unwords [
          renderDesc cmd mdesc
        , renderKeys keys
        ]

    renderDesc :: C.Command -> Maybe Text -> Text
    renderDesc cmd mdesc =
      case mdesc of
        Just desc ->
          escapeEntities desc
        Nothing ->
          escapeEntities (renderCommand cmd)

    renderKeys :: Text -> Text
    renderKeys keys =
     "(<b>" <> escapeEntities keys <> "</b>)"

    renderCommand :: C.Command -> Text
    renderCommand = \case
      C.Spawn x ->
        "Spawn " <> x
      C.Restart ->
        "Restart"
      C.Promote ->
        "Promote"
      C.Pin ->
        "Pin"
      C.Unpin ->
        "Unpin"
      C.Magnify ->
        "Magnify"
      C.Fullscreen ->
        "Fullscreen"
      C.Float ->
        "Float"
      C.Sink ->
        "Sink"
      C.Scratch x ->
        "Scratchpad " <> x

    escapeEntities :: Text -> Text
    escapeEntities =
        T.replace "<" "&lt;"
      . T.replace ">" "&gt;"
      . T.replace "\"" "&quot;"
      . T.replace "'" "&#39;"
      . T.replace "&" "&amp;"

    input :: [Text]
    input =
      fmap render keyss

  choice <-
    runProcessWithInput
      "/usr/bin/rofi" ["-dmenu", "-format", "i", "-markup-rows"]
      (T.unpack (T.unlines input))

  case readMaybe choice of
    Just i ->
      xCmd pads . C.kbCommand $ keyss !! i
    Nothing ->
      pure ()

xKeys :: Config -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
xKeys (C.Config (C.General _term _b _n _f _g) keymap _rules pads) c =
  let
    move d =
      X.withFocused (Snap.snapMove d Nothing)

    ckeys =
      customKeys (const []) (\(X.XConfig {X.modMask = mm}) -> [
        -- TODO fold into Command
          ((mm, xK_s), X.withFocused (Snap.snapMagicResize [Snap.L, Snap.R, Snap.U, Snap.D] Nothing Nothing))
        , ((mm, xK_Left), move Snap.L)
        , ((mm, xK_Right), move Snap.R)
        , ((mm, xK_Up), move Snap.U)
        , ((mm, xK_Down), move Snap.D)

        , ((mm, xK_r), thing pads keymap)

        -- TODO unsure if BSP goes into Command
        -- , ((mm, xK_r), X.sendMessage BSP.Rotate)
        -- , ((mm, xK_t), X.sendMessage BSP.Swap)
        ]) c

    toez (C.Keybind k cmd _d) =
      (T.unpack k, xCmd pads cmd)

    ezkeys =
      EZ.mkKeymap c (fmap toez (C.unKeymap keymap))
  in
    ezkeys <> ckeys

xMouseBindings :: XConfig Layout -> Map (ButtonMask, Button) (Window -> X ())
xMouseBindings cfg =
  let
    snapDistance =
      Just 100

    fillDistance =
      Just 250

    mouseMove w = do
      X.focus w
      X.mouseMoveWindow w
      Snap.afterDrag $
        Snap.snapMagicMove snapDistance snapDistance w

    mouseMoveExpand w = do
      X.focus w
      X.mouseMoveWindow w
      Snap.afterDrag $
        Snap.snapMagicResize
          [Snap.L, Snap.R, Snap.U, Snap.D]
          fillDistance
          fillDistance
          w

    resizeSnap w = do
      X.focus w
      X.mouseResizeWindow w
      Snap.afterDrag $
        Snap.snapMagicResize
          [Snap.R, Snap.D]
          snapDistance
          snapDistance
          w

    custom (X.XConfig {X.modMask = mm}) =
      M.fromList [
          ((mm, button1), mouseMove)
        , ((mm X..|. shiftMask, button1), mouseMoveExpand)
        , ((mm, button3), resizeSnap)
        ]
  in
    custom cfg <> X.mouseBindings X.def cfg

xCmd :: [C.Scratchpad] -> C.Command -> X ()
xCmd pads cmd =
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
    C.Magnify ->
      X.withFocused $ \w -> do
        X.windows $
          W.float w (RationalRect 0.1 0.1 0.8 0.8)
    C.Fullscreen ->
      X.withFocused $ \w -> do
        X.windows $
          W.float w (RationalRect 0 0 1 1)
    C.Float ->
      X.withFocused $ \w ->
        X.float w
    C.Sink ->
      X.withFocused $ \w ->
        X.windows $
          W.sink w
    C.Scratch x ->
      SP.namedScratchpadAction (scratchpads pads) (T.unpack x)

-- -----------------------------------------------------------------------------
-- LayoutHook

type (|||) = Choose
infixr 5 |||

type Layouts =
  ModifiedLayout (ConfigurableBorder Ambiguity) Layouts'

type Layouts' =
      BSP
  ||| Split
  ||| TileLeft
  ||| TileRight
  ||| Middle
  ||| Tile
  ||| Pile
  ||| Tabbed
  ||| Full

type Renamed =
  ModifiedLayout Rename

type Gaps =
  ModifiedLayout Spacing

type BSP =
  Renamed (Gaps BinarySpacePartition)

type Split =
  Renamed (Gaps (Mirror Tall))

type TileLeft =
  Renamed (Gaps Tall)

type TileRight =
  Renamed (ModifiedLayout Reflect TileLeft)

type Middle =
  Renamed (Gaps ThreeCol)

type Tile =
  Renamed (Gaps Grid)

type Pile =
  Renamed (Gaps (ModifiedLayout CenteredMaster Grid))

type Tabbed =
  Renamed (Gaps (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest))


xLayoutHook :: C.General -> Layouts Window
xLayoutHook cfgG =
  let
    g = C.gaps cfgG
    rename x = renamed [Replace x]
    screenBorder = Border g g g g
    windowBorder = Border g g g g
    gaps = spacingRaw False screenBorder True windowBorder True

    bsp  = rename "BSP" . gaps $ BSP.emptyBSP
    splt = rename "Split" . gaps $ Mirror (Tall 2 (2 % 100) (4 % 5))
    sptl = rename "Left" . gaps $ Tall 1 (2 % 100) (7 % 10)
    sptr = rename "Right" $ reflectHoriz sptl
    midd = rename "Middle" . gaps $ ThreeColMid 1 (3 % 100) (1 % 2)
    tile = rename "Tile" . gaps $ Grid
    tabs = rename "Tabs" . gaps $ tabbedAlways shrinkText tabsTheme
    magn = rename "Stack" . gaps $ centerMaster Grid
    full = Full
  in
    lessBorders OnlyScreenFloat $
      bsp ||| splt ||| sptl ||| sptr ||| midd ||| tile ||| magn ||| tabs ||| full

tabsTheme :: Tabbed.Theme
tabsTheme =
  X.def {
      Tabbed.fontName = "xft:Source Sans Pro:pixelsize=22"
    }

-- -----------------------------------------------------------------------------
-- ManageHook

xManageHook :: Config -> X.ManageHook
xManageHook (C.Config _gen _keymap rules pads) =
  MH.composeAll [
      rulesHook rules
    , SP.namedScratchpadManageHook (scratchpads pads)
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
    C.Tile ->
      X.idHook

role :: X.Query String
role = MH.stringProperty "WM_WINDOW_ROLE"

rect :: Rational -> Rational -> Rational -> Rational -> X.ManageHook
rect x y w h = SP.customFloating (RationalRect x y w h)

-- -----------------------------------------------------------------------------
-- Scratchpads

scratchpads :: [C.Scratchpad] -> [SP.NamedScratchpad]
scratchpads =
  fmap scratchpad

scratchpad :: C.Scratchpad -> SP.NamedScratchpad
scratchpad (C.Scratchpad name cmd select act) =
  SP.NS {
      SP.name = T.unpack name
    , SP.cmd = T.unpack cmd
    , SP.query = selector select
    , SP.hook = action act
    }

-- -----------------------------------------------------------------------------
-- Taffybar

taffybar ::
     XConfig Layouts
  -> XConfig (ModifiedLayout AvoidStruts Layouts)
taffybar cfg = do
  ewmh . TP.pagerHints $ Docks.docks cfg {
      X.layoutHook = Docks.avoidStruts (X.layoutHook cfg)
    , X.keys = liftM2 (<>) setStrutsKey (X.keys cfg)
    }

ewmh :: XConfig l -> XConfig l
ewmh cfg =
  cfg {
      X.startupHook = X.startupHook cfg <> EWMH.ewmhDesktopsStartup
    , X.handleEventHook = X.handleEventHook cfg <> EWMH.ewmhDesktopsEventHook
    , X.logHook = X.logHook cfg <> logHookNSP
    }
  where
    logHookNSP =
      EWMH.ewmhDesktopsLogHookCustom SP.namedScratchpadFilterOutWorkspace

setStrutsKey :: XConfig a -> Map (KeyMask, KeySym) (X ())
setStrutsKey =
  (`M.singleton` X.sendMessage ToggleStruts) . toggleStrutsKey

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey X.XConfig{X.modMask = modm} = (modm, xK_b )
