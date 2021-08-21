{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Keybinds where

import Applications (AppConfig (..), maybeSpawn)
import Data.Map (Map)
import qualified Data.Map as M
import Graphics.X11
import Graphics.X11.ExtraTypes (xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_AudioNext, xF86XK_AudioPause, xF86XK_AudioPrev, xF86XK_AudioRaiseVolume, xF86XK_AudioStop)
import System.Exit (exitSuccess)
import XMonad (ChangeLayout (NextLayout), Layout (Layout), MonadIO, Resize (Expand, Shrink), X, XConfig (XConfig, layoutHook, modMask), io, kill, refresh, sendMessage, setLayout, spawn, windows, withFocused, workspaces, (.|.))
import XMonad.Actions.FloatKeys (keysAbsResizeWindow)
import XMonad.Actions.RotSlaves (rotSlavesUp)
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Actions.WithAll (killAll)
import qualified XMonad.Core
import XMonad.Hooks.ManageDocks (Direction2D (D, R, U), ToggleStruts (ToggleStruts))
import XMonad.Layout.MultiToggle (Toggle (..), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.WindowNavigation (Direction2D (L), Navigate (Go))
import qualified XMonad.StackSet as W
import XMonad.Util.NamedActions (NamedAction, addName, subtitle, (^++^))

data KeybindConfig l = KeybindConfig
  { appConfig :: AppConfig,
    xConfig :: XConfig l
  }

keybinds :: KeybindConfig l -> Map (KeyMask, KeySym) (X ())
keybinds (KeybindConfig apps conf@XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart"),
      ((modm .|. shiftMask, xK_Return), maybeSpawn (terminal apps)),
      ((modm, xK_p), maybeSpawn (launcher apps)),
      ((modm, xK_Return), maybeSpawn (launcher apps)),
      ((modm, xK_space), sendMessage NextLayout),
      ((modm, xK_h), sendMessage $ Go L),
      ((modm, xK_j), sendMessage $ Go D),
      ((modm, xK_k), sendMessage $ Go U),
      ((modm, xK_l), sendMessage $ Go R),
      ((modm, xK_Return), windows W.swapMaster),
      ((modm, xK_f), sendMessage (Toggle NBFULL)),
      ((modm .|. shiftMask, xK_t), withFocused toggleFloat)
    ]
      <> switchWsById
  where
    switchWsById =
      [ ((m, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F6],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      where
        key n k a = (k, addName n a)
        action m = if m == shiftMask then "Move to " else "Switch to "

    toggleFloat w =
      windows \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s
