{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Ebn.Keybinds where

import Ebn.Applications (AppConfig (..), maybeSpawn)
import Data.Map (Map)
import Graphics.X11
import XMonad (ChangeLayout (NextLayout), X, XConfig (XConfig, modMask), sendMessage, spawn, windows, withFocused, workspaces, (.|.), kill)
import XMonad.Hooks.ManageDocks (Direction2D (D, R, U))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.MultiToggle (Toggle (..))
import XMonad.Layout.WindowNavigation (Direction2D (L), Navigate (Go))
import XMonad.Util.NamedActions (addName)

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Actions.WithAll (killAll)

data KeybindConfig l = KeybindConfig
  { appConfig :: AppConfig,
    xConfig :: XConfig l
  }

keybinds :: KeybindConfig l -> Map (KeyMask, KeySym) (X ())
keybinds (KeybindConfig apps conf@XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart"),
      ((modm .|. shiftMask, xK_Return), maybeSpawn (terminal apps)),
      ((modm, xK_c), kill),
      ((modm .|. shiftMask, xK_c), killAll),
      ((modm, xK_p), maybeSpawn (launcher apps)),
      ((modm, xK_Return), maybeSpawn (launcher apps)),
      ((modm, xK_space), sendMessage NextLayout),
      ((modm, xK_h), sendMessage $ Go L),
      ((modm, xK_j), sendMessage $ Go D),
      ((modm, xK_k), sendMessage $ Go U),
      ((modm, xK_l), sendMessage $ Go R),
      ((modm, xK_n), windows W.focusUp),
      ((modm, xK_m), windows W.focusDown),
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
