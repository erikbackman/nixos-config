{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Config.Keybinds where

import Config.Applications (AppConfig (..), maybeSpawn)
import Data.Map (Map)
import Graphics.X11
import XMonad (ChangeLayout (NextLayout), X, XConfig (XConfig, modMask), sendMessage, spawn, windows, withFocused, workspaces, (.|.), kill)
import XMonad.Hooks.ManageDocks (Direction2D (D, R, U))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.MultiToggle (Toggle (..))
import XMonad.Layout.WindowNavigation (Direction2D (L), Navigate (Go))
import XMonad.Actions.WithAll (killAll)
import XMonad.Prompt.Window

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Prompt (def, bgColor, XPConfig (..), height, promptBorderWidth)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import System.Exit (exitSuccess)
import XMonad.Core (io)

data KeybindConfig l = KeybindConfig
  { appConfig :: AppConfig,
    xConfig :: XConfig l
  }

keybinds :: KeybindConfig l -> Map (KeyMask, KeySym) (X ())
keybinds (KeybindConfig apps conf@XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart"),
      ((modm .|. shiftMask, xK_q), io exitSuccess),
      ((modm .|. shiftMask, xK_Return), maybeSpawn (terminal apps)),
      ((modm, xK_c), kill),
      ((modm .|. shiftMask, xK_c), killAll),
      ((modm, xK_p), maybeSpawn (launcher apps)),
      ((modm, xK_o), windowPrompt xpConf Goto wsWindows),
      ((modm, xK_space), sendMessage NextLayout),
      ((modm, xK_h), sendMessage $ Go L),
      ((modm, xK_j), sendMessage $ Go D),
      ((modm, xK_k), sendMessage $ Go U),
      ((modm, xK_l), sendMessage $ Go R),
      ((modm, xK_n), windows W.focusUp),
      ((modm, xK_m), windows W.focusDown),
      ((modm, xK_Return), windows W.swapMaster),
      ((modm, xK_f), sendMessage (Toggle NBFULL)),
      ((modm .|. shiftMask, xK_t), withFocused toggleFloat),
      ((modm, xK_Print), spawn "flameshot full -p $HOME/Pictures/flameshot")
    ]
      <> switchWsById
  where
    switchWsById =
      [ ((m, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F6],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]

    toggleFloat w =
      windows \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s

    xpConf = def 
      { bgColor = "#0C0F12",
        font = "xft:JetBrains Mono:size=9",
        height = 20,
        promptBorderWidth = 0,
        searchPredicate = fuzzyMatch,
        maxComplRows = Just 4
      }
