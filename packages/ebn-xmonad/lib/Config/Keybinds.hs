{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Config.Keybinds where

import qualified Config.Applications as Apps
import Config.Applications (App(..))
import Data.Map (Map)
import qualified Data.Map as M
import Graphics.X11
import System.Exit
import XMonad
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (killAll)
import XMonad.Layout.MultiToggle (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import qualified XMonad.StackSet as W

keybinds :: XConfig l -> Map (KeyMask, KeySym) (X ())
keybinds conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    [ ((modm, xK_q), restart'),
      ((modm .|. shiftMask, xK_q), io exitSuccess),
      ((modm .|. shiftMask, xK_Return), spawn $ Apps.appCommand terminal),
      ((modm, xK_c), kill),
      ((modm .|. shiftMask, xK_c), killAll),
      ((modm, xK_p), spawn launcher),
      ((modm, xK_o), spawn documents),
      ((modm, xK_6), runOrRaise' editor),
      ((modm, xK_7), runOrRaise' browser),
      ((modm, xK_8), runOrRaise' terminal),
      ((modm, xK_space), sendMessage NextLayout),
      ((modm, xK_n), windows W.focusUp),
      ((modm, xK_m), windows W.focusDown),
      ((modm, xK_Return), windows W.swapMaster),
      ((modm, xK_f), sendMessage (Toggle NBFULL)),
      ((modm .|. shiftMask, xK_t), withFocused toggleFloat)
    ]
      <> switchWsById
  where
    restart' = restart "xmonad" True
    terminal = ClassApp "kitty" "kitty"
    editor = ClassApp "Emacs" "emacs"
    browser = ClassApp "Brave-browser" "brave"
    launcher = "j4-dmenu-desktop"
    documents = "dmenu-files /home/ebn/Documents/books"
    
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

    runOrRaise' :: App -> X ()
    runOrRaise' app = runOrRaise (Apps.appCommand app) (Apps.isInstance app)
