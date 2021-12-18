{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}
{-# OPTIONS_GHC -Wunused-imports #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Config where

import XMonad
import XMonad.Actions.MessageFeedback
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.MultiToggle (mkToggle, single)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.SimpleFloat (simpleFloat)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Util.Cursor (setDefaultCursor)
import Config.Applications (defaultAppConfig)
import Config.Keybinds (KeybindConfig (..), keybinds)
import Config.Polybar (polybar, polybarHook)
import qualified Config.Applications as App

main :: IO ()
main = xmonad . ewmh . docks . cfg =<< polybar
  where
    cfg dbus =
      def
        { manageHook = App.manageApps <+> manageSpawn,
          logHook = dynamicLogWithPP (polybarHook dbus),
          startupHook = myStartupHook,
          terminal = maybe "xterm" App.appCommand $ App.terminal apps,
          modMask = mod4Mask,
          borderWidth = 1,
          keys = keybinds . KeybindConfig apps,
          handleEventHook = handleEventHook def <+> fullscreenEventHook,
          layoutHook = myLayouts,
          focusedBorderColor = "#bd93f9",
          normalBorderColor = "#434C5E",
          workspaces = myWS
        }

    apps =
      defaultAppConfig
        { App.terminal = Just (App.ClassApp "kitty" "kitty"),
          App.launcher = Just
          (App.NameApp
            "rofi"
            "rofi -matching fuzzy -show drun -modi drun,run -show-icons"),
          App.mailClient = Just (App.ClassApp "Claws-mail" "claws-mail"),
          App.visualEditor = Just (App.ClassApp "Emacs" "emacs"),
          App.browser = Just (App.ClassApp "Brave-browser" "brave")
        }

    myStartupHook :: X ()
    myStartupHook = do
      setDefaultCursor xC_left_ptr

-- Workspaces ---------------------------------------------------------------
webWs = "I"

devWs = "II"

comWs = "III"

wrkWs = "IV"

sysWs = "V"

etcWs = "VI"

myWS :: [WorkspaceId]
myWS = [webWs, devWs, comWs, wrkWs, sysWs, etcWs]

myLayouts =
  avoidStruts
    . windowNavigation
    . smartBorders
    . fullScreenToggle
    . comLayout
    . devLayout
    . webLayout
    . etcLayout
    . wrkLayout
    $ (tiled ||| Mirror tiled ||| column3 ||| full)
  where
    tiled = gapSpaced $ Tall nmaster delta ratio
    full = gapSpaced Full
    column3 = gapSpaced $ ThreeColMid 1 (3 / 100) (1 / 2)
    float = simpleFloat
    mirrorTall = gapSpaced $ Mirror (Tall 1 (3 / 100) (1 / 2))
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100 -- Gaps bewteen windows
    gapSpaced = spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True
    -- Per workspace layout
    --
    comLayout = onWorkspace comWs (tiled ||| full) 

    devLayout = onWorkspace devWs (full ||| column3 ||| tiled ||| mirrorTall)
    webLayout = onWorkspace webWs (tiled ||| full)
    wrkLayout = onWorkspace wrkWs (tiled ||| full)
    etcLayout = onWorkspace etcWs float
    -- Fullscreen
    fullScreenToggle = mkToggle (single NBFULL)

tryResize :: ResizeDirectional -> Resize -> X ()
tryResize x y = sequence_ [tryMessageWithNoRefreshToCurrent x y, refresh]
