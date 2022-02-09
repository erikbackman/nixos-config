{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Config where

import Config.Applications (defaultAppConfig)
import qualified Config.Applications as App
import Config.Keybinds (KeybindConfig (..), keybinds)
import Config.Polybar (polybar, polybarHook)
import XMonad
import XMonad.Actions.MessageFeedback
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.MultiToggle (mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleFloat (simpleFloat)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Util.Cursor (setDefaultCursor)

main :: IO ()
main = xmonad . ewmh . docks . cfg =<< polybar
  where
    cfg dbus =
      def
        { manageHook = App.manageApps apps <+> manageSpawn,
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
          workspaces = workspaceIds
        }

    apps =
      defaultAppConfig
        { App.terminal = Just $ App.ClassApp "kitty" "kitty",
          App.launcher =
            Just $
              App.NameApp
                "rofi"
                "rofi -matching fuzzy -show drun -modi drun,run -show-icons -theme dmenu",
          App.visualEditor = Just $ App.ClassApp "Emacs" "emacs",
          App.browser = Just $ App.ClassApp "Brave-browser" "brave"
        }

    myStartupHook :: X ()
    myStartupHook = do
      setDefaultCursor xC_left_ptr

-- Workspaces ---------------------------------------------------------------

webWs = "web"

devWs = "dev"

comWs = "com"

wrkWs = "wrk"

sysWs = "sys"

etcWs = "etc"

workspaceIds :: [WorkspaceId]
workspaceIds = [webWs, devWs, comWs, wrkWs, sysWs, etcWs]

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
    tiled   = gapSpaced $ Tall nmaster ratio_increment ratio where ratio = 1 / 2
    full    = gapSpaced Full
    column3 = gapSpaced $ ThreeColMid nmaster ratio_increment ratio where ratio = 1 / 2
    column2 = gapSpaced $ reflectHoriz $ Tall nmaster ratio_increment ratio where ratio = 2 / 3
        
    nmaster         = 1
    ratio_increment = 3 / 100
    
    gapSpaced = spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True
    fullScreenToggle = mkToggle (single NBFULL)

    comLayout = onWorkspace comWs (tiled ||| full)
    devLayout = onWorkspace devWs (full ||| column3 ||| tiled ||| column2)
    webLayout = onWorkspace webWs (tiled ||| full)
    wrkLayout = onWorkspace wrkWs (tiled ||| full)
    etcLayout = onWorkspace etcWs simpleFloat
    
tryResize :: ResizeDirectional -> Resize -> X ()
tryResize x y = sequence_ [tryMessageWithNoRefreshToCurrent x y, refresh]
