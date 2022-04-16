{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Config where

import qualified Config.Applications as App
import Config.Keybinds (keybinds)
import XMonad
import XMonad.Actions.MessageFeedback
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.MultiToggle (mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.Named (named)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleFloat (simpleFloat)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation (configurableNavigation, noNavigateBorders)
import XMonad.Util.Cursor (setDefaultCursor)

main :: IO ()
main = xmonad . ewmh . docks $ cfg
  where
    cfg =
      def
        { manageHook = App.manageApps <+> manageSpawn,
          startupHook = myStartupHook,
          modMask = mod4Mask,
          borderWidth = 2,
          keys = keybinds,
          handleEventHook = handleEventHook def <+> fullscreenEventHook,
          layoutHook = myLayouts,
          focusedBorderColor = "#81A1C1",
          normalBorderColor = "#000",
          workspaces = workspaceIds
        }

    myStartupHook :: X ()
    myStartupHook = do
      setDefaultCursor xC_left_ptr      

-- Workspaces ---------------------------------------------------------------

webWs = "web"

devWs = "dev"

comWs = "com"

wrkWs = "wrk"

etcWs = "etc"

workspaceIds :: [WorkspaceId]
workspaceIds = [webWs, devWs, comWs, wrkWs, etcWs]

myLayouts =
  avoidStruts
    . configurableNavigation noNavigateBorders 
--    . simpleDeco shrinkText (def { decoWidth = 100 })
    . gapSpaced
--    . smartBorders
    . fullScreenToggle
    . comLayout
    . devLayout
    . webLayout
    . etcLayout
    . wrkLayout
    $ (tiled ||| Mirror tiled ||| column3 ||| full)
  where
    tiled = named "Default"
      $ Tall nmaster ratio_increment ratio
      where ratio = 1 / 2
      
    full = named "Full" Full
    
    column3 = named "Three Col"
      $ ThreeColMid nmaster ratio_increment ratio
      where ratio = 1 / 2
      
    column2 = named "Master 2/3"
      $ reflectHoriz $ Tall nmaster ratio_increment ratio
      where ratio = 2 / 3

    nmaster = 1
    ratio_increment = 3 / 100

    gapSpaced = spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True
    fullScreenToggle = mkToggle (single NBFULL)

    comLayout = onWorkspace comWs (tiled ||| full)
    devLayout = onWorkspace devWs (column2 ||| full ||| column3)
    webLayout = onWorkspace webWs (tiled ||| full)
    wrkLayout = onWorkspace wrkWs (tiled ||| full)
    etcLayout = onWorkspace etcWs simpleFloat

tryResize :: ResizeDirectional -> Resize -> X ()
tryResize x y = sequence_ [tryMessageWithNoRefreshToCurrent x y, refresh]
