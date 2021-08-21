{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}
{-# OPTIONS_GHC -Wunused-imports #-}

import Applications (AppConfig, defaultAppConfig)
import qualified Applications as App
import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad (replicateM_)
import qualified DBus as D
import qualified DBus.Client as D
import Data.Maybe (fromMaybe)
import Keybinds (KeybindConfig (..), keybinds)
import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Actions.MessageFeedback
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.InsertPosition
  ( Focus (Newer),
    Position (Below),
    insertPosition,
  )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
  ( composeOne,
    doCenterFloat,
    doFullFloat,
    isDialog,
    isFullscreen,
    isInProperty,
    (-?>),
  )
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.MultiToggle (mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.SimpleFloat (simpleFloat)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.NamedScratchpad

main :: IO ()
main = xmonad . dynamicProjects (projects apps) . ewmh . docks . cfg =<< mkDbusClient
  where
    cfg dbus =
      def
        { manageHook = myManageHook,
          logHook = dynamicLogWithPP (polybarHook dbus),
          startupHook = myStartupHook,
          terminal = fromMaybe "xterm" $ App.terminal apps,
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
        { App.terminal = Just "kitty",
          App.launcher = Just "rofi -matching fuzzy -show drun -modi drun,run -show-icons"
        }

mkDbusClient :: IO D.Client
mkDbusClient = D.connectSession >>= \dbus -> requestBus dbus >> pure dbus
  where
    requestBus dbus = D.requestName dbus (D.busName_ "org.xmonad.log") opts
    opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath = D.objectPath_ "/org/xmonad/Log"
      iname = D.interfaceName_ "org.xmonad.Log"
      mname = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body = [D.toVariant $ UTF8.decodeString str]
   in D.emit dbus $ signal {D.signalBody = body}

polybarHook :: D.Client -> PP
polybarHook dbus =
  namedScratchpadFilterOutWorkspacePP $
    def
      { ppOutput = dbusOutput dbus,
        ppCurrent = wrapper focusedFg,
        ppVisible = wrapper fg,
        ppUrgent = wrapper red,
        ppHidden = wrapper gray,
        ppWsSep = "",
        ppSep = " :: ",
        ppTitle = const ""
      }
  where
    wrapper c s = wrap ("%{F" <> c <> "} ") " %{F-}" s
    blue = "#2E9AFE"
    gray = "#7F7F7F"
    orange = "#ea4300"
    purple = "#9058c7"
    focusedFg = "#bd93f9"
    fg = "#ebdbb2"
    bg = "#282828"
    bg1 = "#3c3836"
    red = "#fb4934"

myManageHook :: ManageHook
myManageHook = manageApps <+> manageSpawn
  where
    isBrowserDialog = isDialog <&&> className =? "chromium-browser"

    isFileChooserDialog = isRole =? "GtkFileChooserDialog"

    isPopup = isRole =? "pop-up"

    isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

    isRole = stringProperty "WM_WINDOW_ROLE"

    tileBelow = insertPosition Below Newer

    anyOf :: [Query Bool] -> Query Bool
    anyOf = foldl (<||>) (pure False)

    match :: [App] -> Query Bool
    match = anyOf . fmap isInstance

    manageApps :: ManageHook
    manageApps =
      composeOne
        [ resource =? "desktop_window" -?> doIgnore,
          resource =? "kdesktop" -?> doIgnore,
          anyOf
            [ isBrowserDialog,
              isFileChooserDialog,
              isDialog,
              isPopup,
              isSplash
            ]
            -?> doCenterFloat,
          isFullscreen -?> doFullFloat,
          pure True -?> tileBelow
        ]

isInstance :: App -> Query Bool
isInstance (ClassApp c _) = className =? c
isInstance (TitleApp t _) = title =? t
isInstance (NameApp n _) = appName =? n

-- Workspaces ---------------------------------------------------------------
webWs = "web"

devWs = "dev"

comWs = "com"

wrkWs = "wrk"

sysWs = "sys"

etcWs = "etc"

myWS :: [WorkspaceId]
myWS = [webWs, devWs, comWs, wrkWs, sysWs, etcWs]

-- Dynamic Projects ----------------------------------------------------------
projects :: AppConfig -> [Project]
projects apps =
  [ Project
      { projectName = webWs,
        projectDirectory = "~/",
        projectStartHook = Just $ spawn "chromium-browser"
      },
    Project
      { projectName = devWs,
        projectDirectory = "~/repos/github.com/erikbackman",
        projectStartHook = Just $ replicateM_ 3 (App.maybeSpawn $ App.terminal apps)
      },
    Project
      { projectName = comWs,
        projectDirectory = "~/",
        projectStartHook =
          Just $
            App.maybeSpawn (App.mailClient apps)
              >> spawn "discord"
      },
    Project
      { projectName = wrkWs,
        projectDirectory = "~/",
        projectStartHook = Nothing
      },
    Project
      { projectName = sysWs,
        projectDirectory = "~/repos/github.com/erikbackman/nixos-config",
        projectStartHook = Just $ App.maybeSpawn (App.terminal apps)
      },
    Project
      { projectName = etcWs,
        projectDirectory = "~/",
        projectStartHook = Nothing
      }
  ]

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
    gapSpaced = spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
    -- Per workspace layout
    comLayout = onWorkspace comWs (tiled ||| full)
    devLayout = onWorkspace devWs (full ||| column3 ||| tiled ||| mirrorTall)
    webLayout = onWorkspace webWs (tiled ||| full)
    wrkLayout = onWorkspace wrkWs (tiled ||| full)
    etcLayout = onWorkspace etcWs float
    -- Fullscreen
    fullScreenToggle = mkToggle (single NBFULL)

myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr

tryResize :: ResizeDirectional -> Resize -> X ()
tryResize x y = sequence_ [tryMessageWithNoRefreshToCurrent x y, refresh]

type AppName = String

type AppTitle = String

type AppClassName = String

type AppCommand = String

data App
  = ClassApp AppClassName AppCommand
  | TitleApp AppTitle AppCommand
  | NameApp AppName AppCommand
  deriving (Show)
