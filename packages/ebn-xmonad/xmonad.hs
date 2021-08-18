{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Control.Exception as E
import qualified DBus as D
import qualified DBus.Client as D
import qualified Data.Map as M
import Graphics.X11.ExtraTypes
  ( xF86XK_AudioLowerVolume,
    xF86XK_AudioMute,
    xF86XK_AudioNext,
    xF86XK_AudioPause,
    xF86XK_AudioPrev,
    xF86XK_AudioStop,
  )
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioRaiseVolume)
import System.Exit (exitSuccess)
import System.IO (hClose)
import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces (removeWorkspace)
import XMonad.Actions.FloatKeys (keysAbsResizeWindow, keysResizeWindow)
import XMonad.Actions.MessageFeedback
import XMonad.Actions.Navigation2D
import XMonad.Actions.RotSlaves (rotSlavesUp)
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)
import XMonad.Actions.WithAll (killAll)
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
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle (Toggle (..), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.SimpleFloat (simpleFloat)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

main :: IO ()
main = xmonad . dynamicProjects projects . keybindings . ewmh . docks . config =<< mkDbusClient
  where
    config dbus =
      def
        { manageHook = myManageHook,
          logHook = dynamicLogWithPP (polybarHook dbus),
          startupHook = myStartupHook,
          terminal = myTerminal,
          modMask = mod4Mask,
          borderWidth = 1,
          handleEventHook = handleEventHook def <+> fullscreenEventHook,
          layoutHook = myLayouts,
          focusedBorderColor = "#bd93f9",
          normalBorderColor = "#434C5E",
          workspaces = myWS
        }

    keybindings = addDescrKeys' ((mod4Mask .|. controlMask, xK_F1), showKeybindings) myKeys

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
        ppTitle = const "" --shorten 40
      }
  where
    wrapper c s = wrap ("%{F" <> c <> "} ") " %{F-}" s

    blue = "#2E9AFE"
    gray = "#7F7F7F"
    orange = "#ea4300"
    purple = "#9058c7"
    -- red       = "#722222"
    focusedFg = "#bd93f9"
    fg = "#ebdbb2"
    bg = "#282828"
    bg1 = "#3c3836"
    red = "#fb4934"

myManageHook :: ManageHook
myManageHook = manageApps <+> manageSpawn <+> manageScratchpads
  where
    isBrowserDialog = isDialog <&&> className =? "chromium-browser"

    isFileChooserDialog = isRole =? "GtkFileChooserDialog"

    isPopup = isRole =? "pop-up"

    isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

    isRole = stringProperty "WM_WINDOW_ROLE"

    tileBelow = insertPosition Below Newer

    manageScratchpads = namedScratchpadManageHook scratchpads

    anyOf :: [Query Bool] -> Query Bool
    anyOf = foldl (<||>) (pure False)

    match :: [App] -> Query Bool
    match = anyOf . fmap isInstance

    manageApps :: ManageHook
    manageApps =
      composeOne
        [ match [office] -?> doFloat,
          match [btm, spotify, mpv, yad] -?> doFullFloat,
          resource =? "desktop_window" -?> doIgnore,
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

myEditor = "emacs"

myTerminal = "kitty"

appLauncher = "rofi -matching fuzzy rofi -show drun -modi drun,run -show-icons"

mailClient = "claws-mail"

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
projects :: [Project]
projects =
  [ Project
      { projectName = webWs,
        projectDirectory = "~/",
        projectStartHook = Just $ spawn "chromium-browser"
      },
    Project
      { projectName = devWs,
        projectDirectory = "~/repos/github.com/erikbackman",
        projectStartHook = Just $ spawn myTerminal
      },
    Project
      { projectName = comWs,
        projectDirectory = "~/",
        projectStartHook =
          Just $
            spawn mailClient
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
        projectStartHook = Nothing
      },
    Project
      { projectName = etcWs,
        projectDirectory = "~/",
        projectStartHook = Nothing
      }
  ]

myLayouts =
  avoidStruts
    . smartBorders
    . fullScreenToggle
    . comLayout
    . devLayout
    . webLayout
    . etcLayout
    . wrkLayout
    $ (tiled ||| Mirror tiled ||| column3 ||| full)
  where
    -- default tiling algorithm partitions the screen into two panes

    tiled = gapSpaced 10 $ Tall nmaster delta ratio
    full = gapSpaced 5 Full
    column3 = gapSpaced 10 $ ThreeColMid 1 (3 / 100) (1 / 2)
    float = simpleFloat
    mirrorTall = gapSpaced 10 $ Mirror (Tall 1 (3 / 100) (1 / 2))
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100 -- Gaps bewteen windows
    myGaps gap = gaps [(U, gap), (D, gap), (L, gap), (R, gap)]
    gapSpaced g = spacing g . myGaps g
    -- Per workspace layout
    comLayout = onWorkspace comWs (tiled ||| full)
    devLayout = onWorkspace devWs (full ||| column3 ||| tiled ||| mirrorTall)
    webLayout = onWorkspace webWs (tiled ||| full)
    wrkLayout = onWorkspace wrkWs (tiled ||| full)
    etcLayout = onWorkspace etcWs float
    -- Fullscreen
    fullScreenToggle = mkToggle (single NBFULL)

-- Misc
-- master l = fixMastered (1 / 4) (1 / 2) l

myStartupHook :: X ()
myStartupHook = do
  -- spawnOnce "xrandr --output DP-0 --mode 3440x1440 --rate 99.98"
  -- spawnOnce "~/.fehbg &"
  -- spawnOnce "xset r rate 500 33"
  -- spawnOnce "systemctl --user restart polybar"
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

office = ClassApp "libreoffice" "libreoffice"

btm = TitleApp "btm" "kitty --title=btm btm"

nautilus = ClassApp "Org.gnome.Nautilus" "nautilus"

spotify = ClassApp "Spotify" "myspotify"

mpv = ClassApp "mpv" "mpv"

yad = ClassApp "Yad" "yad --text-info --text 'XMonad'"

getNameCommand :: App -> (AppClassName, AppCommand)
getNameCommand (ClassApp n c) = (n, c)
getNameCommand (TitleApp n c) = (n, c)
getNameCommand (NameApp n c) = (n, c)

getAppName :: App -> AppClassName
getAppName = fst . getNameCommand

getAppCommand :: App -> AppCommand
getAppCommand = snd . getNameCommand

scratchpadApp :: App -> NamedScratchpad
scratchpadApp app =
  NS (getAppName app) (getAppCommand app) (isInstance app) defaultFloating

runScratchpadApp :: App -> X ()
runScratchpadApp = namedScratchpadAction scratchpads . getAppName

scratchpads :: NamedScratchpads
scratchpads = scratchpadApp <$> [spotify, btm]

playerctl :: String -> String
playerctl c = "playerctl --player=spotify,%any " <> c

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x =
  addName "Show Keybindings" . io $
    E.bracket
      (spawnPipe $ getAppCommand yad)
      hClose
      (\h -> hPutStr h (unlines $ showKm x))

myKeys conf@XConfig {XMonad.modMask = modm} =
  keySet "Applications" [key "Slack" (modm, xK_F2) $ spawnOn comWs "slack"]
    ^++^ keySet
      "Audio"
      [ key "Mute" (0, xF86XK_AudioMute) $ spawn "amixer -q set Master toggle",
        key "Lower volume" (0, xF86XK_AudioLowerVolume) $
          spawn "amixer -q set Master 5%-",
        key "Raise volume" (0, xF86XK_AudioRaiseVolume) $
          spawn "amixer -q set Master 5%+",
        key "Play / Pause" (0, xF86XK_AudioPause) $ spawn $ playerctl "play-pause",
        key "Stop" (0, xF86XK_AudioStop) $ spawn $ playerctl "stop",
        key "Previous" (0, xF86XK_AudioPrev) $ spawn $ playerctl "previous",
        key "Next" (0, xF86XK_AudioNext) $ spawn $ playerctl "next"
      ]
    ^++^ keySet
      "Launchers"
      [ key "Terminal" (modm .|. shiftMask, xK_Return) $ spawn myTerminal,
        key "Apps (Rofi)" (modm, xK_p) $ spawn appLauncher,
        -- , key "Lock screen"   (modm .|. controlMask, xK_l       ) $ spawn screenLocker
        key "Emacs everywhere!" (modm .|. controlMask, xK_F10) $
          spawn "emacsclient --eval \"(emacs-everywhere)\""
      ]
    ^++^ keySet
      "Layouts"
      [ key "Next" (modm, xK_space) $ sendMessage NextLayout,
        key "Reset" (modm .|. shiftMask, xK_space) $
          setLayout (XMonad.layoutHook conf),
        key "Fullscreen" (modm, xK_f) $ sendMessage (Toggle NBFULL),
        key "Float" (modm .|. shiftMask, xK_t) $ withFocused toggleFloat
      ]
    ^++^ keySet
      "Scratchpads"
      [ key "mpv" (modm .|. controlMask, xK_a) $ runScratchpadApp mpv,
        key "bottom" (modm .|. controlMask, xK_y) $ runScratchpadApp btm,
        key "Spotify" (modm .|. controlMask, xK_s) $ runScratchpadApp spotify
      ]
    ^++^ keySet
      "System"
      [ key "Toggle status bar gap" (modm, xK_b) toggleStruts,
        key "Logout (quit XMonad)" (modm .|. shiftMask, xK_q) $ io exitSuccess,
        key "Restart XMonad" (modm, xK_q) $
          spawn "xmonad --recompile; xmonad --restart",
        key "Capture entire screen" (modm, xK_Print) $
          spawn "flameshot full -p ~/Pictures/flameshot/"
      ]
    ^++^ keySet
      "Windows"
      [ key "Close focused" (modm, xK_BackSpace) kill,
        key "Close all in ws" (modm .|. shiftMask, xK_BackSpace) killAll,
        key "Refresh size" (modm, xK_n) refresh,
        key "Focus next" (modm, xK_j) $ windows W.focusDown,
        key "Focus previous" (modm, xK_k) $ windows W.focusUp,
        key "Focus master" (modm, xK_m) $ windows W.focusMaster,
        key "Swap master" (modm, xK_Return) $ windows W.swapMaster,
        key "Swap next" (modm .|. shiftMask, xK_j) $ windows W.swapDown,
        key "Swap previous" (modm .|. shiftMask, xK_k) $ windows W.swapUp,
        key "Shrink master" (modm, xK_h) $ sendMessage Shrink,
        key "Expand master" (modm, xK_l) $ sendMessage Expand,
        key "Switch to tile" (modm, xK_t) $ withFocused (windows . W.sink),
        key "Rotate slaves" (modm .|. shiftMask, xK_Tab) rotSlavesUp,
        key "Decrease size" (modm, xK_d) $
          withFocused (keysResizeWindow (-10, -10) (1, 1)),
        key "Increase size" (modm, xK_s) $
          withFocused (keysResizeWindow (10, 10) (1, 1)),
        key "Decr  abs size" (modm .|. shiftMask, xK_d) $
          withFocused (keysAbsResizeWindow (-10, -10) (1024, 752)),
        key "Incr  abs size" (modm .|. shiftMask, xK_s) $
          withFocused (keysAbsResizeWindow (10, 10) (1024, 752))
      ]
    ^++^ keySet
      "Workspaces"
      [ key "Remove" (modm .|. shiftMask, xK_F4) removeWorkspace
      -- , key "Next" (modm, xK_comma)
      ]
      ++ switchWsById
      ++ switchWsByIdF
  where
    togglePolybar = spawn "polybar-msg cmd toggle &"

    toggleStruts = togglePolybar >> sendMessage ToggleStruts

    toggleFloat w =
      windows
        ( \s ->
            if M.member w (W.floating s)
              then W.sink w s
              else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s
        )
    keySet s ks = subtitle s : ks

    key n k a = (k, addName n a)

    action m =
      if m == shiftMask
        then "Move to "
        else "Switch to "

    -- mod-[1..9]: Switch to workspace N | mod-shift-[1..9]: Move client to workspace N
    switchWsById =
      [ key (action m <> show i) (m .|. modm, k) (windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]

    switchWsByIdF =
      [ key (action m <> show i) (m, k) (windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F6],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
