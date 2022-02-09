{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Config.Keybinds where

import Config.Applications
  ( App
  , AppConfig(..)
  , appCommand
  , isInstance
  , maybeSpawn
  )
import Data.Map (Map)
import qualified Data.Map as M
import Graphics.X11
import System.Exit (exitSuccess)
import XMonad
  ( ChangeLayout(NextLayout)
  , X
  , XConfig(XConfig, modMask)
  , (.|.)
  , kill
  , sendMessage
  , spawn
  , windows
  , withFocused
  , workspaces
  )
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (killAll)
import XMonad.Core (io)
import XMonad.Layout.MultiToggle (Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL))
import qualified XMonad.StackSet as W

data KeybindConfig l = KeybindConfig
  { appConfig :: AppConfig,
    xConfig :: XConfig l
  }

keybinds :: KeybindConfig l -> Map (KeyMask, KeySym) (X ())
keybinds (KeybindConfig apps conf@XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ ((modm,               xK_q),        restart),
      ((modm .|. shiftMask, xK_q),        io exitSuccess),
      ((modm .|. shiftMask, xK_Return),   maybeSpawn (terminal apps)),
      ((modm,               xK_c),        kill),
      ((modm .|. shiftMask, xK_c),        killAll),
      ((modm,               xK_p),        maybeSpawn (launcher apps)),
      ((modm,               xK_6),        runOrRaise' $ visualEditor apps),
      ((modm,               xK_7),        runOrRaise' $ browser apps),
      ((modm,               xK_8),        runOrRaise' $ terminal apps),
      ((modm,               xK_space),    sendMessage NextLayout),
      ((modm,               xK_n),        windows W.focusUp),
      ((modm,               xK_m),        windows W.focusDown),
      ((modm,               xK_Return),   windows W.swapMaster),
      ((modm,               xK_f),        sendMessage (Toggle NBFULL)),
      ((modm .|. shiftMask, xK_t),        withFocused toggleFloat)
    ]
      <> switchWsById
  where
    restart = spawn "xmonad --recompile; xmonad --restart"
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

    runOrRaise' :: Maybe App -> X ()
    runOrRaise' Nothing = pure ()
    runOrRaise' (Just app) = runOrRaise (appCommand app) (isInstance app)
