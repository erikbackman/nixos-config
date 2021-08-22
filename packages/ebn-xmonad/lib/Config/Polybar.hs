module Config.Polybar where

import qualified DBus.Client as D
import XMonad.Hooks.DynamicLog (PP(..), wrap)
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import Config.Dbus (mkDbusClient, dbusOutput, SignalConfig(..))
import XMonad (def)
import DBus.Client (Client)

polybar :: IO Client
polybar = mkDbusClient "org.xmonad.log"

polybarHook :: D.Client -> PP
polybarHook dbus =
  namedScratchpadFilterOutWorkspacePP $
    def
      { ppOutput = dbusOutput signalConfig dbus,
        ppCurrent = wrapper focusedFg,
        ppVisible = wrapper fg,
        ppUrgent = wrapper red,
        ppHidden = wrapper gray,
        ppWsSep = "",
        ppSep = " :: ",
        ppTitle = const ""
      }
  where
    signalConfig = SignalConfig
      { sObjectPath = "/org/xmonad/Log",
        sInterfaceName = "org.xmonad.Log",
        sMemberName = "Update"
      }

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
