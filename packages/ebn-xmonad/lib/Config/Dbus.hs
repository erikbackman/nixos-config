module Config.Dbus where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import qualified DBus.Internal.Types as D

data SignalConfig = SignalConfig
  { sObjectPath :: String
  , sInterfaceName :: String
  , sMemberName :: String
  }

mkDbusClient :: String -> IO D.Client
mkDbusClient busname = D.connectSession >>= \dbus -> requestBus dbus >> pure dbus
  where
    requestBus dbus = D.requestName dbus (D.busName_ busname) opts
    opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- Emit a DBus signal on log updates
dbusOutput :: SignalConfig -> D.Client -> String -> IO ()
dbusOutput sconf dbus str =
  let opath = D.objectPath_ $ sObjectPath sconf
      iname = D.interfaceName_ $ sInterfaceName sconf
      mname = D.memberName_ $ sMemberName sconf
      signal = D.signal opath iname mname
      body = [D.toVariant $ UTF8.decodeString str]

   in D.emit dbus $ signal {D.signalBody = body}

