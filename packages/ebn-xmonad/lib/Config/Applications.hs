module Config.Applications where

import Control.Monad.IO.Class (MonadIO)
import XMonad (spawn, ManageHook, Query, resource, (=?), doIgnore, className, title, appName, (<&&>), stringProperty, (<||>))
import XMonad.Hooks.ManageHelpers ((-?>), composeOne, isDialog, doCenterFloat, isFullscreen, doFullFloat, isInProperty)
import XMonad.Hooks.InsertPosition

data AppConfig = AppConfig
  { terminal :: !(Maybe App),
    launcher :: !(Maybe App),
    browser :: !(Maybe App),
    visualEditor :: !(Maybe App),
    mailClient :: !(Maybe App)
  }

type AppName = String

type AppTitle = String

type AppClassName = String

type AppCommand = String

data App
  = ClassApp AppClassName AppCommand
  | TitleApp AppTitle AppCommand
  | NameApp AppName AppCommand
  deriving (Show)

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { terminal = Nothing,
      launcher = Nothing,
      browser = Nothing,
      visualEditor = Nothing,
      mailClient = Nothing
    }

maybeSpawn :: MonadIO m => Maybe App -> m ()
maybeSpawn = maybe (pure ()) (spawn . appCommand)

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

appCommand :: App -> String
appCommand (ClassApp _ s) = s
appCommand (TitleApp _ s) = s
appCommand (NameApp _ s) = s
    
tileBelow :: ManageHook
tileBelow = insertPosition Below Newer

anyOf :: [Query Bool] -> Query Bool
anyOf = foldl (<||>) (pure False)

match :: [App] -> Query Bool
match = anyOf . fmap isInstance

isInstance :: App -> Query Bool
isInstance (ClassApp c _) = className =? c
isInstance (TitleApp t _) = title =? t
isInstance (NameApp n _) = appName =? n

isBrowserDialog :: Query Bool
isBrowserDialog = isDialog <&&> className =? "Brave-browser"

isFileChooserDialog :: Query Bool
isFileChooserDialog = isRole =? "GtkFileChooserDialog"

isPopup :: Query Bool
isPopup = isRole =? "pop-up"

isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

isRole :: Query String
isRole = stringProperty "WM_WINDOW_ROLE"
