module Config.Applications where

import Control.Monad.IO.Class (MonadIO)
import XMonad
  ( ManageHook,
    Query,
    appName,
    className,
    doIgnore,
    resource,
    spawn,
    stringProperty,
    title,
    (<&&>),
    (<||>),
    (=?),
  )
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
  ( composeOne,
    doCenterFloat,
    doFullFloat,
    isDialog,
    isFullscreen,
    isInProperty,
    (-?>),
  )

type AppName = String

type AppTitle = String

type AppClassName = String

type AppCommand = String

data App
  = ClassApp AppClassName AppCommand
  | TitleApp AppTitle AppCommand
  | NameApp AppName AppCommand
  deriving (Show)

maybeSpawn :: MonadIO m => Maybe App -> m ()
maybeSpawn = maybe (pure ()) (spawn . appCommand)

manageApps :: ManageHook
manageApps =
  composeOne
    [ resource =? "desktop_window" -?> doIgnore,
      resource =? "kdesktop" -?> doIgnore,
      anyOf
        [ isFileChooserDialog,
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

isClassDialog :: String -> Query Bool
isClassDialog name = isDialog <&&> className =? name

isFileChooserDialog :: Query Bool
isFileChooserDialog = isRole =? "GtkFileChooserDialog"

isPopup :: Query Bool
isPopup = isRole =? "pop-up"

isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

isRole :: Query String
isRole = stringProperty "WM_WINDOW_ROLE"
