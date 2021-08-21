module Applications where

import Control.Monad.IO.Class (MonadIO)
import XMonad (spawn)
data AppConfig = AppConfig
  { terminal :: Maybe String,
    launcher :: Maybe String,
    browser :: Maybe String,
    visualEditor :: Maybe String,
    mailClient :: Maybe String
  }

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { terminal = Nothing,
      launcher = Nothing,
      browser = Nothing,
      visualEditor = Nothing,
      mailClient = Nothing
    }

maybeSpawn :: MonadIO m => Maybe String -> m ()
maybeSpawn = maybe (pure ()) spawn
