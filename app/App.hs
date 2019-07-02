{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module App
  ( Config(..)
  , defaultConfig
  , AppContext(..)
  , App
  , AppT
  , hoistApp
  , liftDB
  , getConfig
  ) where

import           Control.Monad.Reader
import           Data.Text                (Text)
import           Database.Persist.Sqlite
import           Network.Wai.Handler.Warp
import           Servant

data Config = Config
  { configPort          :: Port
  , configRoot          :: Text
  , configTileDirectory :: FilePath
  , configSqlite        :: Text
  } deriving (Eq, Ord, Show)

defaultConfig :: Config
defaultConfig = Config
  { configPort = 3000
  , configRoot = "/"
  , configTileDirectory = "./tiles"
  , configSqlite = "./lambdamap.sqlite"
  }

data AppContext =
  AppContext { contextConfig   :: Config
             , contextDatabase :: ConnectionPool
             }

type AppT = ReaderT AppContext
type App = AppT Handler

liftDB :: MonadIO m => ReaderT SqlBackend IO a -> AppT m a
liftDB x = do
  AppContext{..} <- ask
  liftIO $ runSqlPool x contextDatabase

getConfig :: Monad m => AppT m Config
getConfig = asks contextConfig

hoistApp :: AppT IO a -> App (IO a)
hoistApp x = asks (runReaderT x)
