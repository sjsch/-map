{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Proxy
import qualified Data.Text.IO             as T
import           Database.Persist.Sqlite
import           Network.Wai.Handler.Warp
import           Servant.Server
import           System.Directory
import           System.Environment
import           Toml                     ((.=))
import qualified Toml                     as T

import           App
import           Database
import           Handlers
import           Routes

configCodec :: T.TomlCodec Config
configCodec = Config
  <$> T.table (T.int "port") "server" .= configPort
  <*> T.table (T.text "root") "server" .= configRoot
  <*> T.table (T.string "tiles") "storage" .= configTileDirectory
  <*> T.table (T.text "database") "storage" .= configSqlite

app :: Config -> ConnectionPool -> Application
app config pool = serve @MapAPI Proxy (hoistServer @MapAPI Proxy f server)
  where
    f x = runReaderT x (AppContext config pool)

main :: IO ()
main = do
  args <- getArgs
  let configFile = case args of
        []              -> "lambdamap.toml"
        ["--config", f] -> f
        _               -> fail "Couldn't parse command line."
  configExists <- doesFileExist configFile
  config <- if configExists
    then T.decodeFile configCodec configFile
    else do T.writeFile configFile (T.encode configCodec defaultConfig)
            pure defaultConfig
  createDirectoryIfMissing True (configTileDirectory config)
  pool <- runStderrLoggingT $ createSqlitePool (configSqlite config) 5
  runSqlPool (runMigration migrateAll) pool
  run (configPort config) (app config pool)
