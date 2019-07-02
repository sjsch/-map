{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Handlers where

import           Control.Monad.IO.Class
import           Data.Text
import qualified Data.Text.Lazy                  as L
import           Database.Persist
import           Lucid
import           Servant                         hiding (serveDirectoryWith)
import           Servant.Multipart
import           Servant.RawM
import           System.Directory
import           System.FilePath
import           WaiAppStatic.Storage.Filesystem
import           WaiAppStatic.Types

import           App
import           Database
import           Routes
import           Slicer
import           Style
import           Views

link :: forall e . (IsElem e MapAPI, HasLink e) => MkLink e Text
link = safeLink' toUrlPiece (Proxy @MapAPI) (Proxy @e)

tileSettings :: FilePath -> StaticSettings
tileSettings d = (defaultFileServerSettings d)
  { ssListing = Nothing }

mapTiles :: ServerT RawM App
mapTiles = do
  Config{..} <- getConfig
  serveDirectoryWith (tileSettings configTileDirectory)

upload :: MultipartData Mem -> App (Html ())
upload MultipartData {..} = do
  Config{..} <- getConfig
  case files of
    [f] -> do
      (l, dl) <- processImage (fdPayload f)
      uploadView l dl
    _ -> throwError err415

getMap :: Text -> App (Html ())
getMap l = do
  Config{..} <- getConfig
  im <- liftDB $ getBy (UniqueLink l)
  case im of
    Nothing -> throwError err404
    Just (Entity _ Image{..}) ->
      if imageProcessing
        then mapView Nothing
        else mapView $ Just (imageLink, imageZoomLevels)

deleteMap :: Text -> App (Html ())
deleteMap dl = do
  Config{..} <- getConfig
  im <- liftDB $ getBy (UniqueDeletion dl)
  case im of
    Nothing -> throwError err404
    Just (Entity k Image{..}) -> do
      liftDB $ delete k
      liftIO $
        removeDirectoryRecursive $ configTileDirectory </> unpack imageLink
      deleteView

staticCss :: App L.Text
staticCss = pure styleText

server :: ServerT MapAPI App
server = homeView
  :<|> upload
  :<|> getMap
  :<|> deleteMap
  :<|> mapTiles
  :<|> staticCss
