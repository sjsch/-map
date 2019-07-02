{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Slicer where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy   as BL
import           Data.Text
import           Database.Persist
import           System.FilePath
import           System.Random

import           App
import           Database
import           Lambdamap

generateLink :: IO Text
generateLink = pack <$> replicateM 10 (randomRIO ('a', 'z'))

processImage :: BL.ByteString -> App (Text, Text)
processImage bs = do
  l <- liftIO generateLink
  dl <- liftIO generateLink
  k <- liftDB $ insert (Image l dl True 0)
  Config{..} <- getConfig
  process <- hoistApp $ do
    r <- liftIO $ generateTiles (BL.toStrict bs)
          (configTileDirectory </> unpack l)
    case r of
      Right z -> liftDB $ do
        k' <- get k
        case k' of
          Nothing -> liftIO (print l)
          Just _  -> update k [ImageProcessing =. False, ImageZoomLevels =. z]
      Left _ -> do
        liftIO (print l)
        liftDB (delete k)
  liftIO (void $ forkIO process)
  pure (l, dl)
