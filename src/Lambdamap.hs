{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeFamilies        #-}

module Lambdamap
  ( generateTiles
  ) where

import           Control.Monad.Except

import           Data.ByteString            (ByteString)
import           Data.Foldable
import           Data.Maybe

import           Vision.Image
import           Vision.Image.Storage.DevIL
import           Vision.Primitive

import           System.Directory
import           System.FilePath

{-# COMPLETE IX2 #-}

pattern IX2 :: Int -> Int -> DIM2

pattern IX2 x y = (Z :. x) :. y

tileSize :: Int
tileSize = 512

{-# INLINABLE expand #-}
expand ::
     (Image i1, FromFunction i2, ImagePixel i1 ~ FromFunctionPixel i2)
  => Size
  -> ImagePixel i1
  -> i1
  -> i2
expand (IX2 h w) c !img =
  fromFunction (IX2 h w) $ \p@(IX2 y x) ->
    if y < oh && x < ow
      then img ! p
      else c
  where
    IX2 oh ow = shape img

cropTile :: RGB -> (Int, Int) -> Int -> (Int, Int) -> RGB
cropTile i (w, h) z (x, y) =
  compute $ e $ r (crop (Rect (x * s) (y * s) sx sy) i :: RGBDelayed)
  where
    s = 2 ^ z * tileSize
    sx = min s (w - x * s)
    sy = min s (h - y * s)
    r =
      if s == 0
        then id
        else resize NearestNeighbor (IX2 (sy `div` 2 ^ z) (sx `div` 2 ^ z))
    e =
      if sx == s && sy == s
        then id
        else expand (IX2 tileSize tileSize) (RGBPixel 0xdd 0xdd 0xdd)

cutMap :: Int -> RGB -> [((Int, Int), RGB)]
cutMap z i = zip xys $ cropTile i (w, h) z <$> xys
  where
    xys = [(x, y) | x <- [0 .. nx - 1], y <- [0 .. ny - 1]]
    s = 2 ^ z
    tilediv :: Int -> Int
    tilediv x =
      ceiling ((fromIntegral x :: Double) / fromIntegral (tileSize * s))
    IX2 h w = shape i
    nx = tilediv w
    ny = tilediv h

generateTiles :: ByteString -> FilePath -> IO (Either StorageError Int)
generateTiles inp outdir =
  runExceptT $ do
    liftIO (createDirectoryIfMissing False outdir)
    case loadBS Autodetect inp of
      Left e -> throwError e
      Right (i :: RGB) ->
        let IX2 h w = shape i
            zooms =
              ceiling
                (logBase 2 (fromIntegral $ max h w `div` tileSize :: Double)) :: Int
         in do
          traverse_ (makeZoom zooms i) [0 .. zooms]
          pure zooms
  where
    makeZoom m i z = do
      liftIO $ createDirectoryIfMissing False (outdir </> show (m - z))
      errors <- liftIO $ traverse (writeTile (show (m - z))) (cutMap z i)
      case catMaybes errors of
        (e:_) -> throwError e
        _     -> pure ()
    writeTile z (p, i) =
      liftIO (save PNG (outdir </> z </> show p <.> "png") i) >>= \case
        Just e -> pure (Just e)
        Nothing -> pure Nothing
