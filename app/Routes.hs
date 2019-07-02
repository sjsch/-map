{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Routes where

import           Data.Text
import qualified Data.Text.Lazy          as L
import           Data.Text.Lazy.Encoding
import           Network.HTTP.Media      ((//), (/:))
import           Servant
import           Servant.HTML.Lucid
import           Servant.Multipart
import           Servant.RawM

import           Lucid

type Home = Get '[HTML] (Html ())

type Upload
  = "upload" :> MultipartForm Mem (MultipartData Mem) :> Post '[HTML] (Html ())

type Map = "map" :> Capture "link" Text :> Get '[HTML] (Html ())

type MapTiles = "tiles" :> RawM

type DeleteMap = "delete" :> Capture "link" Text :> Get '[HTML] (Html ())

data Css
instance Accept Css where
   contentType _ = "text" // "css" /: ("charset", "utf-8")

instance MimeRender Css L.Text where
   mimeRender _ = encodeUtf8

type StaticCss = "style.css" :> Get '[Css] L.Text

type MapAPI = Home :<|> Upload :<|> Map :<|> DeleteMap :<|> MapTiles :<|> StaticCss
