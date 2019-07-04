{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Views
  ( homeView
  , uploadView
  , mapView
  , deleteView
  ) where

import           Data.String.Interpolate
import           Data.Text
import           Lucid
import           Lucid.Servant
import           Servant

import           App
import           Routes

link :: forall e . (IsElem e MapAPI, HasLink e) => MkLink e Text
link = safeLink' toUrlPiece (Proxy @MapAPI) (Proxy @e)

commonView :: Bool -> Text -> Html () -> App (Html ())
commonView p t b = do
  Config{..} <- getConfig
  pure $ doctypehtml_ $ do
    head_ $ do
      title_ $ toHtml ("λmap - " <> t)
      link_ [ rel_ "stylesheet"
            , type_ "text/css"
            , safeHref_ configRoot (Proxy @MapAPI) (Proxy @StaticCss)
            ]
    body_ $ do
      header_ $ do
        span_ "λmap" `with` [id_ "logo"]
        span_ (" - " <> toHtml t)
        a_ "Upload" `with` [ safeHref_ configRoot
                             (Proxy @MapAPI) (Proxy @Home)
                           , class_ "button"]
      if p then b else main_ b

homeView :: App (Html ())
homeView = commonView False "Image Upload" $
    (form_ $ do
      input_ [type_ "file", name_ "map"]
      input_ [type_ "submit"])
    `with` [ method_ "post"
           , action_ (link @Upload)
           , enctype_ "multipart/form-data"
           ]

uploadView :: Text -> Text -> App (Html ())
uploadView l dl = do
  Config{..} <- getConfig
  commonView False "Upload Successful" $ do
    p_ $ a_ "Map link" `with`
      [safeHref_ configRoot (Proxy @MapAPI) (Proxy @Map) l]
    p_ $ a_ "Deletion link" `with`
      [safeHref_ configRoot (Proxy @MapAPI) (Proxy @DeleteMap) dl]

mapView :: Maybe (Text, Int) -> App (Html ())
mapView Nothing = commonView False "View" $ do
  h1_ "Processing..."
  p_ "Reload the page at a later time."
mapView (Just (l, z)) = do
  Config{..} <- getConfig
  commonView True "View" $ do
    link_ [rel_ "stylesheet", href_ "https://unpkg.com/leaflet@1.5.1/dist/leaflet.css"]
    script_ "" `with` [src_ "https://unpkg.com/leaflet@1.5.1/dist/leaflet.js"]
    style_ "#map { height: calc(100vh - 4em); }"
    div_ "" `with` [id_ "map"]
    script_ [i|
      var map = L.map('map').setView([0,0], 1);
      L.tileLayer('#{configRoot}tiles/#{l}/{z}/({x},{y}).png', {
        maxNativeZoom: #{z},
        maxZoom: #{z + 2},
        noWrap: true,
      }).addTo(map);
    |]

deleteView :: App (Html ())
deleteView = commonView False "Deleted" $
  h1_ "Image deleted."
