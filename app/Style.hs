{-# LANGUAGE OverloadedStrings #-}

module Style
  ( styleText
  ) where

import           Clay
import           Data.Text.Lazy

styleText :: Text
styleText = renderWith pretty [] css

global :: Css
global = do
  a ?
    textDecoration none
  body ? do
    sym margin nil
    fontFamily ["Noto Sans"] [sansSerif]

headerCol :: Color
headerCol = "#eee"

primaryCol :: Color
primaryCol = "#408"

greyCol :: Color
greyCol = "#555"

headerCss :: Css
headerCss =
  header ? do
    backgroundColor headerCol
    fontSize (pt 16)
    sym padding (px 8)
    "#logo" ? do
      fontWeight bold
      color primaryCol
    ".button" ? do
      float floatRight
      color greyCol
      hover &
        color primaryCol

mainCss :: Css
mainCss =
  main_ ? do
    sym padding (px 20)
    margin nil auto nil auto
    maxWidth (em 40)

css :: Css
css = do
  global
  headerCss
  mainCss
