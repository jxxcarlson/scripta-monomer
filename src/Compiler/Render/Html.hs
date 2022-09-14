{-# LANGUAGE OverloadedStrings #-}

module Compiler.Render.Html (producePage) where

-- https://jaspervdj.be/blaze/tutorial.html
-- https://mmhaskell.com/blog/2020/3/9/blaze-lightweight-html-generation

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

numbers :: Int -> Html
numbers n = docTypeHtml $ do
    H.head $ do
        H.title "Natural numbers"
    body $ do
        p "A list of natural numbers:"
        ul $ forM_ [1 .. n] (li . toHtml)

image :: Html
image = img ! src "foo.png" ! alt "A foo image."        


basicHtml :: Html
basicHtml = html $ do
  H.head $ do
    H.title "My HTML page"
  body $ do
    h1 "Welcome to our site!"

producePage :: String
producePage = renderHtml basicHtml  