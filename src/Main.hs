{-|
Module      : Main
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable
Main module for the 'Books' example.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Control.Lens
import Data.Default
import Data.Either.Extra
import Data.Maybe
import Data.Text (Text)
import TextShow

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as Sess

import BookTypes
import Monomer

import qualified Monomer.Lens as L

import Compiler.Scripta

type BooksWenv = WidgetEnv BooksModel BooksEvt
type BooksNode = WidgetNode BooksModel BooksEvt


buildUI
  :: WidgetEnv BooksModel BooksEvt
  -> BooksModel
  -> WidgetNode BooksModel BooksEvt
buildUI wenv model = widgetTree where
  sectionBgColor = wenv ^. L.theme . L.sectionColor

  searchForm = keystroke [("Enter", BooksSearch)] $ vstack [
      hstack [
        label "Query:",
        spacer,
        textField query `nodeKey` "query",
        spacer,
        mainButton "Search" BooksSearch
      ] `styleBasic` [bgColor sectionBgColor, padding 25]
    ]

  countLabel = label caption `styleBasic` [padding 10] where
    caption = "Books (" <> showt (length $ model ^. books) <> ")"

  booksChanged wenv old new = old ^. books /= new ^. books

  widgetTree = zstack [
      vstack [
        searchForm
      ]
    ]

handleEvent
  :: Sess.Session
  -> WidgetEnv BooksModel BooksEvt
  -> WidgetNode BooksModel BooksEvt
  -> BooksModel
  -> BooksEvt
  -> [EventResponse BooksModel BooksEvt BooksModel ()]
handleEvent sess wenv node model evt = case evt of
  BooksInit -> [SetFocusOnKey "query"]
  BooksSearch -> [
    Model $ model & searching .~ True,
    Task $ searchBooks sess (model ^. query)
    ]
  BooksSearchResult resp -> [
    Message "mainScroll" ScrollReset,
    Model $ model
      & searching .~ False
      & errorMsg .~ Nothing
      & books .~ resp ^. docs
    ]
  BooksSearchError msg -> [
    Model $ model
      & searching .~ False
      & errorMsg ?~ msg
      & books .~ []
    ]
  BooksShowDetails book -> [Model $ model & selected ?~ book]
  BooksCloseDetails -> [Model $ model & selected .~ Nothing]
  BooksCloseError -> [Model $ model & errorMsg .~ Nothing]

searchBooks :: Sess.Session -> Text -> IO BooksEvt
searchBooks sess query = do
  putStrLn . T.unpack $ "Searching: " <> query
  contents <- TIO.readFile "files/hello.txt"
  putStrLn $ Compiler.Scripta.compileToHtmlString $ contents
    -- A Task requires returning an event, since in general you want to notify users about the result of the action
  result <- catchAny (fetch url) (return . Left . T.pack . show)

  case result of
    Right resp -> return (BooksSearchResult resp)
    Left err -> return (BooksSearchError err)
  where
    url = "https://openlibrary.org/search.json?q=" <> T.unpack query
    checkEmpty resp
      | null (resp ^. docs) = Nothing
      | otherwise = Just resp
    fetch url = do
      resp <- Sess.get sess url
        >>= W.asJSON
        >>= return . preview (W.responseBody . _Just)

      return $ maybeToEither "Empty response" (resp >>= checkEmpty)

main :: IO ()
main = do
  sess <- Sess.newAPISession
  startApp initModel (handleEvent sess) buildUI config
  where
    config = [
      appWindowTitle "Book search",
      appWindowIcon "./assets/images/icon.png",
      appTheme customDarkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      appInitEvent BooksInit
      ]
    initModel = BooksModel "" False Nothing [] Nothing

customLightTheme :: Theme
customLightTheme = lightTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#ECECEC"

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"

-- Utility function to avoid the "Ambiguous type variable..." error
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch