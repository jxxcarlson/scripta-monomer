{-|
Module      : Main
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable
Main module for the 'Scripta' example.
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

import ScriptaTypes
import Monomer

import qualified Monomer.Lens as L

import Compiler.Scripta

type ScriptaWenv = WidgetEnv ScriptaModel ScriptaEvt
type ScriptaNode = WidgetNode ScriptaModel ScriptaEvt


buildUI
  :: WidgetEnv ScriptaModel ScriptaEvt
  -> ScriptaModel
  -> WidgetNode ScriptaModel ScriptaEvt
buildUI wenv model = widgetTree where
  sectionBgColor = wenv ^. L.theme . L.sectionColor

  searchForm = keystroke [("Enter", ScriptaSearch)] $ vstack [
      hstack [
        label "File:",
        spacer,
        textField query `nodeKey` "query",
        spacer,
        mainButton "Open!" ScriptaSearch
      ] `styleBasic` [bgColor sectionBgColor, padding 25]
    ]

  sourceTextDisplay =  vstack [
        label "Source text:",
        spacer, 
        textField sourceText `nodeKey` "query"
      ]  
    


  widgetTree = zstack [
      vstack [
        searchForm,
        sourceTextDisplay

      ]
    ]

handleEvent
  :: Sess.Session
  -> WidgetEnv ScriptaModel ScriptaEvt
  -> WidgetNode ScriptaModel ScriptaEvt
  -> ScriptaModel
  -> ScriptaEvt
  -> [EventResponse ScriptaModel ScriptaEvt ScriptaModel ()]
handleEvent sess wenv node model evt = case evt of
  ScriptaInit -> [SetFocusOnKey "query"]
  ScriptaSearch -> [
    Model $ model & searching .~ True,
    Task $ openFile sess (model ^. query)
    -- Model $ model & sourceText .~ "FOO BAR"
    ]
  ScriptaSearchResult -> [
    Message "mainScroll" ScrollReset,
    Model $ model
      & searching .~ False
      & errorMsg .~ Nothing
    ]
  ScriptaSearchError msg -> [
    Model $ model
      & searching .~ False
      & errorMsg ?~ msg
    ]
  FileProcessed txt -> [
    Model $ model
      & searching .~ False
      & sourceText .~ txt
    ]


openFile :: Sess.Session -> Text -> IO ScriptaEvt
openFile sess query = do
  putStrLn . T.unpack $ "Searching: " <> query
  contents <- TIO.readFile $ T.unpack ("files/" <> query)
  putStrLn $ Compiler.Scripta.compileToHtmlString $ contents
    -- A Task requires returning an event, since in general you want to notify users about the result of the action
  return (FileProcessed contents)
  

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
      appInitEvent ScriptaInit
      ]
    initModel = ScriptaModel "" False Nothing ""



customLightTheme :: Theme
customLightTheme = lightTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#ECECEC"

customDarkTheme :: Theme
customDarkTheme = darkTheme
  & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"

-- Utility function to avoid the "Ambiguous type variable..." error
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch