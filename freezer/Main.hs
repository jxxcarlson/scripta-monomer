{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe

import Data.Text (Text,  length)
import qualified Data.Text.IO as TIO
import TextShow
import Data.Text.Internal.Lazy

import Monomer

import Compiler.Scripta

import qualified Monomer.Lens as L

data AppModel = AppModel {
  _clickCount :: Int,
  _content :: Data.Text.Text,
  _html :: Data.Text.Internal.Lazy.Text
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppDecrease
  | AppIgnore
  | AppLoadPrint
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Hello world",
      spacer,
      hstack [
        label $ "Click count: " <> showt (model ^. clickCount),
        spacer,
        button "Increase count" AppIncrease, 
        spacer,
        button "Decrease count" AppDecrease,
        spacer,
        button "Print" AppLoadPrint
      ]
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]
  AppDecrease -> [Model (model & clickCount -~ 1)]
  AppLoadPrint -> [Task $ do
    contents <- TIO.readFile "files/hello.txt"
    putStrLn $ Compiler.Scripta.compileToHtmlString $ contents
    -- A Task requires returning an event, since in general you want to notify users about the result of the action
    return AppIgnore
    ]
  AppIgnore -> [Model (model & clickCount -~ 0)]

main :: IO ()
main = do
  contents <- readFile "files/hello.txt"
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Hello world",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel { _clickCount = 0, _content = "abc", _html = Compiler.Scripta.compileToHtmlText "abc"}
