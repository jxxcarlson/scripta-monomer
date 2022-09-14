{-# LANGUAGE OverloadedStrings #-}


module Compiler.Parser.Meta (Meta(..), dummy) where

import Prelude hiding(id)
import qualified Data.Text as Text 
import Data.Text (Text) 


data Meta =
    Meta { begin :: Int, end :: Int, index :: Int, id :: Text } deriving (Show)


dummy =
   Meta { begin = 0, end = 0, index = 0, id = "dummyId" }

