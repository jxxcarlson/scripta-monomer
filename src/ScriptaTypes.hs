{-|
Module      : ScriptaTypes
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable
Types for the 'Scripta' example.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module ScriptaTypes where

import Control.Lens.TH
import Data.Aeson
import Data.Default
import Data.Maybe
import Data.Text 
import Data.Text.Internal.Lazy

import Monomer


data ScriptaModel = ScriptaModel {
  _bkmQuery :: Data.Text.Text,
  _bmkSearching :: Bool,
  _bkmErrorMsg :: Maybe Data.Text.Text,
  _bkmSourceText :: Data.Text.Text,
  _bkmRenderedText :: Data.Text.Text
} deriving (Eq, Show)

data ScriptaEvt
  = ScriptaInit
  | ScriptaSearch
  | ScriptaSearchResult
  | ScriptaSearchError Data.Text.Text
  | FileProcessed Data.Text.Text Data.Text.Text
  deriving (Eq, Show)


makeLensesWith abbreviatedFields 'ScriptaModel
