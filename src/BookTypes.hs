{-|
Module      : BookTypes
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

module BookTypes where

import Control.Lens.TH
import Data.Aeson
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Monomer

data Book = Book {
  _bkTitle :: Text,
  _bkAuthors :: [Text],
  _bkYear :: Maybe Int,
  _bkCover :: Maybe Int
} deriving (Eq, Show)

instance FromJSON Book where
  parseJSON = withObject "Book" $ \b -> Book
    <$> b .: "title"
    <*> b .:? "author_name" .!= []
    <*> b .:? "first_publish_year"
    <*> b .:? "cover_i"

data BookResp = BookResp {
  _brDocs :: [Book],
  _brFound :: Int
} deriving (Eq, Show)

instance FromJSON BookResp where
  parseJSON = withObject "BookResp" $ \b -> BookResp
    <$> b .: "docs"
    <*> b .: "numFound"

data ScriptaModel = ScriptaModel {
  _bkmQuery :: Text,
  _bmkSearching :: Bool,
  _bkmErrorMsg :: Maybe Text,
  _bkmScripta :: [Book],
  _bmkSelected :: Maybe Book
} deriving (Eq, Show)

data ScriptaEvt
  = ScriptaInit
  | ScriptaSearch
  | ScriptaSearchResult BookResp
  | ScriptaSearchError Text
  | ScriptaShowDetails Book
  | ScriptaCloseDetails
  | ScriptaCloseError
  deriving (Eq, Show)

makeLensesWith abbreviatedFields 'Book
makeLensesWith abbreviatedFields 'BookResp
makeLensesWith abbreviatedFields 'ScriptaModel
