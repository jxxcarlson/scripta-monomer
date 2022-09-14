{-# LANGUAGE OverloadedStrings,  DuplicateRecordFields #-}

module Compiler.Parser.Line(PrimitiveBlockType(..), Line, getNameAndArgs, isEmpty, indent, prefix,content, lineNumber, position, classify, getBlockType) where

-- https://markkarpov.com/tutorial/megaparsec.html
-- https://serokell.io/blog/parser-combinators-in-haskell
-- https://akashagrawal.me/2017/01/19/beginners-guide-to-megaparsec.html

import qualified Data.Text as Text 
import Data.Text (Text) 
import Text.Megaparsec
import Data.Void

import Compiler.Parser.Language (Language(..)) 

data Line =
    Line { indent :: Int, prefix :: Text, content :: Text, lineNumber :: Int, position :: Int } deriving(Show)

type LineParser = Parsec Data.Void.Void Text

data PrimitiveBlockType = PBVerbatim | PBOrdinary | PBParagraph deriving (Show, Eq)


classify :: Int -> Int -> Text -> Line
classify position_ lineNumber_ txt =
     case parseMaybe (lineParser position_ lineNumber_) txt of 
        Nothing -> Line {indent = 0, prefix = "", content = "", lineNumber = 0, position = 0}
        Just l -> l

isEmpty :: Line -> Bool
isEmpty line =
    indent line == 0 && content line == ""


getBlockType :: Language -> Text -> PrimitiveBlockType
getBlockType lang line_ =
    let
        line =
            Text.strip line_
    in
    case lang of
        L0Lang ->
            if Text.take 2 line == "||" then
                PBVerbatim

            else if Text.take 2 line == "$$" then
                PBVerbatim

            else if
                Text.take 1 line
                    == "|"
            then
                PBOrdinary

            else
                PBParagraph

        MicroLaTeXLang ->
            -- Note the source text has already been partially transformed to conform to L0
            if Text.take 2 line == "||" then
                PBVerbatim

            else if Text.take 2 line == "$$" then
                PBVerbatim

            else if
                Text.take 1 line
                    == "|"
            then
                PBOrdinary

            else
                PBParagraph


getNameAndArgs :: Line -> (Maybe Text, [Text])
getNameAndArgs line =
    let
        normalizedLine =
            Text.strip (content line)

        -- account for possible indentation
    in
    if Text.take 2 normalizedLine == "||" then
        let
            words_ =
                Text.words (Text.drop 3 normalizedLine)

            name =
                 head_ words_

            args =
                Prelude.drop 1 words_
        in
        (  name, args )

    else if Text.take 1 normalizedLine == "|" then
        let
            words_ =
                Text.words (Text.drop 2 normalizedLine)

            name =
                head_ words_ 

            args =
                Prelude.drop 1 words_
        in
        ( name, args )

    else if Text.take 2 (content line) == "$$" then
        ( Just "math", [] )

    else
        ( Nothing, [] )


head_ :: [a] -> Maybe a 
head_ [] = Nothing 
head_ (first:_) = Just first


data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)


lineParser :: Int -> Int -> LineParser Line
lineParser position_ lineNumber_ = 
  do 
    prefix_ <- many (satisfy (\c -> c == ' ')) 
    content_ <- many (satisfy (\c -> c /= '\n')) 
    return Line {indent =  Prelude.length prefix_, prefix = Text.pack prefix_, position = position_, lineNumber = lineNumber_, content = Text.pack content_}

-- slice :: Int -> Int -> [a] -> [a]
-- slice from to xs = Prelude.take (to - from + 1) (Prelude.drop from xs)
