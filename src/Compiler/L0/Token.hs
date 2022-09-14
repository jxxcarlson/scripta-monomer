{-# LANGUAGE OverloadedStrings #-}

module Compiler.L0.Token (parse, prettyPrint, extractMathText, getIndex, getLoc, type_, TokenType(..), length, L0Token(..), Loc(..)) where
import qualified Data.Text as Text 
import Data.Text (Text) 
import Data.List hiding(length)
import Data.List.Index (imap)
import Prelude hiding(length)

import Text.Megaparsec (parseMaybe, choice, satisfy, many, takeWhileP, getOffset, Parsec, Token, MonadParsec)
import Text.Megaparsec.Char (string)
import Data.Void
import Flow ((|>))

-- TYPES

type TokenParser = Parsec Data.Void.Void Text


data L0Token
    = LB Loc
    | RB Loc
    | S Text Loc
    | W Text Loc
    | MathToken Loc
    | CodeToken Loc
    deriving (Eq, Show)


extractMathText :: [L0Token] -> Text
extractMathText tokens = map extractMathText_ tokens |> mconcat

extractMathText_ :: L0Token -> Text
extractMathText_ token =
    case token of 
        LB _ -> "["
        RB _ -> "]"
        S txt _ -> txt
        W txt _ -> txt
        MathToken _ -> ""
        CodeToken _ -> "`"

display :: L0Token -> Text
display token = 
     case token of 
        LB _ -> "["
        RB _ -> "]"
        S txt _ -> txt
        W txt _ -> txt
        MathToken _ -> "$"
        CodeToken _ -> "`"

data Loc = Loc { begin :: Int, end :: Int, index :: Int} deriving(Eq, Show)


incrementIndex :: Int -> L0Token -> L0Token
incrementIndex k token =
  changeLoc (incrementLoc k) token

incrementLoc :: Int -> Loc -> Loc
incrementLoc k meta = meta{index = k + (index meta)}
  
changeLoc :: (Loc -> Loc) -> L0Token -> L0Token
changeLoc changeLoc token =
    case token of 
        LB meta -> LB (changeLoc meta)
        RB meta -> RB (changeLoc meta)
        S txt meta -> S txt (changeLoc meta)
        W txt meta -> W txt (changeLoc meta)
        MathToken meta -> MathToken (changeLoc meta)
        CodeToken meta -> CodeToken (changeLoc meta)

extractLoc :: (Loc -> a) -> L0Token -> a
extractLoc extract token =
    case token of 
        LB meta -> extract meta
        RB meta -> extract meta
        S txt meta -> extract meta
        W txt meta -> extract meta
        MathToken meta -> extract meta
        CodeToken meta -> extract meta


getIndex :: L0Token -> Int
getIndex token = extractLoc (\loc -> index loc) token

setIndex :: Int -> L0Token -> L0Token
setIndex k token = 
    changeLoc (\meta -> meta{index = k}) token

data State a =
    State { source :: Text
    , scanpointer :: Int
    , tokenIndex :: Int
    , sourceLength :: Int
    , tokens :: [a]
    , currentToken :: Maybe a
    , mode :: Mode
    }


data Mode
    = Normal
    | InMath
    | InCode


data TokenType
    = TLB
    | TRB
    | TS
    | TW
    | TMath
    | TCode
    | TTokenError
    deriving(Eq, Show)


type_ :: L0Token -> TokenType
type_ token =
    case token of 
        LB _ -> TLB
        RB _ -> TRB
        S txt _ -> TS
        W txt _ -> TW
        MathToken _ -> TMath
        CodeToken _ -> TCode


getLoc :: L0Token -> Loc
getLoc token =
    case token of 
        LB meta -> meta
        RB meta -> meta
        S _ meta -> meta
        W _ meta -> meta
        MathToken meta -> meta
        CodeToken meta -> meta


prettyPrint :: [L0Token] -> Text
prettyPrint tokens_ =
   tokens_ |> map display |> mconcat


length :: L0Token -> Int
length token = extractLoc (\meta -> (end meta) - (begin meta)) token

languageChars :: [Char]
languageChars =
    [ '[', ']', '`', '$' ]

parse :: Int -> Int -> Text -> Maybe [L0Token]
parse start index_ txt = case (parseMaybe (many (tokenParser start index_)) txt) of 
            Nothing -> Nothing
            Just tokens_ -> Just $ imap incrementIndex tokens_



tokenParser :: Int -> Int -> TokenParser L0Token
tokenParser start index_ =
    choice
        [ textParser start index_
        , leftBracketParser start index_
        , rightBracketParser start index_
        , mathParser start index_
        , codeParser start index_
        , whiteSpaceParser start index_
        ]

whiteSpaceParser :: Int -> Int -> TokenParser L0Token
whiteSpaceParser start index_ = 
    do
      first <- satisfy (\c -> c == ' ')
      rest <- many (satisfy (\c -> c == ' '))
      return $ W (Text.pack (first : rest)) (Loc { begin = start, end = start, index = index_})

rightBracketParser :: Int -> Int -> TokenParser L0Token
rightBracketParser start index_ = 
    do
      a <- getOffset
      _ <- satisfy (\c -> c == ']')
      return $ RB (Loc { begin = start + a, end = start + a, index = index_})


leftBracketParser :: Int -> Int -> TokenParser L0Token
leftBracketParser start index_ = 
    do
      _ <- satisfy (\c -> c == '[')
      return $ LB (Loc { begin = start, end = start, index = index_ })


textParser :: Int -> Int -> TokenParser L0Token
textParser start index_ = 
    do
      a <- getOffset
      first <- satisfy (\c -> not $ Data.List.elem c languageChars)
      rest <- ( many $ satisfy (\c -> not $ Data.List.elem c languageChars))
      b <- getOffset
      let content = Text.pack (first : rest)
      return $ S content (Loc { begin = start + a, end = start + b - 1, index = index_ })


mathParser :: Int -> Int -> TokenParser L0Token
mathParser start index_ = 
    do
      _ <- satisfy (\c -> c == '$')
      return $ MathToken (Loc { begin = start, end = start, index = index_ })


codeParser :: Int -> Int -> TokenParser L0Token
codeParser start index_ = 
    do
      _ <- satisfy (\c -> c == '`')
      return $ CodeToken (Loc { begin = start, end = start, index = index_ })

