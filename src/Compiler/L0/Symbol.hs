{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Compiler.L0.Symbol (Symbol(..), balance, toText, toSymbols, value) where

import qualified Data.Text as Text 
import Data.Text (Text) 
import Flow ((|>))

import Compiler.L0.Token(L0Token(..))


data Symbol
    = L -- LB, [
    | R -- RB, ]
    | ST -- S String (string)
    | M -- dollar sign
    | C -- `
    | WS -- W String (whitespace)
    deriving(Eq, Show)


value :: Symbol -> Int
value symbol =
    case symbol of
        L ->
            1

        R ->
            -1

        ST ->
            0

        WS ->
            0

        M ->
            0

        C ->
            0


balance :: [Symbol] -> Int
balance symbols =
    symbols |> map value |> sum


symbolToText :: Symbol -> Text
symbolToText symbol =
    show symbol |> Text.pack


toText :: [Symbol] -> Text
toText symbols =
  map symbolToText symbols |> Text.intercalate " " 

toSymbols :: [L0Token] -> [Symbol]
toSymbols tokens =
    map toSymbol tokens


toSymbol :: L0Token -> Symbol
toSymbol token =
    case token of
        LB _ ->
            L

        RB _ ->
            R

        S _ _ ->
            ST

        W _ _ ->
            WS

        MathToken _ ->
            M

        CodeToken _ ->
            C

