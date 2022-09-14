{-# LANGUAGE OverloadedStrings #-}

module Compiler.Parser.Expr (Expr(..), displayExpr) where

import Data.Text (Text)
import qualified Data.Text

import Compiler.Parser.Meta (Meta)

data Expr
    = Fun Data.Text.Text [Expr] Meta
    | Text Data.Text.Text Meta
    | Verbatim Text Data.Text.Text Meta
    deriving(Show)

displayExpr :: Expr -> Text
displayExpr expr = 
    case expr of 
        Fun txt exprs _ -> "Function " <> txt <> ": " <> (mconcat $ map displayExpr exprs)
        Text txt _ -> txt
        Verbatim name txt _ -> "Verbatim " <> name <> ": " <> txt