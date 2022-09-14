{-# LANGUAGE OverloadedStrings,  DuplicateRecordFields #-}

module Compiler.Parser.Base (Parser, fooParser) where

-- https://markkarpov.com/tutorial/megaparsec.html
-- https://serokell.io/blog/parser-combinators-in-haskell
-- https://akashagrawal.me/2017/01/19/beginners-guide-to-megaparsec.html

import qualified Data.Text as Text 
import Data.Text (Text) 
import Text.Megaparsec 
import Text.Megaparsec.Char (string)
import Data.Void

type Parser = Parsec Data.Void.Void Text

fooParser = string "foo" :: Parser Text





















-- char :: MonadParsec e s m => Token s -> m (Token s)

-- char i  = satisfy(==i)

-- string :: (Traversable t, MonadParsec e s f) => t (Token s) -> f (t (Token s))
-- string = traverse char


-- char' :: Char -> TokenParser Char
-- char' i  = satisfy(==i)


-- string' :: String -> TokenParser String
-- string' = (traverse char) 

