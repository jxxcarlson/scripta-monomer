module Compiler.Scripta (compile, compileToHtmlString, compileToHtmlText) where

import Prelude

import Data.Text
import Data.Text.Internal.Lazy    
import Flow ((|>)) 

import Compiler.Parser.PrimitiveBlock (PrimitiveBlock(..))
import qualified Compiler.Parser.PrimitiveBlock as Parser.PrimitiveBlock
import qualified Compiler.L0.Parser as L0.Parser
import Compiler.Parser.ExprBlock (ExprBlock(..))
import qualified Compiler.Parser.ExprBlock as Parser.ExprBlock
import Compiler.Parser.Language (Language(..))
import qualified Compiler.Render.Block

compile :: Data.Text.Text -> [ExprBlock]
compile sourceText = 
    sourceText 
      |> Data.Text.lines
      |> Parser.PrimitiveBlock.parse L0Lang (\_ -> False) 
      |> Prelude.map Parser.ExprBlock.toExpressionBlock


compileToHtmlString :: Data.Text.Text -> String
compileToHtmlString sourceText = 
  sourceText |> compile |> Compiler.Render.Block.renderToString


compileToHtmlText :: Data.Text.Text -> Data.Text.Internal.Lazy.Text
compileToHtmlText sourceText = 
  sourceText |> compile |> Compiler.Render.Block.renderToText 