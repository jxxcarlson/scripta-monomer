{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Compiler.L0.Parser (run) where

import qualified Data.Text as Text 
import Data.Text (Text) 
import Data.List
import Data.List.Index (imap)
import Data.Vector (Vector, fromList, (!?))
import Data.Void
import Flow ((|>))
import Prelude hiding(id)
import Debug.Trace

import Compiler.L0.Token (L0Token(..), Loc(..), TokenType(..))
import qualified Compiler.L0.Token as Token
import Compiler.Parser.Expr(Expr(..))
import qualified Compiler.Parser.Expr as Parser.Expr
import Compiler.Parser.Meta(Meta(..))
import Compiler.L0.Match as M
import Compiler.L0.Symbol as L0.Symbol
import qualified Compiler.L0.Match as Match



data State =
    State { step :: Int
    , tokens :: Vector L0Token
    , numberOfTokens :: Int
    , tokenIndex :: Int
    , committed :: [Expr]
    , stack :: [L0Token]
    , messages :: [Text]
    , lineNumber :: Int
    }

instance Show State where
  show state = 
    show (stack state)



xlog :: Show a => String -> a -> a
xlog msg a = Debug.Trace.trace (msg <> ": " <> show a) a



data Step state a
    = Loop state
    | Done a

loop :: state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ -> loop s_ f
        Done b -> b

run :: Int ->  Text -> [Expr]
run k txt = 
    case Token.parse 0 0 txt of 
        Nothing -> []
        Just tokens_ -> 
           loop (initWithTokens k tokens_)  nextStep |> committed


initWithTokens :: Int -> [L0Token] -> State
initWithTokens lineNumber_ tokens_ =
   State { step = 0
    , tokens = Data.Vector.fromList $ reverse tokens_
    , numberOfTokens = length tokens_
    , tokenIndex = 0
    , committed = []
    , stack = []
    , messages = []
    , lineNumber = lineNumber_
    }



nextStep :: State -> Step State State
nextStep state =
    case getToken state of
        Nothing ->
            if stackIsEmpty state then
                Done state

            else
                recoverFromError state

        Just token ->
            state
                |> advanceTokenIndex
                |> pushOrCommit token
                |> reduceState
                -- |> xlog "Stack"
                -- |> (\st -> State { step = 1 + step st })
                |> Loop

getToken :: State -> Maybe L0Token
getToken state = (tokens state) !? (tokenIndex state)

stackIsEmpty :: State -> Bool
stackIsEmpty state = (stack state) == []

advanceTokenIndex :: State -> State
advanceTokenIndex state = state {tokenIndex = 1 + (tokenIndex state)}


recoverFromError :: State -> Step State State
recoverFromError state = Done state


reduceState :: State -> State
reduceState state =
    if tokensAreReducible state then
        state {stack = [], committed = reduceStack state ++ (committed state)}

    else
        state


tokensAreReducible state =
    M.isReducible (stack state 
      |> L0.Symbol.toSymbols 
      -- |> xlog "Symbols"
      )
     --  |> xlog "Reducible"


reduceStack :: State -> [Expr]
reduceStack state =
    reduceTokens (lineNumber state) (stack state)

head_ :: [a] -> Maybe a 
head_ [] = Nothing
head_ (first:rest) = Just first

reduceTokens :: Int -> [L0Token] -> [Expr]
reduceTokens lineNumber tokens =
    if isExpr tokens then
        let
            args =
                unbracket tokens
        in
        case args of
            -- The reversed token list is of the form [LB name EXPRS RB], so return [Expr name (evalList EXPRS)]
            (S name loc) : _ ->
                let 
                    ws = Text.words name
                    name_ = head_ ws |> maybe "anon" (\a -> a)
                    txt  = Text (drop 1 ws |>  Text.unwords) dummyLocWithId
                in 
                if length ws <= 1 then
                  [ Fun name_ (reduceRestOfTokens lineNumber (drop 1 args)) (boostMeta lineNumber (Token.index loc) loc) ]
                else 
                  [ Fun name_ (txt : (reduceRestOfTokens lineNumber (drop 1 args))) (boostMeta lineNumber (Token.index loc) loc) ]
                

            _ ->
                -- this happens with input of "[]"
                [ errorMessage "[????]" ]

    else
        case tokens of            
            (MathToken loc) : rest ->
                Verbatim "math" (Token.extractMathText rest) (boostMeta lineNumber (Token.index loc) loc) : []
            
            (CodeToken loc) : (S str _) : (CodeToken _) : rest ->
                Verbatim "code" str (boostMeta lineNumber (Token.index loc) loc) : reduceRestOfTokens lineNumber rest

            _ ->
                [ errorMessage "[????]" ]


reduceRestOfTokens :: Int -> [L0Token] -> [Expr]
reduceRestOfTokens lineNumber tokens =
    case tokens of
        (LB _) : _ ->
            case splitTokens tokens of
                Nothing ->
                    [ errorMessageInvisible "Error on match", Text "error on match" dummyLocWithId ]

                Just ( a, b ) ->
                    reduceTokens lineNumber a ++ reduceRestOfTokens lineNumber b

        (MathToken _) : _ ->
            let
                ( a, b ) =
                    splitTokensWithSegment tokens
            in
            reduceTokens lineNumber a ++ reduceRestOfTokens lineNumber b

        (CodeToken _) : _ ->
            let
                ( a, b ) =
                    splitTokensWithSegment tokens
            in
            reduceTokens lineNumber a ++ reduceRestOfTokens lineNumber b

        (S str meta) : _ ->
            Text str (boostMeta 0 (Token.getIndex (S str meta)) meta) : reduceRestOfTokens lineNumber (drop 1 tokens)

        token : _ ->
            case stringTokenToExpr token of
                Just expr ->
                    expr : reduceRestOfTokens lineNumber (drop 1 tokens)

                Nothing ->
                    [ errorMessage ("Line " <> (Text.pack $ show lineNumber) <> ", error converting token"), Text "error converting Token" dummyLocWithId ]

        _ ->
            []



pushOrCommit :: L0Token -> State -> State
pushOrCommit token state =
    case token of
        S _ _ ->
            pushOrCommit_ token state

        W _ _ ->
            pushOrCommit_ token state

        MathToken _ ->
            pushOnStack_ token state

        CodeToken _ ->
            pushOnStack_ token state

        LB _ ->
            pushOnStack_ token state

        RB _ ->
            pushOnStack_ token state


pushOnStack_ :: L0Token -> State -> State
pushOnStack_ token state =
    state { stack = token : (stack state) }


pushOrCommit_ :: L0Token -> State -> State
pushOrCommit_ token state =
    if stackIsEmpty state then
        commit token state

    else
        push token state

push :: L0Token -> State -> State
push token state =
    state { stack = token : (stack state) }


commit :: L0Token -> State -> State
commit token state =
    case stringTokenToExpr token of
        Nothing ->
            state

        Just expr ->
            state { committed = expr : (committed state) }


stringTokenToExpr :: L0Token -> Maybe Expr
stringTokenToExpr token =
    case token of
        S txt loc ->
            Just (Parser.Expr.Text txt (boostMeta 0 (Token.getIndex token) loc))

        W txt loc ->
            Just (Parser.Expr.Text txt (boostMeta 0 (Token.getIndex token) loc))

        _ ->
            Nothing


boostMeta :: Int -> Int -> Loc -> Meta
boostMeta lineNumber tokenIndex loc =
   Meta { begin = (Token.begin loc), end = (Token.end loc), index = (Token.index loc), id = makeId lineNumber tokenIndex }


makeId :: Int -> Int -> Text
makeId a b =
    (Text.pack $ show a) <> "." <> (Text.pack $ show b)



{-| remove first and last token
-}
unbracket :: [a] -> [a]
unbracket list =
    drop 1 (take ((length list) - 1) list)


{-| areBracketed tokens == True iff tokens are derived from "[ ... ]"
-}
isExpr :: [L0Token] -> Bool
isExpr tokens =
    map Token.type_ (take 1 tokens)
        == [ TLB ]
        && map Token.type_ (take 1 (reverse tokens))
        == [ TRB ]

errorMessage :: Text -> Expr
errorMessage message =
    Fun "errorHighlight" [ Text message dummyLocWithId ] dummyLocWithId


errorMessageInvisible :: Text -> Expr
errorMessageInvisible message =
    Fun "invisible" [ Text message dummyLocWithId ] dummyLocWithId


dummyLocWithId :: Meta
dummyLocWithId =
    Meta { begin = 0, end = 0, index = dummyTokenIndex, id = "dummy (2)" }

dummyTokenIndex = 0

splitTokens :: [L0Token] -> Maybe ( [L0Token], [L0Token] )
splitTokens tokens =
    case M.match (L0.Symbol.toSymbols tokens) of
        Nothing ->
            Nothing

        Just k ->
            Just (M.splitAt (k + 1) tokens)

splitTokensWithSegment :: [L0Token] -> ( [L0Token], [L0Token] )
splitTokensWithSegment tokens =
    M.splitAt (segLength tokens + 1) tokens


segLength :: [L0Token] -> Int
segLength tokens =
    M.getSegment M (tokens |> L0.Symbol.toSymbols) |> length

