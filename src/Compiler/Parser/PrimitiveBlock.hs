{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, DuplicateRecordFields, OverloadedLabels #-}

-- OverloadedRecordDot
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html

-- https://pure-hack.com/posts/overloaded-labels-in-haskell/
-- https://cpufun.substack.com/p/setting-up-the-apple-m1-for-native
-- https://www.reddit.com/r/haskell/comments/tqzxy1/now_that_stackage_supports_ghc_92_is_it_easy_to/

-- TESTING
-- https://mmhaskell.com/testing
-- https://mmhaskell.com/testing/test-driven-development
-- @av: use hspec
-- https://hspec.github.io/
-- https://www.youtube.com/watch?v=PGsDvgmZF7A
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html
-- https://pure-hack.com/posts/overloaded-labels-in-haskell/
-- https://typeclasses.com/ghc/no-implicit-prelude

-- https://jakewheat.github.io/intro_to_parsing/#very-simple-expression-parsing

module Compiler.Parser.PrimitiveBlock (PrimitiveBlock(..), args, content, checkPosition, lineNumber, position, cleanArgs, empty, parse, displayBlocks) where



import qualified Data.Text.IO as TIO
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.List
import qualified Data.Map as Map
import Prelude hiding(init)
import Flow ((|>))
import Debug.Trace

import qualified Compiler.Parser.Line as Line
import Compiler.Parser.Line (PrimitiveBlockType(..),Line) 
import Compiler.Parser.Language (Language(..)) 




{-

    The function 

        parse :: Language -> (Text -> Bool) ->  [Text] ->  [PrimitiveBlock]

    transforms input text like that displayed below

        | section 1 foo:bar yada:a b c
        Introduction

        blah blah blah ...
        etc.

          abc
          def

        || image 71 46 width:300 caption:Primitive Steam Engine
        https://techmuseum.org/steam-engine.png


    and transforms it into a list of blocks like these:

        type: Ordinary
        lineNumber: 1
        position: 0
        indent: 0
        name: section
        args:, 1
        properties: foo: bar, yada: a b c
        ------
        Introduction

        type: Paragraph
        lineNumber: 4
        position: 42
        indent: 0
        name: anon
        args:
        properties:
        ------
        blah blah blah ...
        etc.

        type: Paragraph
        lineNumber: 7
        position: 64
        indent: 2
        name: anon
        args:
        properties:
        ------
        abc
        def

        type: Verbatim
        lineNumber: 10
        position: 70
        indent: 0
        name: image
        args:, 71, 46
        properties: caption: Primitive Steam Engine, width: 300
        ------
        https://techmuseum.org/steam-engine.png

    Blocks consist of consecutive non-empty lines separated by empty lines.  A block
    consists of a possibly empty header and a body.  The header, if present, is the 
    first line.  A header line has the form

        | BLOCK-NAME ARGS

    or

        || BLOCK-NAME ARGS

    where ARGS = w1 w2 ... is a sequence of words separated by spaces.  The ARGS
    consist of postional args, which are words not containing ':', followed
    by named args.  For example, the line

        2 3 5 width:300 caption:Steam Engine

    has positional args 2 3 5 and names args 'width' and 'caption'.  This line would 
    results in the fields args and properties of a PrimitiveBlock:
        
        args = ["2", "3", "5"]
        properties = Map.fromList [("width", ["300"]), ("caption", ["Steam", "Engine"])]

    Thus positional args are retained while named args are installed in the 'properties'
    field as key-value pairs, where the value is a list of words.

    The position field indicates the position of the first character of a block on the source text.
    The lineNumber is the 1-based line number in the source text on which the block begins.

    Primitive blocks may be of BlockType PBOrdinary, PBVerbatim, or PBParagraph.  The body
    of a PBParagraph or a PBOrdinary block is later parsed as text in whatever markup
    language is used.  The body of a PBVerbatim block is not parsed.

-}
data PrimitiveBlock = PrimitiveBlock
    { indent :: Int
    , lineNumber :: Int
    , position  :: Int
    , args :: [Text]
    , properties :: Map Text Text
    , content  :: [Text]
    , name  :: Maybe Text
    , blockType  :: PrimitiveBlockType
    , sourceText :: Text
    } deriving (Eq)


empty :: PrimitiveBlock
empty = 
    PrimitiveBlock
    { indent = 0
    , lineNumber = 0
    , position = 0
    , content = []
    , name = Nothing
    , args = []
    , properties = Map.fromList []
    , sourceText = ""
    , blockType = PBParagraph
    }


data State =
   State { blocks ::  [PrimitiveBlock]
    , currentBlock :: Maybe PrimitiveBlock
    , lang :: Language
    , lines_ :: [Text]
    , inBlock :: Bool
    , inVerbatim :: Bool
    , indentation :: Int
    , currentLineNumber :: Int
    , cursor :: Int
    , isVerbatimLine :: Text -> Bool
    , count :: Int
    , label :: Text
    } 

xlog :: Show a => String -> a -> a
xlog msg a = Debug.Trace.trace (msg <> ": " <> show a) a

instance Show State where
  show state = 
    show (((label state), map content $ blocks state) )

init :: Language -> (Text -> Bool) ->  [Text] -> State
init lang_ isVerbatimLine_ lines__ =
   State{ blocks = []
    , currentBlock = Nothing
    , lang = lang_
    , lines_ = lines__
    , indentation = 0
    , currentLineNumber = 0
    , cursor = 0
    , inBlock = False
    , inVerbatim = False
    , isVerbatimLine = isVerbatimLine_
    , count = 0
    , label = "0, START"
    }

data Step state a
    = Loop state
    | Done a



{-| Parse a list of strings into a list of primitive blocks given a markup
language and a function for determining when a string is the first line
of a verbatim block
-}
parse :: Language -> (Text -> Bool) ->  [Text] ->  [PrimitiveBlock]
parse lang_ isVerbatimLine_ lines_ =
    case lang_ of
        L0Lang ->
            lines_ |> parse_ lang_ isVerbatimLine_ 

        MicroLaTeXLang ->
            --lines_ |> MicroLaTeX.Parser.TransformLaTeX.toL0 |> parse_ lang isVerbatimLine
            lines_ |> parse_ lang_ isVerbatimLine_

        
parse_ :: Language -> (Text -> Bool) ->  [Text] -> [PrimitiveBlock]
parse_ lang_ isVerbatimLine_ lines2 =
    loop (init lang_ isVerbatimLine_ lines2) nextStep
        |> map (\block -> finalize block)


head_ :: [Text] -> Maybe Text
head_ [] = Nothing 
head_ (first:_) = Just first

nextStep :: State -> Step State  [PrimitiveBlock]
nextStep state =
    case head_ $ lines_ $ state of
        Nothing ->
            case currentBlock state of
                Nothing ->
                    Done (reverse $ blocks $ state)

                Just block ->
                    let
                        newBlocks =
                            if content block == [ "" ] then
                                -- Debug.log (Tools.cyan "****, DONE" 13)
                                reverse (blocks state)

                            else
                                -- Debug.log (Tools.cyan "****, DONE" 13)
                                reverse (block : (blocks state))
                    in
                    Done newBlocks

        Just rawLine ->
            let
                -- cursor = position of current character in source text
                newCursor = cursor state + (Text.length rawLine) + 1 

                currentLine =
                    Line.classify (cursor state) (currentLineNumber state + 1) rawLine -- |> xlog "currentLine"

            in
            case ( inBlock state, Line.isEmpty currentLine, isNonEmptyBlank currentLine ) of
                -- not in a block, pass over empty line
                ( False, True, _ ) ->
                    Loop (advance newCursor state{label = "1, EMPTY" }) 

                -- not in a block, pass over blank, non-empty line
                ( False, False, True ) ->
                    Loop (advance newCursor state{label = "2, PASS" })

                -- create a new block: we are not in a block, but
                -- the current line is nonempty and nonblank
                ( False, False, False ) ->
                    Loop (createBlock newCursor currentLine  state{label = "3, NEW" } )

                -- A nonempty line was encountered inside a block, so add it
                ( True, False, _ ) ->
                    Loop (addCurrentLine newCursor currentLine  state{label = "4, ADD" } )

                -- commit the current block: we are in a block and the
                -- current line is empty
                ( True, True, _ ) ->
                    Loop (commitBlock newCursor currentLine  state{label = "5, COMMIT" } )


loop :: state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ -> loop s_ f
        Done b -> b


-- FUNCTIONS

finalize :: PrimitiveBlock -> PrimitiveBlock
finalize block =
    let
        finalContent =
           reverse (content block)

        sourceText_ =
            -- String.join "\n" content
            Text.intercalate "\n" finalContent

    in
    block{content = finalContent, sourceText = sourceText_ }


isNonEmptyBlank :: Line -> Bool
isNonEmptyBlank line =
   Line.indent line > 0 && Line.content line == ""


advance :: Int -> State -> State
advance newCursor state =
    state{lines_ = drop 1 (lines_ state)
        , currentLineNumber = (currentLineNumber state) + 1
        , cursor = newCursor
        , count = (count state) + 1
    } 

createBlock :: Int ->  Line ->  State -> State
createBlock newCursor currentLine state  =
    let
        newBlocks =
            case currentBlock state of
                Nothing ->
                    blocks state 

                -- When creating a new block push the current block onto state.blocks
                -- only if its content is nontrivial (not == [""])
                Just block ->
                    if content block == [] then
                        blocks state 

                    else
                        block : (blocks state) 

        newBlock =
            Just (blockFromLine (lang state) currentLine) 
    in
    state{lines_ = drop 1 (lines_ state)
        , currentLineNumber = (currentLineNumber state) + 1
        , cursor = newCursor 
        , count = (count state) + 1
        , indentation = (Line.indent currentLine)
        , inBlock = True
        , currentBlock = newBlock
        , blocks = newBlocks
    } 

blockFromLine :: Language -> Line -> PrimitiveBlock
blockFromLine lang_ line =
   PrimitiveBlock { indent = Line.indent line
    , lineNumber = Line.lineNumber line
    , position = Line.position line 
    , content =  [Line.content line] 
    , name = Nothing
    , args = []
    , properties = Map.empty
    , sourceText = ""
    , blockType = Line.getBlockType lang_ (Line.content line)
    }
        |> elaborate line 


elaborate :: Line -> PrimitiveBlock -> PrimitiveBlock
elaborate line pb =
    let
        ( name_, args_ ) =
            Line.getNameAndArgs line

        content = 
            case blockType pb  of
                PBParagraph -> Compiler.Parser.PrimitiveBlock.content pb
                PBOrdinary -> Compiler.Parser.PrimitiveBlock.content pb |> drop 1
                PBVerbatim -> Compiler.Parser.PrimitiveBlock.content pb |> drop 1 |> map Text.strip

    in
    pb{ content = content, name = name_, args = cleanArgs args_, properties = args_ |> prepareList |> prepareKVData }


addCurrentLine :: Int ->  Line ->  State -> State
addCurrentLine newCursor currentLine state =
    case currentBlock state of
        Nothing ->
            state{ lines_ = Prelude.drop 1 (lines_ state) } 

        Just block ->
            state{lines_ = Prelude.drop 1 (lines_ state)
                , currentLineNumber = currentLineNumber state + 1
                , cursor = newCursor 
                , count = (count state) + 1
                , currentBlock =
                    Just (addCurrentLine_ currentLine block)
            }          


addCurrentLine_ :: Line -> PrimitiveBlock -> PrimitiveBlock
addCurrentLine_ line block =
    if blockType block == PBVerbatim then
        if name block == Just "math" then
            block{  content = Line.content line : content block 
            , sourceText = Text.concat [sourceText block, "\n",  Line.prefix line, Line.content line ]}

        else
            block{ content = (Text.concat [Line.prefix line,  Line.content line]) : content block
            , sourceText = Text.concat [sourceText block, "\n",  Line.prefix line, Line.content line ]}  

    else
        block{ content = Line.content line :  content block
         , sourceText = Text.concat [sourceText block, "\n",  Line.prefix line, Line.content line ]}  
 


commitBlock :: Int -> Line -> State -> State
commitBlock newCursor currentLine state  =
    case currentBlock state of
        Nothing ->
            state{ 
                 lines_ = Prelude.drop 1 (lines_ state)
                , indentation = Line.indent currentLine
            } 

        Just block ->
            let
                ( currentBlock, newBlocks ) =
                    if content block == [ "" ] then
                        ( Nothing, blocks state )

                    else
                        ( Just (blockFromLine (lang state) currentLine), block : blocks state )
            in
            state{ 
                 lines_ = Prelude.drop 1 (lines_ state)
                , currentLineNumber = currentLineNumber state + 1
                , cursor = newCursor 
                , count = count state + 1
                , blocks = newBlocks
                , inBlock = False
                , inVerbatim = (isVerbatimLine state) (Line.content currentLine)
                , currentBlock = currentBlock
            }


-- KEY-VALUE DICTIONARY

 

data KVState = KVState { input :: [Text], kvList :: [(Text, [Text])], currentKey :: Maybe Text, currentValue :: [Text], kvStatus :: KVStatus}


{-

    ghci> dd = ["a:", "1", "2", "3", "b:", "XYX", "c:", "U", "V"] |> map Text.pack
    ghci> properties = prepareKVData dd
    ghci> Map.lookup (Text.pack "c") properties
    Just ["U","V"]

-}

cleanArgs :: [Text] -> [Text]
cleanArgs ts =
    case Data.List.findIndex (\t -> findChar ':' t) ts of
        Nothing -> ts
        Just k -> Prelude.take k ts

findChar :: Char -> Text -> Bool
findChar c txt = 
    case Text.find (\c' -> c' == c) txt of 
        Nothing -> False
        Just _ -> True

prepareKVData :: [Text] -> (Map Text Text)
prepareKVData data_ =
    let 
        initialState = KVState {input = data_, kvList = [], currentKey = Nothing, currentValue = [], kvStatus = KVInKey}
    in
    loop initialState nextKVStep


nextKVStep ::  KVState -> Step (KVState) (Map Text Text)
nextKVStep state = 
    case  Data.List.uncons $ (input state) of 
        Nothing -> 
            let
              kvList' =
                case (currentKey state) of 
                    Nothing -> (kvList state)
                    Just key -> (key, (currentValue state)): (kvList state) 
                        |> map (\(k, v) -> (k, Data.List.reverse v))
            in
            Done (Map.fromList (map (\(k,v) -> (k, Text.unwords v)) kvList'))
        Just (item, rest) ->
            case kvStatus state of
                KVInKey -> 
                    if Text.last item == ':' then
                        case currentKey state of
                            Nothing -> 
                              Loop state {input = rest, currentKey = Just (dropLast item), kvStatus = KVInValue }
                            Just key ->
                              Loop state {  input = rest
                                     , currentKey = Just (dropLast item)
                                     , kvStatus = KVInValue 
                                     , kvList = (key, currentValue state) : (kvList state)
                                     , currentValue = []
                                     }
                    else 
                        Loop state {input = rest}
                KVInValue -> if Text.last item == ':' then
                        case currentKey state of
                            Nothing -> 
                              Loop state {  input = rest
                                          , currentKey = Just (dropLast item)
                                          , currentValue = []
                                          , kvStatus = KVInValue }
                            Just key ->
                              Loop state {  input = rest
                                     , currentKey = Just (dropLast item)
                                     , kvStatus = KVInValue 
                                     , kvList = (key, currentValue state) : (kvList state)
                                     , currentValue = []
                                     }
                    else 
                       Loop state { input = rest
                              , currentValue = item : (currentValue state)
                              } 

explode :: [Text] -> [[Text]]
explode txt = map (Text.split (== ':')) txt

prepareList :: [Text] -> [Text]
prepareList ts = 
    ts |> explode |> map fix |> concat

fix :: [Text] -> [Text]
fix (a:rest:[]) = (a <> ":"):rest:[]
fix (a:[]) = a:[]


dropLast :: Text -> Text 
dropLast txt = 
  (Text.take ((Text.length txt) - 1)) txt

data KVStatus = KVInKey | KVInValue

-- DISPLAY PRIMITIVEBLOCK

displayName :: PrimitiveBlock -> Text
displayName block = 
    case name block of 
        Nothing -> "name: anon"
        Just txt -> ["name:",  txt] |> Text.unwords

displayLineNumber :: PrimitiveBlock -> Text
displayLineNumber block = 
    ["lineNumber:", (Text.pack . show) (lineNumber block)] |> Text.unwords

displayPosition :: PrimitiveBlock -> Text
displayPosition block = 
    ["position:", (Text.pack . show) (position block)] |> Text.unwords

displayIndentation :: PrimitiveBlock -> Text
displayIndentation block = 
    ["indent:", (Text.pack . show) (indent block)] |> Text.unwords



displayDict :: PrimitiveBlock -> Text 
displayDict block = 
    -- ["properties:", (properties block) |> Map.toList  |> map yazzle  |> Text.unwords] |> Text.unwords
    ["properties:", (properties block) |> Map.toList |> map yazzle  |> Text.intercalate ", "] |> Text.unwords

yazzle :: (Text, Text)  -> Text
yazzle (txt, val) =
    txt <> ": " <> val


displayBlock :: PrimitiveBlock -> Text
displayBlock block = 
    Text.unlines $ displayBlockType block : displayLineNumber block : displayPosition block : displayIndentation block : displayName block : displayArgs block : displayDict block :  "------" : (content $ block  ) 

displayBlockType :: PrimitiveBlock -> Text
displayBlockType block = 
    case blockType block of 
        PBVerbatim -> "type: Verbatim"
        PBOrdinary -> "type: Ordinary"
        PBParagraph -> "type: Paragraph"

displayArgs :: PrimitiveBlock -> Text
displayArgs block = 
    ("args:" : args block) |> Text.intercalate ", "   


displayBlocks :: [PrimitiveBlock] -> Text
displayBlocks blocks_ = 
   (map displayBlock blocks_) |> Text.unlines



checkPosition blockNumber = 
    do
    text <- TIO.readFile "ex0.txt"
    putStrLn $ "block: " <> show blockNumber
    let lines = Text.lines text
    let blocks = parseBlock text
    let block = blocks !! blockNumber
    let lineNo = (lineNumber block)
    let firstLine = lines !! (lineNo - 1)
    putStrLn $ "Line: " <> (Text.unpack firstLine)
    let pos = position block
    putStrLn $ "line number of block: " <> (show $ lineNo)
    putStrLn $ "position of block: " <> (show $ pos)
    putStrLn $ "firstLine: " <> (Text.unpack firstLine)
    putStrLn $ "slice: " <> (Text.unpack $ slice (pos) (pos + Text.length firstLine) text)
    putStrLn $ "blocks: " <> (show $ length blocks)
   

slice :: Int -> Int -> Text -> Text
slice a b text = 
    text |> Text.take (b + 1) |> Text.drop a

parseBlock :: Text -> [PrimitiveBlock]
parseBlock text = parse L0Lang (\_ -> True) (Text.lines text ) 