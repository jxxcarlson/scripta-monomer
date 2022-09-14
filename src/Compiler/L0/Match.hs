module Compiler.L0.Match (deleteAt, getSegment, hasReducibleArgs, isReducible, match, splitAt) where


import Prelude hiding(splitAt)
import Compiler.L0.Symbol (Symbol(..), value) 
import Flow ((|>))



isReducible ::  [Symbol] -> Bool
isReducible symbols_ =
    let
        symbols =
            filter (/= WS) symbols_
    in
    case symbols of
        M : rest ->
            head_(reverse rest) == Just M

        C : rest ->
            head_(reverse rest) == Just C

        L : ST : rest ->
            case head_(reverse rest) of
                Just R ->
                    hasReducibleArgs (dropLast rest)

                _ ->
                    False

        _ ->
            False


hasReducibleArgs :: [Symbol] -> Bool
hasReducibleArgs symbols =
    case symbols of
        [] ->
            True

        L : _ ->
            reducibleAux symbols

        C : _ ->
            reducibleAux symbols

        M : _ ->
            let
                seg =
                    getSegment M symbols
            in
            if isReducible seg then
                hasReducibleArgs (drop (length seg) symbols)

            else
                False

        ST : rest ->
            hasReducibleArgs rest

        _ -> False


split ::  [Symbol] -> Maybe ( [Symbol], [Symbol ])
split symbols =
    case match symbols of
        Nothing ->
            Nothing

        Just k ->
            Just (splitAt (k + 1) symbols)


reducibleAux symbols =
    case split symbols of
        Nothing ->
            False

        Just ( a, b ) ->
            isReducible a && hasReducibleArgs b


dropLast :: [a]-> [a]
dropLast list =
    let
        n =
            length list
    in
    take (n - 1) list


{-|

> deleteAt 1 [0, 1, 2]

     [0,2] : List number

-}
deleteAt :: Int -> [a] -> [a]
deleteAt k list =
    take k list ++ drop (k + 1) list


{-|

    > splitAt 2 [0, 1, 2, 3, 4]
      ([0,1],[3,4])

-}
splitAt :: Int -> [a] -> ( [a], [a] )
splitAt k list =
    ( take k list, drop k list )


data State =
   State { symbols :: [Symbol], index :: Int, brackets :: Int }


getSegment :: Symbol ->  [Symbol] ->  [Symbol]
getSegment sym symbols =
    let
        seg_ =
            takeWhile (\sym_ -> sym_ /= sym) (drop 1 symbols)

        n =
            length seg_
    in
    case  getAt (n + 1) symbols of
        Nothing ->
            sym : seg_

        Just last ->
            sym : seg_ ++ [ last ]



getAt :: Int -> [a] -> Maybe a
getAt k as = 
    as |> drop k |> head_

head_ :: [a] -> Maybe a 
head_ [] = Nothing
head_ (first:rest) = Just first

match :: [Symbol] -> Maybe Int
match symbols =
    case head_ symbols of
        Nothing ->
            Nothing

        Just symbol ->
            if elem symbol [ C, M ] then
                Just (length (getSegment symbol symbols) - 1)

            else if value symbol < 0 then
                Nothing

            else
                loop ( State{ symbols = drop 1 symbols, index = 1, brackets = value symbol } )nextStep


nextStep :: State -> Step State (Maybe Int)
nextStep state =
    case head_ (symbols state) of
        Nothing ->
            Done Nothing

        Just sym ->
            let
                brackets_ =
                    (brackets state) + value sym
            in
            if brackets_ < 0 then
                Done Nothing

            else if brackets_ == 0 then
                Done (Just (index state))

            else
                Loop $ state { symbols = drop 1 (symbols state), index = 1 + index state, brackets = brackets_  }


data Step state a
    = Loop state
    | Done a

loop :: state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ -> loop s_ f
        Done b -> b