module L0.Parser.Match exposing (deleteAt, getSegment, hasReducibleArgs, isReducible, match, splitAt)

import L0.Parser.Symbol exposing (Symbol(..), value)
import List.Extra
import Parser.Helpers exposing (Step(..), loop)


isReducible : List Symbol -> Bool
isReducible symbols_ =
    let
        symbols =
            List.filter (\sym -> sym /= WS) symbols_
    in
    case symbols of
        M :: rest ->
            List.head (List.reverse rest) == Just M

        C :: rest ->
            List.head (List.reverse rest) == Just C

        L :: ST :: rest ->
            case List.head (List.reverse rest) of
                Just R ->
                    hasReducibleArgs (dropLast rest)

                _ ->
                    False

        _ ->
            False


hasReducibleArgs : List Symbol -> Bool
hasReducibleArgs symbols =
    case symbols of
        [] ->
            True

        L :: _ ->
            reducibleAux symbols

        C :: _ ->
            reducibleAux symbols

        M :: _ ->
            let
                seg =
                    getSegment M symbols
            in
            if isReducible seg then
                hasReducibleArgs (List.drop (List.length seg) symbols)

            else
                False

        BM :: rest ->
            hasReducibleArgs rest

        ST :: rest ->
            hasReducibleArgs rest

        _ ->
            False


split : List Symbol -> Maybe ( List Symbol, List Symbol )
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


dropLast : List a -> List a
dropLast list =
    let
        n =
            List.length list
    in
    List.take (n - 1) list


{-|

> deleteAt 1 [0, 1, 2]

     [0,2] : List number

-}
deleteAt : Int -> List a -> List a
deleteAt k list =
    List.take k list ++ List.drop (k + 1) list


{-|

    > splitAt 2 [0, 1, 2, 3, 4]
      ([0,1],[3,4])

-}
splitAt : Int -> List a -> ( List a, List a )
splitAt k list =
    ( List.take k list, List.drop k list )


type alias State =
    { symbols : List Symbol, index : Int, brackets : Int }


getSegment : Symbol -> List Symbol -> List Symbol
getSegment sym symbols =
    let
        seg_ =
            List.Extra.takeWhile (\sym_ -> sym_ /= sym) (List.drop 1 symbols)

        n =
            List.length seg_
    in
    case List.Extra.getAt (n + 1) symbols of
        Nothing ->
            sym :: seg_

        Just last ->
            sym :: seg_ ++ [ last ]


match : List Symbol -> Maybe Int
match symbols =
    case List.head symbols of
        Nothing ->
            Nothing

        Just symbol ->
            if List.member symbol [ C, M ] then
                Just (List.length (getSegment symbol symbols) - 1)

            else if value symbol < 0 then
                Nothing

            else
                loop { symbols = List.drop 1 symbols, index = 1, brackets = value symbol } nextStep


nextStep : State -> Step State (Maybe Int)
nextStep state =
    case List.head state.symbols of
        Nothing ->
            Done Nothing

        Just sym ->
            let
                brackets =
                    state.brackets + value sym
            in
            if brackets < 0 then
                Done Nothing

            else if brackets == 0 then
                Done (Just state.index)

            else
                Loop { symbols = List.drop 1 state.symbols, index = state.index + 1, brackets = brackets }
