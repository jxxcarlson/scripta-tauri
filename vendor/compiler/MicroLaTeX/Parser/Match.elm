module MicroLaTeX.Parser.Match exposing (deleteAt, getSegment, hasReducibleArgs, match, reducible, split, splitAt)

import Compiler.Util
import List.Extra
import MicroLaTeX.Parser.Symbol exposing (Symbol(..), value)
import Parser.Helpers exposing (Step(..), loop)


reducible : List Symbol -> Bool
reducible symbols =
    case symbols of
        --LM :: rest ->
        --    List.Extra.last rest == Just RM
        LM :: St :: RM :: [] ->
            True

        M :: rest ->
            List.head (List.reverse rest) == Just M

        C :: rest ->
            List.head (List.reverse rest) == Just C

        B :: St :: rest ->
            case List.Extra.last rest of
                Just R ->
                    hasReducibleArgs rest

                _ ->
                    False

        _ ->
            False


hasReducibleArgs : List Symbol -> Bool
hasReducibleArgs symbols =
    case symbols of
        [] ->
            True

        LM :: St :: RM :: _ ->
            case split symbols of
                Nothing ->
                    False

                Just ( a, b ) ->
                    hasReducibleArgs (Compiler.Util.middle a) && hasReducibleArgs b

        L :: _ ->
            --if List.Extra.last rest == Just R then
            case split symbols of
                Nothing ->
                    False

                Just ( a, b ) ->
                    hasReducibleArgs (Compiler.Util.middle a) && hasReducibleArgs b

        --else
        --    False
        C :: _ ->
            reducibleAux symbols

        M :: _ ->
            let
                seg =
                    getSegment M symbols
            in
            if reducible seg then
                hasReducibleArgs (List.drop (List.length seg) symbols)

            else
                False

        B :: rest ->
            hasReducibleArgs rest

        St :: rest ->
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
            reducible a && hasReducibleArgs b


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
