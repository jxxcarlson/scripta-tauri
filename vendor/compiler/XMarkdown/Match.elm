module XMarkdown.Match exposing (deleteAt, match, reducible, splitAt)

import Compiler.Util
import List.Extra
import Parser.Helpers exposing (Step(..), loop)
import XMarkdown.Symbol as Symbol exposing (Symbol(..))


reducible : List Symbol -> Bool
reducible symbols =
    case List.head symbols of
        Just M ->
            List.head (List.reverse (List.drop 1 symbols)) == Just M

        Just C ->
            List.head (List.reverse (List.drop 1 symbols)) == Just C

        Just SBold ->
            List.head (List.reverse (List.drop 1 symbols)) == Just SBold

        Just SItalic ->
            List.head (List.reverse (List.drop 1 symbols)) == Just SItalic

        Just SImage ->
            symbols == [ SImage, LBracket, RBracket, LParen, RParen ]

        Just LBracket ->
            if symbols == [ LBracket, RBracket, LParen, RParen ] then
                True

            else
                False

        Just SAT ->
            if List.length symbols > 1 then
                -- symbols == [ SAT, LBracket, RBracket ]
                reducibleAux (List.drop 1 symbols)

            else
                False

        _ ->
            reducibleF symbols


reducibleAux : List Symbol -> Bool
reducibleAux symbols =
    if List.isEmpty symbols then
        True

    else if List.head symbols == Just LBracket && List.Extra.last symbols == Just RBracket then
        reducibleAux (Compiler.Util.middle symbols)

    else
        False


reducibleF : List Symbol -> Bool
reducibleF symbols =
    symbols
        == [ LBracket, RBracket, LParen, RParen ]
        || symbols
        == [ LParen, RParen ]


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
    ( List.take k list, List.drop (k + 0) list )


type alias State =
    { start : Bool, symbols : List Symbol, index : Int, brackets : Int }


{-|

    > [L, R] |> match
    Just 1

    > [L, R, L, R] |> match
    Just 1

    > [L, L, R, R] |> match
    Just 3

-}
match : List Symbol -> Maybe Int
match symbols =
    case List.head symbols of
        Nothing ->
            Nothing

        Just symbol ->
            if Symbol.value symbol < 0 then
                Nothing

            else
                let
                    start =
                        Symbol.value symbol == 1
                in
                loop { start = start, symbols = List.drop 1 symbols, index = 1, brackets = Symbol.value symbol } nextStep


nextStep : State -> Step State (Maybe Int)
nextStep state =
    case List.head state.symbols of
        Nothing ->
            Done Nothing

        Just sym ->
            let
                start =
                    if not state.start then
                        Symbol.value sym == 1

                    else
                        state.start

                brackets =
                    state.brackets + Symbol.value sym
            in
            if brackets < 0 && state.start then
                Done Nothing

            else if brackets == 0 && state.start then
                Done (Just state.index)

            else
                Loop { start = start, symbols = List.drop 1 state.symbols, index = state.index + 1, brackets = brackets }
