module Compiler.TextMacroParser exposing (getParam)

import Parser.Advanced as PA
    exposing
        ( (|.)
        , (|=)
        , Token(..)
        , chompUntil
        , run
        , succeed
        , symbol
        )


type Context
    = CArg String


type Problem
    = ExpectingInt
    | InvalidNumber
    | ExpectingHash


paramParser2 : PA.Parser String Problem Int
paramParser2 =
    succeed identity
        |. chompUntil (Token "#" ExpectingHash)
        |. symbol (Token "#" ExpectingHash)
        |= PA.int ExpectingInt InvalidNumber


getParam : String -> Maybe String
getParam str =
    case run paramParser2 str of
        Ok n ->
            Just ("#" ++ String.fromInt n)

        Err _ ->
            Nothing
