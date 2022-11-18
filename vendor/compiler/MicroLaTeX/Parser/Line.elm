module MicroLaTeX.Parser.Line exposing
    ( Line
    , classify
    , getNameAndArgs
    , getNameAndArgs2
    , isEmpty
    , isNonEmptyBlank
    , prefixLength
    , prefixLengths
    )

import Compiler.Util
import Parser exposing ((|.), (|=), Parser)


{-|

    - ident:      the number of blanks before the first non-blank
    - prefix:     the string of blanks preceding the first non-blank
    - content:    the original string with the prefix removed
    - lineNumber: the line number in the source text
    - position:   the position of the first character of the line in the source text

-}
type alias Line =
    { indent : Int, prefix : String, content : String, lineNumber : Int, position : Int }


isEmpty : Line -> Bool
isEmpty line =
    line.indent == 0 && line.content == ""


isNonEmptyBlank : Line -> Bool
isNonEmptyBlank line =
    line.indent > 0 && line.content == ""


classify : Int -> Int -> String -> Line
classify position lineNumber str =
    case Parser.run (prefixParser position lineNumber) str of
        Err _ ->
            { indent = 0, content = "!!ERROR", prefix = "", position = position, lineNumber = lineNumber }

        Ok result ->
            result


prefixLength : Int -> Int -> String -> Int
prefixLength position lineNumber str =
    classify position lineNumber str |> .indent


prefixLengths : Int -> Int -> List String -> List Int
prefixLengths position lineNumber strs =
    strs |> List.map (prefixLength position lineNumber) |> List.filter (\n -> n /= 0)


{-|

    The prefix is the first word of the line

-}
prefixParser : Int -> Int -> Parser Line
prefixParser position lineNumber =
    Parser.succeed (\prefixStart prefixEnd lineEnd content -> { indent = prefixEnd - prefixStart, prefix = String.slice 0 prefixEnd content, content = String.slice prefixEnd lineEnd content, position = position, lineNumber = lineNumber })
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c == ' ')
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= '\n')
        |= Parser.getOffset
        |= Parser.getSource


getNameAndArgs line =
    let
        normalizedLine =
            String.trim line.content

        name =
            case Compiler.Util.getMicroLaTeXItem "begin" normalizedLine of
                Just str ->
                    Just str

                Nothing ->
                    if normalizedLine == "$$" then
                        Just "math"

                    else
                        Nothing
    in
    ( name, Compiler.Util.getBracketedItems normalizedLine )


getNameAndArgs2 line =
    let
        normalizedLine =
            String.trim line.content

        name =
            case Compiler.Util.getMicroLaTeXItem "begin" normalizedLine of
                Just str ->
                    Just str

                Nothing ->
                    if normalizedLine == "$$" then
                        Just "math"

                    else
                        Nothing
    in
    ( name, Compiler.Util.getBracketedItem normalizedLine )
