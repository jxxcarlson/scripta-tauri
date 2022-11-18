module Parser.Line exposing
    ( Line
    , PrimitiveBlockType(..)
    , classify
    , getBlockType
    , getNameAndArgs
    , isEmpty
    , isNonEmptyBlank
    , prefixLength
    , prefixLengths
    , showBlockType
    )

import L0.Parser.Line
import MicroLaTeX.Parser.Line
import Parser exposing ((|.), (|=), Parser)
import Scripta.Language exposing (Language(..))
import XMarkdown.Line


{-|

    - ident:      the number of blanks before the first non-blank
    - prefix:     the string of blanks preceding the first non-blank
    - content:    the original string with the prefix removed
    - lineNumber: the line number in the source text
    - position:   the position of the first character of the line in the source text

-}
type alias Line =
    { indent : Int, prefix : String, content : String, lineNumber : Int, position : Int }


type PrimitiveBlockType
    = PBVerbatim
    | PBOrdinary
    | PBParagraph


showBlockType : PrimitiveBlockType -> String
showBlockType blockType =
    case blockType of
        PBVerbatim ->
            "Verbatim"

        PBOrdinary ->
            "Ordinary"

        PBParagraph ->
            "Paragraph"


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


getBlockType : Language -> String -> PrimitiveBlockType
getBlockType lang line_ =
    let
        line =
            String.trim line_
    in
    case lang of
        L0Lang ->
            if String.left 2 line == "||" then
                PBVerbatim

            else if String.left 2 line == "$$" then
                PBVerbatim

            else if
                String.left 1 line
                    == "|"
            then
                PBOrdinary

            else
                PBParagraph

        MicroLaTeXLang ->
            -- Note the source text has already been partially transformed to conform to L0
            if String.left 2 line == "||" then
                PBVerbatim

            else if String.left 2 line == "$$" then
                PBVerbatim

            else if
                String.left 1 line
                    == "|"
            then
                PBOrdinary

            else
                PBParagraph

        PlainTextLang ->
            PBParagraph

        XMarkdownLang ->
            if String.left 3 line == "```" then
                PBVerbatim

            else if String.left 3 line == "|| " then
                PBVerbatim

            else if String.left 2 line == "$$" then
                PBVerbatim

            else if String.left 2 line == "| " then
                PBOrdinary

            else
                PBParagraph


getNameAndArgs : Language -> Line -> ( Maybe String, List String )
getNameAndArgs lang line =
    case lang of
        MicroLaTeXLang ->
            MicroLaTeX.Parser.Line.getNameAndArgs line

        L0Lang ->
            L0.Parser.Line.getNameAndArgs line

        PlainTextLang ->
            ( Nothing, [] )

        XMarkdownLang ->
            XMarkdown.Line.getNameAndArgs line


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
