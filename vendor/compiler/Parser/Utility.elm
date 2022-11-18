module Parser.Utility exposing (getKeyedParagraph, getLeadingBlanks, l0TitleParser, leadingBlanks, microLaTeXTitleParser, parseItem, parseTitle)

import Parser exposing ((|.), (|=), Parser)
import Scripta.Language exposing (Language(..))


getLeadingBlanks : String -> String
getLeadingBlanks str =
    case Parser.run leadingBlanks str of
        Err _ ->
            ""

        Ok s ->
            s


leadingBlanks : Parser String
leadingBlanks =
    Parser.succeed (\start end src -> String.slice start end src)
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c == ' ')
        |= Parser.getOffset
        |= Parser.getSource


{-| If the text is

    abc
    def

    Vacation:
    sun
    sea
    good food

    ho ho ho!

then 'run (keyedParagraphParser "Vacation:") theText'
will return

    Vacation:
    sun
    sea
    good food

-}
keyedParagraphParser : String -> Parser String
keyedParagraphParser headline =
    Parser.succeed (\start end src -> String.slice start end src)
        |. Parser.chompUntil headline
        |= Parser.getOffset
        |. Parser.chompUntil "\n\n"
        |= Parser.getOffset
        |= Parser.getSource


getKeyedParagraph : String -> String -> Maybe String
getKeyedParagraph headline target =
    case Parser.run (keyedParagraphParser headline) target of
        Err _ ->
            Nothing

        Ok data ->
            Just data


l0TitleParser : Parser String
l0TitleParser =
    Parser.succeed (\start end src -> String.slice start end src |> String.dropLeft 8 |> String.trimRight)
        |. Parser.chompUntil "| title "
        |= Parser.getOffset
        |. Parser.chompUntil "\n"
        |= Parser.getOffset
        |= Parser.getSource


itemParser : String -> Parser String
itemParser item =
    Parser.succeed (\start end src -> String.slice start end src)
        |. Parser.chompUntil (item ++ "=")
        |. Parser.symbol (item ++ "=\"")
        |= Parser.getOffset
        |. Parser.chompUntil "\""
        |= Parser.getOffset
        |= Parser.getSource


{-|

    > str = """<iframe src="https://www.desmos.com/calculator/ycaswggsgb?embed" width="500" height="500" style="border: 1px solid #ccc" frameborder=0></iframe>"""
    > parseItem "src" str
      Just  "https://www.desmos.com/calculator/ycaswggsgb?embed"

-}
parseItem : String -> String -> Maybe String
parseItem item str =
    case Parser.run (itemParser item) str of
        Ok output ->
            Just output

        Err _ ->
            Nothing


microLaTeXTitleParser : Parser String
microLaTeXTitleParser =
    Parser.succeed (\start end src -> String.slice start end src |> String.dropLeft 7)
        |. Parser.chompUntil "\\title{"
        |= Parser.getOffset
        |. Parser.chompUntil "}"
        |= Parser.getOffset
        |= Parser.getSource


parseTitle : Language -> String -> Maybe String
parseTitle lang src =
    case lang of
        L0Lang ->
            case Parser.run l0TitleParser src of
                Ok title ->
                    Just title

                Err _ ->
                    Nothing

        MicroLaTeXLang ->
            case Parser.run microLaTeXTitleParser src of
                Ok title ->
                    Just title

                Err _ ->
                    Nothing

        _ ->
            Nothing
