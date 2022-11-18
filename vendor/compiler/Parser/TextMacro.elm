module Parser.TextMacro exposing (MyMacro(..), eraseLeadingMacro, get, toString)

import Parser exposing ((|.), (|=), Parser)
import Set


foo =
    1


type MyMacro
    = MyMacro String (List String)


toString : MyMacro -> String
toString (MyMacro name args) =
    "\\" ++ name ++ (List.map (\a -> "{" ++ a ++ "}") args |> String.join "")


eraseLeadingMacro : String -> String -> String
eraseLeadingMacro name str =
    case Parser.run (argsOfNamedMacro name) str of
        Ok args ->
            String.replace (toString (MyMacro name args)) "" str

        Err _ ->
            str


get : String -> Result (List Parser.DeadEnd) MyMacro
get str =
    Parser.run macro str


macro =
    Parser.succeed MyMacro
        |= macroName
        |= itemList arg


argsOfNamedMacro : String -> Parser (List String)
argsOfNamedMacro name =
    Parser.succeed identity
        |. Parser.spaces
        |. Parser.symbol ("\\" ++ name)
        |= itemList arg


arg : Parser String
arg =
    Parser.succeed identity
        |. Parser.symbol "{"
        |. Parser.spaces
        -- = word (\c -> c /= ' ' && c /= '}')
        |= word (\c -> c /= '}')
        |. Parser.symbol "}"


{-| Use `inWord` to parse a word.

import Parser

inWord : Char -> Bool
inWord c = not (c == ' ')

MXParser.run word "this is a test"
--> Ok "this"

-}
word : (Char -> Bool) -> Parser String
word inWord =
    Parser.succeed String.slice
        |. Parser.spaces
        |= Parser.getOffset
        |. Parser.chompIf inWord
        |. Parser.chompWhile inWord
        |. Parser.spaces
        |= Parser.getOffset
        |= Parser.getSource


itemList : Parser a -> Parser (List a)
itemList itemParser =
    itemList_ [] itemParser


itemList_ : List a -> Parser a -> Parser (List a)
itemList_ initialList itemParser =
    Parser.loop initialList (itemListHelper itemParser)


itemListHelper : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
itemListHelper itemParser revItems =
    Parser.oneOf
        [ Parser.succeed (\item_ -> Parser.Loop (item_ :: revItems))
            |= itemParser
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revItems))
        ]


macroName : Parser String
macroName =
    Parser.variable
        { start = \c -> c == '\\'
        , inner = \c -> Char.isAlphaNum c
        , reserved = Set.fromList []
        }
        |> Parser.map (String.dropLeft 1)
