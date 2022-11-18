module Compiler.Util exposing
    ( compressWhitespace
    , depth
    , dropLast
    , eraseItem
    , getBracketedItem
    , getBracketedItems
    , getItem
    , getMarkdownImageArgs
    , getMicroLaTeXItem
    , macroValParser
    , macroValParserX
    , many
    , middle
    , normalizedWord
    , removeNonAlphaNum
    , size
    , transformLabel
    )

import Parser exposing ((|.), (|=), Parser, Step(..), loop, map, oneOf, spaces, succeed)
import Regex
import Scripta.Language exposing (Language(..))
import Tree exposing (Tree)


normalizedWord : List String -> String
normalizedWord words =
    words
        |> List.map
            (String.toLower
                -->> compressWhitespace
                >> removeNonAlphaNum
            )
        -- >> String.replace " " "-")
        |> String.join "-"


dropLast : List a -> List a
dropLast list =
    let
        n =
            List.length list
    in
    List.take (n - 1) list


middle : List a -> List a
middle list =
    list |> List.drop 1 |> dropLast


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


transformLabel : String -> String
transformLabel str =
    let
        normalize m =
            m |> List.map (Maybe.withDefault "") |> String.join "" |> String.trim
    in
    userReplace "\\[label(.*)\\]" (\m -> "\\label{" ++ (m.submatches |> normalize) ++ "}") str


compressWhitespace : String -> String
compressWhitespace string =
    userReplace "\\s\\s+" (\m -> " ") string


removeNonAlphaNum : String -> String
removeNonAlphaNum string =
    userReplace "[^a-zA-Z0-9 ]" (\_ -> "") string


{-| Apply a parser zero or more times and return a list of the results.
-}
many : Parser a -> Parser (List a)
many p =
    loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Step (List a) (List a))
manyHelp p vs =
    oneOf
        [ succeed (\v -> Loop (v :: vs))
            |= p
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse vs))
        ]


depth : Tree a -> Int
depth t =
    let
        c =
            Tree.children t
    in
    if c == [] then
        0

    else
        1 + maxiumumPositiveInteger (List.map depth c)


maxiumumPositiveInteger : List Int -> Int
maxiumumPositiveInteger ints =
    List.foldl (\i acc -> max i acc) 0 ints


size : Tree a -> Int
size t =
    let
        c =
            Tree.children t
    in
    if c == [] then
        1

    else
        1 + List.sum (List.map size c)


{-|

    > getItem MicroLaTeXLang "foo" "... whatever ... \\foo{bar} ... whatever else ..."
    "bar" : String

    > getItem L0Lang "foo" "... whatever ... [foo bar] ... whatever else ..."
    "bar" : String

-}
getItem : Language -> String -> String -> String
getItem language key str =
    -- TODO: fix this
    case language of
        L0Lang ->
            runParser (keyValParser key) str ""

        MicroLaTeXLang ->
            runParser (macroValParser key) str ""

        PlainTextLang ->
            runParser (keyValParser key) str ""

        XMarkdownLang ->
            runParser (keyValParser key) str ""


getMicroLaTeXItem : String -> String -> Maybe String
getMicroLaTeXItem key str =
    case Parser.run (macroValParser key) str of
        Ok val ->
            Just val

        Err _ ->
            Nothing


{-|

    > getBracketedItems "ho ho ho! [foo] [bar]"
    ["foo","bar"] : List String

-}
getBracketedItems : String -> List String
getBracketedItems str =
    case Parser.run (many bracketedItemParser) str of
        Ok val ->
            val

        Err _ ->
            []


getBracketedItem : String -> Maybe String
getBracketedItem str =
    case Parser.run bracketedItemParser str of
        Ok val ->
            Just val

        Err _ ->
            Nothing


{-|

    > eraseItem MicroLaTeXLang "foo" "bar" "... whatever\\foo{bar}\n, whatever else ..."
    "... whatever, whatever else ..." : String

    > eraseItem L0Lang "foo" "bar" "... whateve[foo bar]\n, whatever else ..."
    "... whatever, whatever else ..." : String

-}
eraseItem : Language -> String -> String -> String -> String
eraseItem language key value str =
    case language of
        L0Lang ->
            let
                target =
                    "[" ++ key ++ " " ++ value ++ "]\n"
            in
            String.replace target "" str

        PlainTextLang ->
            str

        MicroLaTeXLang ->
            let
                target =
                    "\\" ++ key ++ "{" ++ value ++ "}\n"
            in
            String.replace target "" str

        XMarkdownLang ->
            -- TODO: implement this
            "((unimplemented))"


runParser stringParser str default =
    case Parser.run stringParser str of
        Ok s ->
            s

        Err _ ->
            default


{-|

    > Parser.run macroValParser "... whatever ... \\foo{bar} ... whatever else ..."
    Ok "bar"

-}
macroValParser : String -> Parser String
macroValParser macroName =
    (Parser.succeed String.slice
        |. Parser.chompUntil ("\\" ++ macroName ++ "{")
        |. Parser.symbol ("\\" ++ macroName ++ "{")
        |. Parser.spaces
        |= Parser.getOffset
        |. Parser.chompUntilEndOr "}"
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map String.trim


macroValParserX : String -> Parser String
macroValParserX macroName =
    (Parser.succeed String.slice
        |. Parser.chompUntil ("\\" ++ macroName ++ "{")
        |. Parser.symbol ("\\" ++ macroName ++ "{")
        |. Parser.spaces
        |= Parser.getOffset
        |. Parser.chompUntilEndOr "!!!!"
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map String.trim


getMarkdownImageArgs str =
    case Parser.run markdownImageParser str of
        Ok result ->
            Just result

        Err _ ->
            Nothing


markdownImageParser : Parser ( String, String )
markdownImageParser =
    bracketedItemParser |> Parser.andThen (\a -> parenthesizedItemParser |> Parser.map (\b -> ( a, b )))


bracketedItemParser : Parser String
bracketedItemParser =
    itemParser "[" "]"


parenthesizedItemParser : Parser String
parenthesizedItemParser =
    itemParser "(" ")"


itemParser : String -> String -> Parser String
itemParser leftDelimiter rightDelimiter =
    (Parser.succeed String.slice
        |. Parser.chompUntil leftDelimiter
        |. Parser.symbol leftDelimiter
        |. Parser.spaces
        |= Parser.getOffset
        |. Parser.chompUntil rightDelimiter
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map String.trim


{-|

    > Parser.run macroValParser "... whatever ... \\foo{bar} ... whatever else ..."
    Ok "bar"

-}
keyValParser : String -> Parser String
keyValParser key =
    (Parser.succeed String.slice
        |. Parser.chompUntil ("[" ++ key ++ " ")
        |. Parser.symbol ("[" ++ key ++ " ")
        |. Parser.spaces
        |= Parser.getOffset
        |. Parser.chompUntil "]"
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map String.trim
