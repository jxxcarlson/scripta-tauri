module XMarkdown.Symbol exposing (Symbol(..), balance, convertTokens, convertTokens2, symbolToStringLiteral, toString, value)

import Maybe.Extra
import XMarkdown.Token exposing (Token(..))


type Symbol
    = LBracket
    | RBracket
    | LParen
    | RParen
    | SBold
    | SItalic
    | SImage
    | SAT
    | O
    | M
    | C


value : Symbol -> Int
value symbol =
    case symbol of
        LBracket ->
            1

        RBracket ->
            -1

        LParen ->
            1

        RParen ->
            -1

        SBold ->
            0

        SItalic ->
            0

        SImage ->
            1

        SAT ->
            1

        O ->
            0

        M ->
            0

        C ->
            0


balance : List Symbol -> Int
balance symbols =
    symbols |> List.map value |> List.sum


symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        LBracket ->
            "LBracket"

        RBracket ->
            "RBracket"

        LParen ->
            "LParen"

        RParen ->
            "RParen"

        SBold ->
            "SBold"

        SItalic ->
            "SItalic"

        SImage ->
            "SImage"

        SAT ->
            "SAT"

        O ->
            "O"

        M ->
            "M"

        C ->
            "C"


symbolToStringLiteral : Symbol -> String
symbolToStringLiteral symbol =
    case symbol of
        LBracket ->
            "["

        RBracket ->
            "]"

        LParen ->
            "("

        RParen ->
            ")"

        SBold ->
            "*"

        SItalic ->
            "_"

        SImage ->
            "image"

        SAT ->
            "@"

        O ->
            "O"

        M ->
            "$"

        C ->
            "`"


toString : List Symbol -> String
toString symbols =
    List.map symbolToString symbols |> String.join " "


convertTokens : List Token -> List Symbol
convertTokens tokens =
    List.map toSymbol tokens |> Maybe.Extra.values


convertTokens2 : List Token -> List Symbol
convertTokens2 tokens =
    List.map toSymbol2 tokens


toSymbol : Token -> Maybe Symbol
toSymbol token =
    case token of
        LB _ ->
            Just LBracket

        RB _ ->
            Just RBracket

        LP _ ->
            Just LParen

        Bold _ ->
            Just SBold

        Italic _ ->
            Just SItalic

        Image _ ->
            Just SImage

        AT _ ->
            Just SAT

        RP _ ->
            Just RParen

        MathToken _ ->
            Just M

        CodeToken _ ->
            Just C

        _ ->
            Nothing


toSymbol2 : Token -> Symbol
toSymbol2 token =
    case token of
        LB _ ->
            LBracket

        RB _ ->
            RBracket

        MathToken _ ->
            M

        CodeToken _ ->
            C

        _ ->
            O
