module MicroLaTeX.Parser.Symbol exposing (Symbol(..), balance, convertTokens2, toString, value)

import MicroLaTeX.Parser.Token exposing (Token(..))


type Symbol
    = B
    | L
    | R
    | St
    | M
    | LM
    | RM
    | C
    | Fn
    | Ws
    | TEs


value : Symbol -> Int
value symbol =
    case symbol of
        B ->
            0

        L ->
            1

        R ->
            -1

        St ->
            0

        M ->
            0

        LM ->
            1

        RM ->
            -1

        C ->
            0

        Fn ->
            0

        Ws ->
            0

        TEs ->
            0


balance : List Symbol -> Int
balance symbols =
    symbols |> List.map value |> List.sum


symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        B ->
            "B"

        L ->
            "L"

        R ->
            "R"

        St ->
            "O"

        M ->
            "M"

        LM ->
            "LM"

        RM ->
            "RM"

        C ->
            "C"

        Fn ->
            "F"

        Ws ->
            "W"

        TEs ->
            "TE"


toString : List Symbol -> String
toString symbols =
    List.map symbolToString symbols |> String.join " "


convertTokens2 : List Token -> List Symbol
convertTokens2 tokens =
    List.map toSymbol2 tokens


toSymbol2 : Token -> Symbol
toSymbol2 token =
    case token of
        BS _ ->
            B

        LB _ ->
            L

        RB _ ->
            R

        MathToken _ ->
            M

        LMathBracket _ ->
            LM

        RMathBracket _ ->
            RM

        CodeToken _ ->
            C

        S _ _ ->
            St

        F _ _ ->
            Fn

        W _ _ ->
            Ws

        TokenError _ _ ->
            TEs
