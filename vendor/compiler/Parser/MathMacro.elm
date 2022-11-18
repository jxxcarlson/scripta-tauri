module Parser.MathMacro exposing
    ( MathMacroDict
    , evalStr
    , makeMacroDict
    , parse
    , parseMany
    , parseNewCommand
    , print
    , printList
    , printNewCommand
    )

import Dict exposing (Dict)
import Maybe.Extra
import Parser.Advanced as PA
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Step(..)
        , Token(..)
        , backtrackable
        , chompIf
        , chompWhile
        , getOffset
        , getSource
        , lazy
        , loop
        , map
        , oneOf
        , run
        , succeed
        , symbol
        )
import Result.Extra



-- TYPES


type NewCommand
    = NewCommand MathExpr Int (List MathExpr)


type MacroBody
    = MacroBody Int (List MathExpr)


evalStr : MathMacroDict -> String -> String
evalStr dict str =
    case parseMany (String.trim str) of
        Ok result ->
            let
                _ =
                    result
            in
            List.map (expandMacroWithDict dict) result |> printList

        Err _ ->
            -- the intent of evalStr is to expand macros.  So if something
            -- goes wrong with the process, just return the input string.
            -- TODO: This solves the problem of false error reporting, but I don't like the solution.
            str


parseMany : String -> Result (List (DeadEnd Context Problem)) (List MathExpr)
parseMany str =
    str
        |> String.trim
        |> String.lines
        |> List.map String.trim
        |> List.map parse
        |> Result.Extra.combine
        |> Result.map List.concat


type MathExpr
    = AlphaNum String
    | F0 String
    | Arg (List MathExpr)
    | Sub Deco
    | Super Deco
    | Param Int
    | WS
    | MathSpace
    | MathSmallSpace
    | MathMediumSpace
    | LeftMathBrace
    | RightMathBrace
    | MathSymbols String
    | Macro String (List MathExpr)
    | Expr (List MathExpr)


type Deco
    = DecoM MathExpr
    | DecoI Int



-- RESUlT: [Macro "frac" [Arg [Macro "baar" [Arg [AlphaNum "X"]]],Arg [Macro "baar" [Arg [AlphaNum "Y"]]]]]


expandMacroWithDict : MathMacroDict -> MathExpr -> MathExpr
expandMacroWithDict dict expr =
    case expr of
        Macro macroName args ->
            case Dict.get macroName dict of
                Nothing ->
                    Macro macroName (List.map (expandMacroWithDict dict) args)

                Just (MacroBody k exprs) ->
                    Expr (expandMacro_ (List.map (expandMacroWithDict dict) args) (MacroBody k (List.map (expandMacroWithDict dict) exprs)))

        Arg exprs ->
            Arg (List.map (expandMacroWithDict dict) exprs)

        Sub decoExpr ->
            case decoExpr of
                DecoM decoMExpr ->
                    Sub (DecoM (expandMacroWithDict dict decoMExpr))

                DecoI m ->
                    Sub (DecoI m)

        Super decoExpr ->
            case decoExpr of
                DecoM decoMExpr ->
                    Super (DecoM (expandMacroWithDict dict decoMExpr))

                DecoI m ->
                    Super (DecoI m)

        _ ->
            expr


{-|

    > args = [Exprs [AlphaNum "x"],Exprs [AlphaNum "y"]]
    > macroDefBody = (MacroBody 2 [Macro "alpha" [],MathSymbols "(",Param 1,MathSymbols ",",Param 2,MathSymbols ")"])
    > expandMacro_  args macroDefBody
    [Macro "alpha" [],MathSymbols "(",Exprs [AlphaNum "x"],MathSymbols ",",Exprs [AlphaNum "y"],MathSymbols ")"]

-}
expandMacro_ : List MathExpr -> MacroBody -> List MathExpr
expandMacro_ args (MacroBody arity macroDefBody) =
    replaceParams args macroDefBody


type alias MathMacroDict =
    Dict String MacroBody


replaceParam_ : Int -> MathExpr -> MathExpr -> MathExpr
replaceParam_ k expr target =
    case target of
        Arg exprs ->
            Arg (List.map (replaceParam_ k expr) exprs)

        Sub decoExpr ->
            case decoExpr of
                DecoM decoMExpr ->
                    Sub (DecoM (replaceParam_ k expr decoMExpr))

                DecoI m ->
                    Sub (DecoI m)

        Super decoExpr ->
            case decoExpr of
                DecoM decoMExpr ->
                    Super (DecoM (replaceParam_ k expr decoMExpr))

                DecoI m ->
                    Super (DecoI m)

        Param m ->
            if m == k then
                expr

            else
                Param m

        Macro name exprs ->
            Macro name (List.map (replaceParam_ k expr) exprs)

        _ ->
            target


replaceParam : Int -> MathExpr -> List MathExpr -> List MathExpr
replaceParam k expr exprs =
    List.map (replaceParam_ k expr) exprs


replaceParams : List MathExpr -> List MathExpr -> List MathExpr
replaceParams replacementList target =
    List.foldl (\( k, replacement ) acc -> replaceParam (k + 1) replacement acc) target (List.indexedMap (\k item -> ( k, item )) replacementList)


makeMacroDict : String -> Dict String MacroBody
makeMacroDict str =
    str
        |> String.trim
        |> String.lines
        |> List.map (parseNewCommand >> makeEntry)
        |> Maybe.Extra.values
        |> Dict.fromList


makeEntry : Result error NewCommand -> Maybe ( String, MacroBody )
makeEntry newCommand_ =
    case newCommand_ of
        Ok (NewCommand (F0 name) arity [ Arg body ]) ->
            Just ( name, MacroBody arity body )

        _ ->
            Nothing


type Context
    = CArg String


type Problem
    = ExpectingLeftBrace
    | ExpectingAlpha
    | ExpectingNotAlpha
    | ExpectingInt
    | InvalidNumber
    | ExpectingMathSmallSpace
    | ExpectingMathMediumSpace
    | ExpectingLeftBracket
    | ExpectingMathSpace
    | ExpectingRightBracket
    | ExpectingLeftMathBrace
    | ExpectingRightMathBrace
    | ExpectingUnderscore
    | ExpectingCaret
    | ExpectingSpace
    | ExpectingRightBrace
    | ExpectingHash
    | ExpectingBackslash
    | ExpectingNewCommand


type alias MathExprParser a =
    PA.Parser Context Problem a



-- PARSER


parse : String -> Result (List (DeadEnd Context Problem)) (List MathExpr)
parse str =
    PA.run (many mathExprParser) str


macroParser =
    succeed Macro
        |. symbol (Token "\\" ExpectingBackslash)
        |= alphaNumParser_
        |= many argParser


mathExprParser =
    oneOf
        [ mathMediumSpaceParser
        , mathSmallSpaceParser
        , mathSpaceParser
        , leftBraceParser
        , rightBraceParser
        , macroParser
        , mathSymbolsParser
        , lazy (\_ -> argParser)
        , paramParser
        , whitespaceParser
        , alphaNumParser
        , f0Parser
        , subscriptParser
        , superscriptParser
        ]


mathSymbolsParser =
    (succeed String.slice
        |= getOffset
        |. chompIf (\c -> not (Char.isAlpha c) && not (List.member c [ '_', '^', '#', '\\', '{', '}' ])) ExpectingNotAlpha
        |. chompWhile (\c -> not (Char.isAlpha c) && not (List.member c [ '_', '^', '#', '\\', '{', '}' ]))
        |= getOffset
        |= getSource
    )
        |> PA.map MathSymbols


optionalParamParser =
    succeed identity
        |. symbol (Token "[" ExpectingLeftBracket)
        |= PA.int ExpectingInt InvalidNumber
        |. symbol (Token "]" ExpectingRightBracket)


parseNewCommand : String -> Result (List (DeadEnd Context Problem)) NewCommand
parseNewCommand str =
    run newCommandParser str


newCommandParser =
    oneOf [ backtrackable newCommandParser1, newCommandParser2 ]


mathSpaceParser : PA.Parser c Problem MathExpr
mathSpaceParser =
    succeed MathSpace
        |. symbol (Token "\\ " ExpectingMathSpace)


mathSmallSpaceParser : PA.Parser c Problem MathExpr
mathSmallSpaceParser =
    succeed MathSmallSpace
        |. symbol (Token "\\," ExpectingMathSmallSpace)


mathMediumSpaceParser : PA.Parser c Problem MathExpr
mathMediumSpaceParser =
    succeed MathMediumSpace
        |. symbol (Token "\\;" ExpectingMathMediumSpace)


leftBraceParser : PA.Parser c Problem MathExpr
leftBraceParser =
    succeed LeftMathBrace
        |. symbol (Token "\\{" ExpectingLeftMathBrace)


rightBraceParser : PA.Parser c Problem MathExpr
rightBraceParser =
    succeed RightMathBrace
        |. symbol (Token "\\}" ExpectingRightMathBrace)


newCommandParser1 : PA.Parser Context Problem NewCommand
newCommandParser1 =
    succeed (\name arity body -> NewCommand name arity body)
        |. symbol (Token "\\newcommand" ExpectingNewCommand)
        |. symbol (Token "{" ExpectingLeftBrace)
        |= f0Parser
        |. symbol (Token "}" ExpectingRightBrace)
        |= optionalParamParser
        |= many mathExprParser


newCommandParser2 =
    succeed (\name body -> NewCommand name 0 body)
        |. symbol (Token "\\newcommand" ExpectingNewCommand)
        |. symbol (Token "{" ExpectingLeftBrace)
        |= f0Parser
        |. symbol (Token "}" ExpectingRightBrace)
        |= many mathExprParser


argParser : PA.Parser Context Problem MathExpr
argParser =
    (succeed identity
        |. symbol (Token "{" ExpectingLeftBrace)
        |= lazy (\_ -> many mathExprParser)
    )
        |. symbol (Token "}" ExpectingRightBrace)
        |> PA.map Arg


whitespaceParser =
    symbol (Token " " ExpectingSpace) |> PA.map (\_ -> WS)


alphaNumParser : PA.Parser c Problem MathExpr
alphaNumParser =
    alphaNumParser_ |> PA.map AlphaNum


alphaNumParser_ : PA.Parser c Problem String
alphaNumParser_ =
    succeed String.slice
        |= getOffset
        |. chompIf Char.isAlpha ExpectingAlpha
        |. chompWhile Char.isAlphaNum
        |= getOffset
        |= getSource


f0Parser : PA.Parser Context Problem MathExpr
f0Parser =
    second (symbol (Token "\\" ExpectingBackslash)) alphaNumParser_
        |> PA.map F0


paramParser =
    (succeed identity
        |. symbol (Token "#" ExpectingHash)
        |= PA.int ExpectingInt InvalidNumber
    )
        |> PA.map Param


subscriptParser =
    (succeed identity
        |. symbol (Token "_" ExpectingUnderscore)
        |= decoParser
    )
        |> PA.map Sub


superscriptParser =
    (succeed identity
        |. symbol (Token "^" ExpectingCaret)
        |= decoParser
    )
        |> PA.map Super


decoParser =
    oneOf [ numericDecoParser, lazy (\_ -> mathExprParser) |> PA.map DecoM ]


numericDecoParser =
    PA.int ExpectingInt InvalidNumber |> PA.map DecoI



-- PRINT


printNewCommand (NewCommand mathExpr arity body) =
    if arity == 0 then
        "\\newcommand" ++ enclose (print mathExpr) ++ printList body

    else
        "\\newcommand" ++ enclose (print mathExpr) ++ "[" ++ String.fromInt arity ++ "]" ++ printList body


printList : List MathExpr -> String
printList exprs =
    List.map print exprs |> String.join ""


print : MathExpr -> String
print expr =
    case expr of
        AlphaNum str ->
            str

        LeftMathBrace ->
            "\\{"

        RightMathBrace ->
            "\\}"

        MathSmallSpace ->
            "\\,"

        MathMediumSpace ->
            "\\;"

        MathSpace ->
            "\\ "

        F0 str ->
            "\\" ++ str

        Param k ->
            "#" ++ String.fromInt k

        Arg exprs ->
            enclose (printList exprs)

        Sub deco ->
            -- "_" ++ enclose (printDeco deco)
            "_" ++ printDeco deco

        Super deco ->
            -- "^" ++ enclose (printDeco deco)
            "^" ++ printDeco deco

        MathSymbols str ->
            str

        WS ->
            " "

        Macro name body ->
            "\\" ++ name ++ printList body

        Expr exprs ->
            List.map print exprs |> String.join ""


printDeco : Deco -> String
printDeco deco =
    case deco of
        DecoM expr ->
            print expr

        DecoI k ->
            String.fromInt k



-- HELPERS


second : MathExprParser a -> MathExprParser b -> MathExprParser b
second p q =
    p |> PA.andThen (\_ -> q)


{-| Apply a parser zero or more times and return a list of the results.
-}
many : MathExprParser a -> MathExprParser (List a)
many p =
    loop [] (manyHelp p)


manyHelp : MathExprParser a -> List a -> MathExprParser (Step (List a) (List a))
manyHelp p vs =
    oneOf
        [ succeed (\v -> Loop (v :: vs))
            |= p

        -- |. PA.spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse vs))
        ]


enclose : String -> String
enclose str =
    "{" ++ str ++ "}"
