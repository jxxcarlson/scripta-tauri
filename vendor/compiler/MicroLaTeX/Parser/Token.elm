module MicroLaTeX.Parser.Token exposing
    ( Token(..)
    , TokenType(..)
    , backslashParser2
    , init
    , run
    , toString
    , toString2
    , type_
    )

import Parser.Advanced as Parser exposing (DeadEnd, Parser)
import Parser.Helpers exposing (Step(..), loop)
import Parser.Meta exposing (Meta)
import Parser.Tools as PT exposing (Context, Problem)



-- TYPES


type Token
    = BS Meta
    | LB Meta
    | RB Meta
    | F String Meta
    | LMathBracket Meta
    | RMathBracket Meta
    | S String Meta
    | W String Meta
    | MathToken Meta
    | CodeToken Meta
    | TokenError (List (DeadEnd Context Problem)) Meta


type alias State a =
    { source : String
    , scanpointer : Int
    , tokenIndex : Int
    , sourceLength : Int
    , tokens : List a
    , currentToken : Maybe Token
    , mode : Mode
    }


type Mode
    = Normal
    | InMath
    | InCode


type TokenType
    = TBS
    | TLB
    | TRB
    | TF
    | TLMathBrace
    | TRMathBrace
    | TS
    | TW
    | TMath
    | TCode
    | TTokenError


init : String -> State a
init str =
    { source = str
    , scanpointer = 0
    , sourceLength = String.length str
    , tokens = []
    , currentToken = Nothing
    , tokenIndex = 0
    , mode = Normal
    }


type alias TokenParser =
    Parser Context Problem Token


run : String -> List Token
run source =
    loop (init source) nextStep


nextStep : State Token -> Step (State Token) (List Token)
nextStep state =
    if state.scanpointer >= state.sourceLength then
        finish state

    else
        let
            token =
                get state state.scanpointer (String.dropLeft state.scanpointer state.source)

            newScanPointer =
                state.scanpointer + length token + 1

            ( tokens, tokenIndex, currentToken_ ) =
                if isTextToken token then
                    -- update the current token so as to merge words into a single phrase
                    handleMerge state token

                else if type_ token == TBS then
                    handleBS state token

                else if type_ token == TLB then
                    -- commit a left bracket token immediately, taking care to commit the currentToken if it contains text
                    handleLB state token

                else
                    -- the token is neither a left bracket token nore a text token.  Commit it immediately, taking care
                    -- to also commit the currentToken if it holds text.
                    handleDefault state token

            currentToken =
                if isTextToken token then
                    currentToken_

                else
                    Nothing
        in
        Loop
            { state
                | tokens = tokens
                , scanpointer = newScanPointer
                , tokenIndex = tokenIndex
                , currentToken = currentToken
                , mode = newMode token state.mode
            }



-- XXX --


setIndex : Int -> Token -> Token
setIndex k token =
    case token of
        BS meta ->
            BS { meta | index = k }

        F str meta ->
            F str { meta | index = k }

        LB meta ->
            LB { meta | index = k }

        RB meta ->
            RB { meta | index = k }

        LMathBracket meta ->
            LMathBracket { meta | index = k }

        RMathBracket meta ->
            RMathBracket { meta | index = k }

        S str meta ->
            S str { meta | index = k }

        W str meta ->
            W str { meta | index = k }

        MathToken meta ->
            MathToken { meta | index = k }

        CodeToken meta ->
            CodeToken { meta | index = k }

        TokenError list meta ->
            TokenError list { meta | index = k }


type_ : Token -> TokenType
type_ token =
    case token of
        BS _ ->
            TBS

        F _ _ ->
            TF

        LB _ ->
            TLB

        RB _ ->
            TRB

        LMathBracket _ ->
            TLMathBrace

        RMathBracket _ ->
            TRMathBrace

        S _ _ ->
            TS

        W _ _ ->
            TW

        MathToken _ ->
            TMath

        CodeToken _ ->
            TCode

        TokenError _ _ ->
            TTokenError


getMeta : Token -> Meta
getMeta token =
    case token of
        BS m ->
            m

        F _ m ->
            m

        LB m ->
            m

        RB m ->
            m

        LMathBracket m ->
            m

        RMathBracket m ->
            m

        S _ m ->
            m

        W _ m ->
            m

        MathToken m ->
            m

        CodeToken m ->
            m

        TokenError _ m ->
            m


stringValue : Token -> String
stringValue token =
    case token of
        BS _ ->
            "\\"

        F _ _ ->
            "F"

        LB _ ->
            "{"

        RB _ ->
            "}"

        LMathBracket _ ->
            "\\["

        RMathBracket _ ->
            "\\]"

        S str _ ->
            str

        W str _ ->
            str

        MathToken _ ->
            "$"

        CodeToken _ ->
            "`"

        TokenError _ _ ->
            "tokenError"


stringValue2 : Token -> String
stringValue2 token =
    case token of
        BS m ->
            "BS:" ++ String.fromInt m.index

        F _ m ->
            "F:" ++ String.fromInt m.index

        LB m ->
            "LB:" ++ String.fromInt m.index

        RB m ->
            "RB:" ++ String.fromInt m.index

        LMathBracket m ->
            "LTB:" ++ String.fromInt m.index

        RMathBracket m ->
            "RTB:" ++ String.fromInt m.index

        S str m ->
            "S " ++ str ++ ": " ++ String.fromInt m.index

        W _ m ->
            "W:" ++ String.fromInt m.index

        MathToken m ->
            "$:" ++ String.fromInt m.index

        CodeToken m ->
            "C:" ++ String.fromInt m.index

        TokenError _ m ->
            "tokenError:" ++ String.fromInt m.index


toString : List Token -> String
toString tokens =
    List.map stringValue tokens |> String.join ""


toString2 : List Token -> String
toString2 tokens =
    List.map stringValue2 tokens |> String.join "; "


length : Token -> Int
length token =
    case token of
        BS meta ->
            meta.end - meta.begin

        F _ meta ->
            meta.end - meta.begin

        LB meta ->
            meta.end - meta.begin

        RB meta ->
            meta.end - meta.begin

        LMathBracket meta ->
            meta.end - meta.begin

        RMathBracket meta ->
            meta.end - meta.begin

        S _ meta ->
            meta.end - meta.begin

        MathToken meta ->
            meta.end - meta.begin

        CodeToken meta ->
            meta.end - meta.begin

        W _ meta ->
            meta.end - meta.begin

        TokenError _ meta ->
            meta.end - meta.begin


{-|

    NOTES. In the computation of the end field of the Meta component of a Token,
    one must use the code `end = start + data.end - data.begin  - 1`.  The
    `-1` is because the data.end comes from the position of the scanPointer,
    which is at this juncture pointing one character beyond the string chomped.

-}
get : State Token -> Int -> String -> Token
get state start input =
    case Parser.run (tokenParser state.mode start state.tokenIndex) input of
        Ok token ->
            token

        Err errorList ->
            TokenError errorList { begin = start, end = start + 1, index = state.tokenIndex, id = makeId start state.tokenIndex }


makeId : Int -> Int -> String
makeId a b =
    String.fromInt a ++ "." ++ String.fromInt b


finish : State Token -> Step (State Token) (List Token)
finish state =
    case state.currentToken of
        Just token ->
            Done (token :: state.tokens)

        Nothing ->
            Done state.tokens


handleMerge : State Token -> Token -> ( List Token, Int, Maybe Token )
handleMerge state token =
    ( state.tokens, state.tokenIndex, updateCurrentToken state.tokenIndex token state.currentToken )


handleBS : State Token -> Token -> ( List Token, Int, Maybe Token )
handleBS state token =
    case state.currentToken of
        Nothing ->
            ( setIndex state.tokenIndex token :: state.tokens, state.tokenIndex + 1, Nothing )

        Just textToken ->
            ( setIndex (state.tokenIndex + 1) token :: setIndex state.tokenIndex textToken :: state.tokens
            , state.tokenIndex + 2
            , Nothing
            )


handleLB : State Token -> Token -> ( List Token, Int, Maybe Token )
handleLB state token =
    case state.currentToken of
        Nothing ->
            ( setIndex state.tokenIndex token :: state.tokens, state.tokenIndex + 1, Nothing )

        Just textToken ->
            ( setIndex (state.tokenIndex + 1) token :: setIndex state.tokenIndex textToken :: state.tokens, state.tokenIndex + 2, Nothing )


handleDefault : State Token -> Token -> ( List Token, Int, Maybe Token )
handleDefault state token =
    case state.currentToken of
        Nothing ->
            ( setIndex state.tokenIndex token :: state.tokens, state.tokenIndex + 1, Nothing )

        Just textToken ->
            ( setIndex (state.tokenIndex + 1) token :: textToken :: state.tokens, state.tokenIndex + 2, Nothing )


updateCurrentToken : Int -> Token -> Maybe Token -> Maybe Token
updateCurrentToken index token currentToken =
    case currentToken of
        Nothing ->
            Just (setIndex index token)

        Just token_ ->
            Just <| setIndex index (mergeToken token_ token)


isTextToken : Token -> Bool
isTextToken token =
    List.member (type_ token) [ TW, TS ]


mergeToken : Token -> Token -> Token
mergeToken lastToken currentToken =
    let
        lastTokenMeta =
            getMeta lastToken

        currentTokenMeta =
            getMeta currentToken

        meta =
            { begin = lastTokenMeta.begin, end = currentTokenMeta.end, index = -1 }
    in
    S (stringValue lastToken ++ stringValue currentToken) (boostMeta meta.begin meta.end meta)


boostMeta : Int -> Int -> { begin : Int, end : Int, index : Int } -> { begin : Int, end : Int, index : Int, id : String }
boostMeta lineNumber tokenIndex { begin, end, index } =
    { begin = begin, end = end, index = index, id = makeId lineNumber tokenIndex }


newMode : Token -> Mode -> Mode
newMode token currentMode =
    case currentMode of
        Normal ->
            case token of
                MathToken _ ->
                    InMath

                CodeToken _ ->
                    InCode

                _ ->
                    Normal

        InMath ->
            case token of
                MathToken _ ->
                    Normal

                _ ->
                    InMath

        InCode ->
            case token of
                CodeToken _ ->
                    Normal

                _ ->
                    InCode


{-| Expression.Tokenizer.tokenParser calls L1.tokenParser
with arguments tokenStack and start. The first argument
is not used (although it is for the Markdown parser)
-}
tokenParser : Mode -> Int -> Int -> TokenParser
tokenParser mode start index =
    case mode of
        Normal ->
            tokenParser_ start index

        InMath ->
            mathParser_ start index

        InCode ->
            codeParser_ start index


languageChars =
    [ '\\', '{', '}', '`', '$' ]


mathChars =
    [ '$' ]


codeChars =
    [ '`' ]


tokenParser_ : Int -> Int -> TokenParser
tokenParser_ start index =
    Parser.oneOf
        [ textParser start index
        , leftMathBracketParser start index
        , rightMathBracketParser start index
        , backslashParser start index
        , leftBraceParser start index
        , rightBraceParser start index
        , mathParser start index
        , codeParser start index
        , whiteSpaceParser start index
        ]


mathParser_ : Int -> Int -> TokenParser
mathParser_ start index =
    Parser.oneOf
        [ mathTextParser start index
        , mathParser start index
        , leftMathBracketParser start index
        , rightMathBracketParser start index
        , whiteSpaceParser start index
        ]


codeParser_ : Int -> Int -> TokenParser
codeParser_ start index =
    Parser.oneOf
        [ codeTextParser start index
        , codeParser start index
        , whiteSpaceParser start index
        ]


whiteSpaceParser : Int -> Int -> TokenParser
whiteSpaceParser start index =
    PT.text (\c -> c == ' ') (\c -> c == ' ')
        |> Parser.map (\data -> W data.content { begin = start, end = start, index = index, id = makeId start index })


backslashParser : Int -> Int -> TokenParser
backslashParser start index =
    -- Parser.oneOf [ Parser.backtrackable (backslashParser2 start index), backslashParser1 start index ]
    -- backslashParser1 start index
    Parser.oneOf [ Parser.backtrackable (backslashParser2 start index), backslashParser1 start index ]


backslashParser1 : Int -> Int -> TokenParser
backslashParser1 start index =
    PT.text (\c -> c == '\\') (\_ -> False)
        |> Parser.map (\_ -> BS { begin = start, end = start, index = index, id = makeId start index })


backslashParser2 : Int -> Int -> TokenParser
backslashParser2 start index =
    first (PT.text (\c -> c == '\\') (\c -> (c /= ' ') && (c /= '{'))) (PT.symbol " ")
        |> Parser.map (\data -> F (String.dropLeft 1 data.content) { begin = start, end = start + data.end - data.begin - 1, index = index, id = makeId start index })


first p q =
    p |> Parser.andThen (\x -> q |> Parser.map (\_ -> x))



--decide data =
--    let
--        content = data.content { begin = start, end = start + data.end - data.begin - 1, index = index, id = makeId start index }
-- |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1, index = index, id = makeId start index })


leftBraceParser : Int -> Int -> TokenParser
leftBraceParser start index =
    PT.text (\c -> c == '{') (\_ -> False)
        |> Parser.map (\_ -> LB { begin = start, end = start, index = index, id = makeId start index })


rightBraceParser : Int -> Int -> TokenParser
rightBraceParser start index =
    PT.text (\c -> c == '}') (\_ -> False)
        |> Parser.map (\_ -> RB { begin = start, end = start, index = index, id = makeId start index })


textParser start index =
    PT.text (\c -> not <| List.member c (' ' :: languageChars)) (\c -> not <| List.member c (' ' :: languageChars))
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1, index = index, id = makeId start index })


mathTextParser : Int -> Int -> Parser Context Problem Token
mathTextParser start index =
    PT.text (\c -> not <| List.member c (' ' :: mathChars)) (\c -> not <| List.member c (' ' :: languageChars))
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1, index = index, id = makeId start index })


codeTextParser start index =
    PT.text (\c -> not <| List.member c (' ' :: codeChars)) (\c -> not <| List.member c (' ' :: languageChars))
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1, index = index, id = makeId start index })


mathParser : Int -> Int -> TokenParser
mathParser start index =
    PT.text (\c -> c == '$') (\_ -> False)
        |> Parser.map (\_ -> MathToken { begin = start, end = start, index = index, id = makeId start index })


leftMathBracketParser : Int -> Int -> TokenParser
leftMathBracketParser start index =
    PT.symbol "\\[" |> Parser.map (\_ -> LMathBracket { begin = start, end = start + 1, index = index, id = makeId start index })


rightMathBracketParser : Int -> Int -> TokenParser
rightMathBracketParser start index =
    PT.symbol "\\]" |> Parser.map (\_ -> RMathBracket { begin = start, end = start + 1, index = index, id = makeId start index })


codeParser : Int -> Int -> TokenParser
codeParser start index =
    PT.text (\c -> c == '`') (\_ -> False)
        |> Parser.map (\_ -> CodeToken { begin = start, end = start, index = index, id = makeId start index })
