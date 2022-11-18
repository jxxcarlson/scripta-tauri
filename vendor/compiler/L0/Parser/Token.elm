module L0.Parser.Token exposing
    ( Meta
    , Token(..)
    , TokenType(..)
    , changeTokenIndicesFrom
    , idem
    , idemTest
    , indexOf
    , init
    , run
    , toString
    , type_
    )

import Parser.Advanced as Parser exposing ((|.), (|=), DeadEnd, Parser)
import Parser.Helpers exposing (Step(..), loop)
import Parser.Tools as PT exposing (Context, Problem)


idem : String -> String
idem str =
    str |> run |> List.reverse |> toString


idemTest : String -> Bool
idemTest str =
    str == idem str



-- TYPES


type Token
    = LB Meta
    | RB Meta
    | S String Meta
    | W String Meta
    | MathToken Meta
    | BracketedMath String Meta
    | CodeToken Meta
    | TokenError (List (DeadEnd Context Problem)) Meta


changeTokenIndicesFrom : Int -> Int -> List Token -> List Token
changeTokenIndicesFrom from delta tokens =
    let
        f : Token -> Token
        f token =
            let
                k =
                    indexOf token
            in
            if k >= from then
                setIndex (k + delta) token

            else
                token
    in
    List.map (\token -> f token) tokens


indexOf : Token -> Int
indexOf token =
    case token of
        LB meta ->
            meta.index

        RB meta ->
            meta.index

        S _ meta ->
            meta.index

        W _ meta ->
            meta.index

        MathToken meta ->
            meta.index

        BracketedMath _ meta ->
            meta.index

        CodeToken meta ->
            meta.index

        TokenError _ meta ->
            meta.index


setIndex : Int -> Token -> Token
setIndex k token =
    case token of
        LB meta ->
            LB { meta | index = k }

        RB meta ->
            RB { meta | index = k }

        S str meta ->
            S str { meta | index = k }

        W str meta ->
            W str { meta | index = k }

        MathToken meta ->
            MathToken { meta | index = k }

        BracketedMath str meta ->
            BracketedMath str { meta | index = k }

        CodeToken meta ->
            CodeToken { meta | index = k }

        TokenError list meta ->
            TokenError list { meta | index = k }


type alias Meta =
    { begin : Int, end : Int, index : Int }


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
    = TLB
    | TRB
    | TS
    | TW
    | TMath
    | TBracketedMath
    | TCode
    | TTokenError


type_ : Token -> TokenType
type_ token =
    case token of
        LB _ ->
            TLB

        RB _ ->
            TRB

        S _ _ ->
            TS

        W _ _ ->
            TW

        MathToken _ ->
            TMath

        BracketedMath _ _ ->
            TBracketedMath

        CodeToken _ ->
            TCode

        TokenError _ _ ->
            TTokenError


getMeta : Token -> Meta
getMeta token =
    case token of
        LB m ->
            m

        RB m ->
            m

        S _ m ->
            m

        W _ m ->
            m

        MathToken m ->
            m

        BracketedMath _ m ->
            m

        CodeToken m ->
            m

        TokenError _ m ->
            m


stringValue : Token -> String
stringValue token =
    case token of
        LB _ ->
            "["

        RB _ ->
            "]"

        S str _ ->
            str

        W str _ ->
            str

        MathToken _ ->
            "$"

        BracketedMath s _ ->
            "\\[" ++ s ++ "\\]"

        CodeToken _ ->
            "`"

        TokenError _ _ ->
            "tokenError"


toString : List Token -> String
toString tokens =
    List.map stringValue tokens |> String.join ""


length : Token -> Int
length token =
    case token of
        LB meta ->
            meta.end - meta.begin

        RB meta ->
            meta.end - meta.begin

        S _ meta ->
            meta.end - meta.begin

        MathToken meta ->
            meta.end - meta.begin

        CodeToken meta ->
            meta.end - meta.begin

        BracketedMath _ meta ->
            meta.end - meta.begin

        W _ meta ->
            meta.end - meta.begin

        TokenError _ meta ->
            meta.end - meta.begin


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
            TokenError errorList { begin = start, end = start + 1, index = state.tokenIndex }


nextStep : State Token -> Step (State Token) (List Token)
nextStep state =
    if state.scanpointer >= state.sourceLength then
        case state.currentToken of
            Just token ->
                Done (token :: state.tokens)

            Nothing ->
                Done state.tokens

    else
        let
            token =
                get state state.scanpointer (String.dropLeft state.scanpointer state.source)

            newScanPointer =
                state.scanpointer + length token + 1

            ( tokens, tokenIndex, currentToken_ ) =
                if isTextToken token then
                    -- if head of the token list is a left bracket token, commit the text token immediately:
                    -- it is the expected function name
                    if Maybe.map type_ (List.head state.tokens) == Just TLB then
                        ( setIndex state.tokenIndex token :: state.tokens, state.tokenIndex + 1, Nothing )

                    else
                        -- otherwise, update the current token so as to merge words into a single phrase
                        ( state.tokens, state.tokenIndex, updateCurrentToken state.tokenIndex token state.currentToken )

                else if type_ token == TLB then
                    -- commit a left bracket token immediately, taking care to commit the currentToken if it contains text
                    case state.currentToken of
                        Nothing ->
                            ( setIndex state.tokenIndex token :: state.tokens, state.tokenIndex + 1, Nothing )

                        Just textToken ->
                            ( setIndex (state.tokenIndex + 1) token :: setIndex state.tokenIndex textToken :: state.tokens, state.tokenIndex + 2, Nothing )

                else
                    -- the token is neither a left bracket token nore a text token.  Commit it immediatley, taking care
                    -- to also commit the currentToken if it holds text.
                    case state.currentToken of
                        Nothing ->
                            ( setIndex state.tokenIndex token :: state.tokens, state.tokenIndex + 1, Nothing )

                        Just textToken ->
                            ( setIndex (state.tokenIndex + 1) token :: textToken :: state.tokens, state.tokenIndex + 2, Nothing )

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
    S (stringValue lastToken ++ stringValue currentToken) meta


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
    [ '[', ']', '`', '$', '\\' ]


mathChars =
    [ '$' ]


codeChars =
    [ '`' ]


tokenParser_ : Int -> Int -> TokenParser
tokenParser_ start index =
    Parser.oneOf
        [ textParser start index
        , leftBracketParser start index
        , rightBracketParser start index
        , bracketedMathParser start index
        , mathParser start index
        , codeParser start index
        , whiteSpaceParser start index
        ]


mathParser_ : Int -> Int -> TokenParser
mathParser_ start index =
    Parser.oneOf
        [ mathTextParser start index
        , mathParser start index
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
        |> Parser.map (\data -> W data.content { begin = start, end = start, index = index })


leftBracketParser : Int -> Int -> TokenParser
leftBracketParser start index =
    PT.text (\c -> c == '[') (\_ -> False)
        |> Parser.map (\_ -> LB { begin = start, end = start, index = index })


rightBracketParser : Int -> Int -> TokenParser
rightBracketParser start index =
    PT.text (\c -> c == ']') (\_ -> False)
        |> Parser.map (\_ -> RB { begin = start, end = start, index = index })


bracketedMathParser : Int -> Int -> TokenParser
bracketedMathParser start index =
    Parser.succeed (\a b content -> BracketedMath (String.slice a (b - 2) content) { begin = start, end = start + b - a + 1, index = index })
        |. Parser.symbol (Parser.Token "\\[" (PT.ExpectingSymbol "\\["))
        |= Parser.getOffset
        |. Parser.chompUntil (Parser.Token "\\]" (PT.ExpectingSymbol "\\]"))
        |. Parser.symbol (Parser.Token "\\]" (PT.ExpectingSymbol "\\]"))
        |= Parser.getOffset
        |= Parser.getSource


textParser start index =
    PT.text (\c -> not <| List.member c (' ' :: languageChars)) (\c -> not <| List.member c (' ' :: languageChars))
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1, index = index })


mathTextParser start index =
    PT.text (\c -> not <| List.member c (' ' :: mathChars)) (\c -> not <| List.member c (' ' :: languageChars))
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1, index = index })


codeTextParser start index =
    PT.text (\c -> not <| List.member c (' ' :: codeChars)) (\c -> not <| List.member c (' ' :: languageChars))
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1, index = index })


mathParser : Int -> Int -> TokenParser
mathParser start index =
    PT.text (\c -> c == '$') (\_ -> False)
        |> Parser.map (\_ -> MathToken { begin = start, end = start, index = index })


codeParser : Int -> Int -> TokenParser
codeParser start index =
    PT.text (\c -> c == '`') (\_ -> False)
        |> Parser.map (\_ -> CodeToken { begin = start, end = start, index = index })
