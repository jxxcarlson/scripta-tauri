module XMarkdown.Token exposing
    ( Token(..)
    , TokenType(..)
    , changeTokenContentAt
    , changeTokenIndicesFrom
    , idem
    , idemTest
    , imageParser
    , init
    , run
    , toString
    , toString2
    , type_
    )

import List.Extra
import Parser.Advanced as Parser exposing (DeadEnd, Parser)
import Parser.Helpers exposing (Step(..), loop)
import Parser.Meta exposing (Meta)
import Parser.Tools as PT exposing (Context, Problem)



--fakeDebugLog label =
--    Debug.log label


fakeDebugLog =
    \_ -> identity


idem : String -> String
idem str =
    str |> run |> List.reverse |> toString2


idemTest : String -> Bool
idemTest str =
    str == idem str



-- TYPES


type Token
    = LB Meta
    | RB Meta
    | LP Meta
    | RP Meta
    | Image Meta
    | AT Meta
    | Bold Meta
    | Italic Meta
    | S String Meta
    | W String Meta
    | MathToken Meta
    | CodeToken Meta
    | TokenError (List (DeadEnd Context Problem)) Meta


setIndex : Int -> Token -> Token
setIndex k token =
    case token of
        LB meta ->
            LB { meta | index = k }

        RB meta ->
            RB { meta | index = k }

        LP meta ->
            LP { meta | index = k }

        RP meta ->
            RP { meta | index = k }

        Bold meta ->
            Bold { meta | index = k }

        Italic meta ->
            Italic { meta | index = k }

        Image meta ->
            Image { meta | index = k }

        AT meta ->
            AT { meta | index = k }

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
    | TLP
    | TRP
    | TBold
    | TItalic
    | TImage
    | TAT
    | TS
    | TW
    | TMath
    | TCode
    | TTokenError


type_ : Token -> TokenType
type_ token =
    case token of
        LB _ ->
            TLB

        RB _ ->
            TRB

        LP _ ->
            TLP

        RP _ ->
            TRP

        Bold _ ->
            TBold

        Italic _ ->
            TItalic

        Image _ ->
            TImage

        AT _ ->
            TAT

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
        LB m ->
            m

        RB m ->
            m

        LP m ->
            m

        RP m ->
            m

        Bold m ->
            m

        Italic m ->
            m

        Image m ->
            m

        AT m ->
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
        LB _ ->
            "["

        RB _ ->
            "]"

        LP _ ->
            "("

        RP _ ->
            ")"

        Bold _ ->
            "**"

        Italic _ ->
            "*"

        Image _ ->
            "image"

        AT _ ->
            "@"

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
        LB _ ->
            "LB"

        RB _ ->
            "RB"

        LP _ ->
            "LP"

        RP _ ->
            "RP"

        Bold _ ->
            "BOLD"

        Italic _ ->
            "ITALIC"

        Image _ ->
            "IMAGE"

        AT _ ->
            "@"

        S str _ ->
            str

        W str _ ->
            str

        MathToken _ ->
            "M"

        CodeToken _ ->
            "C"

        TokenError _ _ ->
            "tokenError"


toString : List Token -> String
toString tokens =
    List.map stringValue tokens |> String.join ""


toString2 : List Token -> String
toString2 tokens =
    List.map stringValue2 tokens |> String.join ", "


length : Token -> Int
length token =
    case token of
        LB meta ->
            meta.end - meta.begin

        RB meta ->
            meta.end - meta.begin

        LP meta ->
            meta.end - meta.begin

        RP meta ->
            meta.end - meta.begin

        Bold meta ->
            meta.end - meta.begin

        Italic meta ->
            meta.end - meta.begin

        Image meta ->
            meta.end - meta.begin

        AT meta ->
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
                    handleMerge state token |> fakeDebugLog "(1)"

                else if type_ token == TLB then
                    -- commit a left bracket token immediately, taking care to commit the currentToken if it contains text
                    handleLB state token |> fakeDebugLog "(2)"

                else
                    -- the token is neither a left bracket token nore a text token.  Commit it immediately, taking care
                    -- to also commit the currentToken if it holds text.
                    handleDefault state token |> fakeDebugLog "(3)"

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
    [ '\\', '[', ']', '(', ')', '`', '*', '$' ]


mathChars =
    [ '$' ]


codeChars =
    [ '`' ]


tokenParser_ : Int -> Int -> TokenParser
tokenParser_ start index =
    Parser.oneOf
        [ imageParser start index
        , atParser start index
        , textParser start index
        , leftBracketParser start index
        , rightBracketParser start index
        , leftParenParser start index
        , rightParenParser start index
        , boldParser start index
        , italicParser start index
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
        |> Parser.map (\data -> W data.content { begin = start, end = start, index = index, id = makeId start index })


imageParser : Int -> Int -> TokenParser
imageParser start index =
    Parser.symbol (Parser.Token "![" PT.ExpectingImageStart)
        |> Parser.map (\_ -> Image { begin = start, end = start, index = index, id = makeId start index })


atParser : Int -> Int -> TokenParser
atParser start index =
    Parser.symbol (Parser.Token "@[" PT.ExpectingATStart)
        |> Parser.map (\_ -> AT { begin = start, end = start, index = index, id = makeId start index })


leftBracketParser : Int -> Int -> TokenParser
leftBracketParser start index =
    PT.text (\c -> c == '[') (\_ -> False)
        |> Parser.map (\_ -> LB { begin = start, end = start, index = index, id = makeId start index })


rightBracketParser : Int -> Int -> TokenParser
rightBracketParser start index =
    PT.text (\c -> c == ']') (\_ -> False)
        |> Parser.map (\_ -> RB { begin = start, end = start, index = index, id = makeId start index })


leftParenParser : Int -> Int -> TokenParser
leftParenParser start index =
    PT.text (\c -> c == '(') (\_ -> False)
        |> Parser.map (\_ -> LP { begin = start, end = start, index = index, id = makeId start index })


rightParenParser : Int -> Int -> TokenParser
rightParenParser start index =
    PT.text (\c -> c == ')') (\_ -> False)
        |> Parser.map (\_ -> RP { begin = start, end = start, index = index, id = makeId start index })


boldParser : Int -> Int -> TokenParser
boldParser start index =
    Parser.symbol (Parser.Token "**" (PT.ExpectingSymbol "**"))
        |> Parser.map (\_ -> Bold { begin = start, end = start + 1, index = index, id = makeId start index })


italicParser : Int -> Int -> TokenParser
italicParser start index =
    PT.text (\c -> c == '*') (\_ -> False)
        |> Parser.map (\_ -> Italic { begin = start, end = start, index = index, id = makeId start index })


textParser start index =
    PT.text (\c -> not <| List.member c (' ' :: languageChars)) (\c -> not <| List.member c (' ' :: languageChars))
        |> Parser.map (\data -> S data.content { begin = start, end = start + data.end - data.begin - 1, index = index, id = makeId start index })


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


codeParser : Int -> Int -> TokenParser
codeParser start index =
    PT.text (\c -> c == '`') (\_ -> False)
        |> Parser.map (\_ -> CodeToken { begin = start, end = start, index = index, id = makeId start index })


changeTokenContentAt : Int -> String -> List Token -> List Token
changeTokenContentAt k newContent tokens =
    case List.Extra.getAt k tokens of
        Nothing ->
            tokens

        Just tok ->
            case tok of
                S _ meta ->
                    List.Extra.setAt k (S newContent meta) tokens

                _ ->
                    tokens


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

        CodeToken meta ->
            meta.index

        TokenError _ meta ->
            meta.index

        LP meta ->
            meta.index

        RP meta ->
            meta.index

        Image meta ->
            meta.index

        AT meta ->
            meta.index

        Bold meta ->
            meta.index

        Italic meta ->
            meta.index
