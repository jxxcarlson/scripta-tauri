module MicroLaTeX.Parser.Expression exposing
    ( State
    , extractMessages
    , isReducible
    , parse
    , parseToState
    , parseTokens
    , reduceRestOfTokens
    , reduceTokens
    )

import List.Extra
import MicroLaTeX.Parser.Match
import MicroLaTeX.Parser.Symbol as Symbol exposing (Symbol(..))
import MicroLaTeX.Parser.Token as Token exposing (Token(..), TokenType(..))
import Parser.Expr exposing (Expr(..))
import Parser.Helpers as Helpers exposing (Step(..), loop)
import Parser.Meta
import Tools



-- TYPES


type alias State =
    { step : Int
    , tokens : List Token
    , numberOfTokens : Int
    , tokenIndex : Int
    , committed : List Expr
    , stack : List Token
    , messages : List String
    , lineNumber : Int
    }


extractMessages : State -> List String
extractMessages state =
    state.messages



-- STATE FOR THE PARSER


initWithTokens : Int -> List Token -> State
initWithTokens lineNumber tokens =
    { step = 0
    , tokens = List.reverse tokens
    , numberOfTokens = List.length tokens
    , tokenIndex = 0
    , committed = []
    , stack = []
    , messages = []
    , lineNumber = lineNumber
    }



-- Exposed functions


parseTokens : List Token -> List Expr
parseTokens tokens =
    let
        state =
            tokens |> initWithTokens 0 |> run

        exprs =
            state.committed
    in
    exprs


parse : Int -> String -> ( List Expr, List String )
parse lineNumber str =
    let
        state =
            str |> Token.run |> initWithTokens lineNumber |> run

        exprs =
            state.committed

        messages =
            state.messages
    in
    ( exprs, messages )


parseToState : Int -> String -> State
parseToState lineNumber str =
    str
        |> Token.run
        |> initWithTokens lineNumber
        |> run



-- PARSER


run : State -> State
run state =
    loop state nextStep
        |> (\state_ -> { state_ | committed = List.reverse state_.committed })


nextStep : State -> Step State State
nextStep state =
    case List.Extra.getAt state.tokenIndex state.tokens of
        Nothing ->
            if List.isEmpty state.stack then
                Done (state |> Tools.forklogBlue "Done" 12 show)

            else
                -- the stack is not empty, so we need to handle the parse error
                recoverFromError (state |> Tools.forklogRed "Recover" 12 show)

        Just token ->
            pushToken token { state | tokenIndex = state.tokenIndex + 1 }
                |> reduceState
                |> (\st -> { st | step = st.step + 1 })
                |> Tools.forklogCyan "Push-Reduce" 12 show
                |> Loop


show state =
    ( state.stack |> List.reverse |> Token.toString2, state.committed |> List.map Parser.Expr.simplify )



-- PUSH


pushToken : Token -> State -> State
pushToken token state =
    case token of
        S _ _ ->
            pushOrCommit token state

        F _ _ ->
            commit token state

        W _ _ ->
            pushOrCommit token state

        MathToken _ ->
            pushOnStack token state

        LMathBracket _ ->
            pushOnStack token state

        RMathBracket _ ->
            pushOnStack token state

        CodeToken _ ->
            pushOnStack token state

        BS _ ->
            pushOnStack token state

        LB _ ->
            pushOnStack token state

        RB _ ->
            pushOnStack token state

        TokenError _ _ ->
            pushOnStack token state


pushOnStack : Token -> State -> State
pushOnStack token state =
    { state | stack = token :: state.stack }


pushOrCommit : Token -> State -> State
pushOrCommit token state =
    if List.isEmpty state.stack then
        commit token state

    else
        push token state


commit : Token -> State -> State
commit token state =
    case exprOfToken state.lineNumber token of
        Nothing ->
            state

        Just expr ->
            { state | committed = expr :: state.committed }


exprOfToken : Int -> Token -> Maybe Expr
exprOfToken lineNumber token =
    case token of
        F str meta ->
            Just (Fun str [] (boostMeta lineNumber meta))

        S str meta ->
            Just (Text str (boostMeta lineNumber meta))

        W str meta ->
            Just (Text str (boostMeta lineNumber meta))

        _ ->
            Nothing


push : Token -> State -> State
push token state =
    { state | stack = token :: state.stack }



-- REDUCE


isLBToken maybeTok =
    case maybeTok of
        Just (LB _) ->
            True

        _ ->
            False


reduceState : State -> State
reduceState state =
    let
        peek : Maybe Token
        peek =
            List.Extra.getAt state.tokenIndex state.tokens
    in
    -- the peek clause is needed to parse macros with more than one argument
    if isReducible state.stack && not (Maybe.map Token.type_ peek == Just TLB) then
        reduceState_ state

    else
        state |> Tools.forklogRed "Not reducible" 12 (\state_ -> state_.stack |> Symbol.convertTokens2 |> List.reverse)


reduceState_ : State -> State
reduceState_ state =
    let
        symbols : List Symbol
        symbols =
            state.stack |> Symbol.convertTokens2 |> List.reverse |> Tools.forklogYellow "Symbols (reduceState_)" 12 identity
    in
    case List.head symbols of
        Just B ->
            case reduceTokens state.lineNumber (state.stack |> List.reverse) of
                (Fun "ERROR" [ Text message _ ] _) :: rest ->
                    { state | stack = [], committed = rest ++ state.committed, messages = Helpers.prependMessage state.lineNumber message state.messages }

                exprs ->
                    { state | stack = [], committed = exprs ++ state.committed }

        Just M ->
            handleMath state

        Just LM ->
            handleBracketedMath state

        Just C ->
            handleCode state

        _ ->
            state


handleBracketedMath : State -> State
handleBracketedMath state =
    let
        content =
            state.stack |> List.reverse |> Token.toString

        trailing =
            String.right 1 content

        committed =
            if trailing == "]" then
                Verbatim "math" (content |> String.dropLeft 2 |> String.dropRight 2) (boostMeta_ state.tokenIndex 2 { begin = 0, end = 0, index = 0 }) :: state.committed

            else
                Fun "red" [ Text "$" dummyLocWithId ] dummyLocWithId
                    :: Verbatim "math" (String.replace "$" "" content) { begin = 0, end = 0, index = 0, id = makeId state.lineNumber state.tokenIndex }
                    :: state.committed
    in
    { state | stack = [], committed = committed }


handleMath : State -> State
handleMath state =
    case state.stack of
        (MathToken _) :: (S str m2) :: (MathToken _) :: [] ->
            { state
                | committed = Verbatim "math" str (boostMeta state.lineNumber m2) :: state.committed
                , stack = []
            }

        _ ->
            state


handleCode : State -> State
handleCode state =
    let
        content =
            state.stack |> List.reverse |> Token.toString

        trailing =
            String.right 1 content

        committed =
            if trailing == "`" && content == "`" then
                let
                    ( first_, rest_ ) =
                        case state.committed of
                            first :: rest ->
                                ( first, rest )

                            _ ->
                                ( Fun "red" [ Text "????(4)" (boostMeta_ state.lineNumber state.tokenIndex dummyLoc) ] dummyLocWithId, [] )
                in
                first_ :: Fun "red" [ Text "`" (boostMeta_ state.lineNumber state.tokenIndex dummyLoc) ] dummyLocWithId :: rest_

            else if trailing == "`" then
                Verbatim "code" (String.replace "`" "" content) (boostMeta_ state.lineNumber state.tokenIndex { begin = 0, end = 0, index = 0 }) :: state.committed

            else
                Fun "red" [ Text "`" dummyLocWithId ] dummyLocWithId :: Verbatim "code" (String.replace "`" "" content) (boostMeta_ state.lineNumber state.tokenIndex { begin = 0, end = 0, index = 0 }) :: state.committed
    in
    { state | stack = [], committed = committed }


reduceTokens : Int -> List Token -> List Expr
reduceTokens lineNumber tokens =
    case tokens of
        -- The reversed token list is of the form [LB name EXPRS RB], so return [Expr name (evalList EXPRS)]
        (S t m1) :: (BS m2) :: rest ->
            Text t (boostMeta lineNumber m1) :: reduceTokens lineNumber (BS m2 :: rest)

        (S t m2) :: rest ->
            Text t (boostMeta lineNumber m2) :: reduceRestOfTokens Nothing lineNumber rest

        (BS m1) :: (S name _) :: rest ->
            let
                ( a, b ) =
                    split rest
            in
            if b == [] then
                [ Fun name (reduceRestOfTokens (Just name) lineNumber rest) m1 ]

            else if List.head b |> isLBToken then
                [ Fun name (reduceRestOfTokens (Just name) lineNumber a ++ reduceRestOfTokens (Just name) lineNumber b) m1 ]

            else
                [ Fun name (reduceRestOfTokens (Just name) lineNumber a) m1 ] ++ reduceRestOfTokens (Just name) lineNumber b

        _ ->
            [ errorMessage1Part "{??}" ]


reduceRestOfTokens : Maybe String -> Int -> List Token -> List Expr
reduceRestOfTokens macroName lineNumber tokens =
    case tokens of
        (BS _) :: _ ->
            reduceTokens lineNumber tokens

        (S str m1) :: rest ->
            Text str (boostMeta lineNumber m1) :: reduceRestOfTokens Nothing lineNumber rest

        (LB _) :: _ ->
            case MicroLaTeX.Parser.Match.match (Symbol.convertTokens2 tokens) of
                -- there was no match for the left brace;
                -- this is an error
                Nothing ->
                    errorMessage3Part ("\\" ++ (macroName |> Maybe.withDefault "x")) (Token.toString tokens) " ?}"

                Just k ->
                    -- there are k matching tokens
                    let
                        ( a, b ) =
                            MicroLaTeX.Parser.Match.splitAt (k + 1) tokens

                        aa =
                            -- drop the leading and trailing LB, RG
                            a |> List.take (List.length a - 1) |> List.drop 1
                    in
                    reduceTokens lineNumber aa ++ reduceRestOfTokens Nothing lineNumber b

        (MathToken _) :: (S str m2) :: (MathToken _) :: more ->
            Verbatim "math" str (boostMeta lineNumber m2) :: reduceRestOfTokens Nothing lineNumber more

        (LMathBracket _) :: (S str m2) :: (RMathBracket _) :: more ->
            Verbatim "math" str (boostMeta lineNumber m2) :: reduceRestOfTokens Nothing lineNumber more

        token :: more ->
            case exprOfToken lineNumber token of
                Just expr ->
                    expr :: reduceRestOfTokens Nothing lineNumber more

                Nothing ->
                    [ errorMessage "•••?(7)" ]

        [] ->
            []


split : List Token -> ( List Token, List Token )
split tokens =
    case MicroLaTeX.Parser.Match.match (Symbol.convertTokens2 tokens) of
        Nothing ->
            ( tokens, [] )

        Just k ->
            MicroLaTeX.Parser.Match.splitAt (k + 1) tokens


isReducible : List Token -> Bool
isReducible tokens =
    let
        symbols =
            tokens |> List.reverse |> Symbol.convertTokens2

        --|> List.filter (\sym -> sym /= O)
    in
    if symbols == [] then
        False

    else
        symbols |> MicroLaTeX.Parser.Match.reducible


recoverFromError : State -> Step State State
recoverFromError state =
    case List.reverse state.stack of
        (BS _) :: (S fname m) :: [] ->
            Done
                { state
                    | committed = Fun fname [] m :: state.committed
                    , stack = []
                }

        (BS _) :: (S fname _) :: (LB m3) :: _ ->
            Loop
                { state
                    | committed = errorMessage ("\\" ++ fname ++ "{") :: state.committed
                    , stack = []
                    , tokenIndex = m3.index + 1
                    , messages = Helpers.prependMessage state.lineNumber ("Missing right brace, column " ++ String.fromInt m3.begin) state.messages
                }

        -- braces with no intervening text
        (LB _) :: (RB meta) :: _ ->
            Loop
                { state
                    | committed = errorMessage "{?}" :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , messages = Helpers.prependMessage state.lineNumber "Brackets need to enclose something" state.messages
                }

        -- consecutive left brackets
        (LB _) :: (LB meta) :: _ ->
            Loop
                { state
                    | committed = errorMessage "{" :: state.committed
                    , stack = []
                    , tokenIndex = meta.index
                    , messages = Helpers.prependMessage state.lineNumber "You have consecutive left brackets" state.messages
                }

        -- missing right bracket // OK
        (LB _) :: (S fName meta) :: rest ->
            Loop
                { state
                    | committed = errorMessage (errorSuffix rest) :: errorMessage2 ("{" ++ fName) :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , messages = Helpers.prependMessage state.lineNumber "Missing right bracket" state.messages
                }

        -- space after left bracket // OK
        (LB _) :: (W " " meta) :: _ ->
            Loop
                { state
                    | committed = errorMessage "{ - can't have space after the brace " :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , messages = Helpers.prependMessage state.lineNumber "Can't have space after left bracket - try [something ..." state.messages
                }

        -- left bracket with nothing after it.  // OK
        (LB _) :: [] ->
            Done
                { state
                    | committed = errorMessage "..extra{?" :: state.committed
                    , stack = []
                    , tokenIndex = 0
                    , numberOfTokens = 0
                    , messages = Helpers.prependMessage state.lineNumber "That left bracket needs something after it" state.messages
                }

        -- extra right bracket
        (RB meta) :: _ ->
            Loop
                { state
                    | committed = errorMessage " extra }?" :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , messages = Helpers.prependMessage state.lineNumber "Extra right braces(s)" state.messages
                }

        -- dollar sign with no closing dollar sign
        (MathToken meta) :: rest ->
            let
                content =
                    Token.toString rest

                message =
                    if content == "" then
                        "$?$"

                    else
                        "$ "
            in
            Loop
                { state
                    | committed = errorMessage message :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , numberOfTokens = 0
                    , messages = Helpers.prependMessage state.lineNumber "opening dollar sign needs to be matched with a closing one" state.messages
                }

        -- backtick with no closing backtick
        (CodeToken meta) :: rest ->
            let
                content =
                    Token.toString rest

                message =
                    if content == "" then
                        "`?`"

                    else
                        "` "
            in
            Loop
                { state
                    | committed = errorMessageBold message :: state.committed
                    , stack = []
                    , tokenIndex = meta.index + 1
                    , numberOfTokens = 0
                    , messages = Helpers.prependMessage state.lineNumber "opening backtick needs to be matched with a closing one" state.messages
                }

        _ ->
            recoverFromError1 state


errorSuffix rest =
    case rest of
        [] ->
            "]?"

        (W _ _) :: [] ->
            "]?"

        _ ->
            ""


boostMeta : Int -> Parser.Meta.Meta -> Parser.Meta.Meta
boostMeta lineNumber meta =
    { meta | id = String.fromInt lineNumber ++ "." ++ meta.id }


boostMeta_ : Int -> Int -> { begin : Int, end : Int, index : Int } -> { begin : Int, end : Int, index : Int, id : String }
boostMeta_ lineNumber tokenIndex { begin, end, index } =
    { begin = begin, end = end, index = index, id = makeId lineNumber tokenIndex }


makeId : Int -> Int -> String
makeId a b =
    String.fromInt a ++ "." ++ String.fromInt b


recoverFromError1 : State -> Step State State
recoverFromError1 state =
    let
        k =
            Symbol.balance <| Symbol.convertTokens2 (List.reverse state.stack)

        newStack =
            List.repeat k (RB (boostMeta_ state.lineNumber state.tokenIndex dummyLoc)) ++ state.stack

        newSymbols =
            Symbol.convertTokens2 (List.reverse newStack)

        reducible =
            MicroLaTeX.Parser.Match.reducible newSymbols
    in
    if reducible then
        Done <|
            addErrorMessage " ]? " <|
                reduceState <|
                    { state
                        | stack = newStack
                        , tokenIndex = 0
                        , numberOfTokens = List.length newStack
                        , committed = errorMessage "{" :: state.committed

                        -- TODO: the below supresses spurious error messages. But it might supress others as well.
                        --, messages = Helpers.prependMessage state.lineNumber ("Unmatched brackets: added " ++ String.fromInt k ++ " right brackets") state.messages
                    }

    else
        Done
            { state
                | committed =
                    bracketError k
                        :: state.committed
                , messages = Helpers.prependMessage state.lineNumber (braceErrorAsString k) state.messages
            }


bracketError : Int -> Expr
bracketError k =
    if k < 0 then
        let
            braces =
                List.repeat -k "]" |> String.join ""
        in
        errorMessage <| " " ++ braces ++ " extra { (" ++ String.fromInt -k ++ ")"

    else
        let
            braces =
                List.repeat k "{" |> String.join ""
        in
        errorMessage <| " " ++ "\\" ++ braces ++ "?"


braceErrorAsString : Int -> String
braceErrorAsString k =
    if k < 0 then
        "Too many right braces (" ++ String.fromInt -k ++ ")"

    else
        "Too many left braces (" ++ String.fromInt k ++ ")"



-- ERROR MESSAGES


errorMessage1Part : String -> Expr
errorMessage1Part a =
    Fun "errorHighlight" [ Text a dummyLocWithId ] dummyLocWithId


errorMessage3Part : String -> String -> String -> List Expr
errorMessage3Part a b c =
    [ Fun "blue" [ Text a dummyLocWithId ] dummyLocWithId, Fun "errorHighlight" [ Text b dummyLocWithId ] dummyLocWithId, Fun "errorHighlight" [ Text c dummyLocWithId ] dummyLocWithId ]


errorMessage : String -> Expr
errorMessage message =
    Fun "errorHighlight" [ Text message dummyLocWithId ] dummyLocWithId


errorMessageBold : String -> Expr
errorMessageBold message =
    Fun "bold" [ Fun "red" [ Text message dummyLocWithId ] dummyLocWithId ] dummyLocWithId


errorMessage2 : String -> Expr
errorMessage2 message =
    Fun "blue" [ Text message dummyLocWithId ] dummyLocWithId


addErrorMessage : String -> State -> State
addErrorMessage message state =
    let
        committed =
            errorMessage message :: state.committed
    in
    { state | committed = committed }



-- HELPERS


dummyTokenIndex =
    0


dummyLoc =
    { begin = 0, end = 0, index = dummyTokenIndex }


dummyLocWithId =
    { begin = 0, end = 0, index = dummyTokenIndex, id = "dummy (3)" }



-- LOOP
