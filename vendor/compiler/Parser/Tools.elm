module Parser.Tools exposing
    ( Context(..)
    , Problem(..)
    , StringData
    , sequence
    , symbol
    , text
    , textWithEndSymbol
    )

import Parser.Advanced as Parser exposing ((|.), (|=))


type Problem
    = ExpectingPrefix
    | ExpectingSymbol String
    | ExpectingImageStart
    | ExpectingATStart


type Context
    = TextExpression


type alias Parser a =
    Parser.Parser Context Problem a


type alias StringData =
    { begin : Int, end : Int, content : String }


{-| Get the longest string
whose first character satisfies `prefix` and whose remaining
characters satisfy `continue`. ParserTests:

    line =
        textPS (\c -> Char.isAlpha) [ '\n' ]

recognizes lines that start with an alphabetic character.

-}
text : (Char -> Bool) -> (Char -> Bool) -> Parser StringData
text prefix continue =
    Parser.succeed (\start finish content -> { begin = start, end = finish, content = String.slice start finish content })
        |= Parser.getOffset
        |. Parser.chompIf (\c -> prefix c) ExpectingPrefix
        |. Parser.chompWhile (\c -> continue c)
        |= Parser.getOffset
        |= Parser.getSource


symbol : String -> Parser StringData
symbol symb =
    Parser.succeed (\start finish -> { begin = start, end = finish, content = symb })
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token symb (ExpectingSymbol symb))
        |= Parser.getOffset


textWithEndSymbol : String -> (Char -> Bool) -> (Char -> Bool) -> Parser StringData
textWithEndSymbol symb prefix continue =
    Parser.succeed (\start finish content -> { begin = start, end = finish, content = String.slice start finish content })
        |= Parser.getOffset
        |. Parser.chompIf (\c -> prefix c) ExpectingPrefix
        |. Parser.chompWhile (\c -> continue c)
        |. Parser.symbol (Parser.Token symb (ExpectingSymbol symb))
        -- TODO: replace with real "Expecting"
        |= Parser.getOffset
        |= Parser.getSource



-- LOOP


sequence : List (Parser a) -> Parser (List a)
sequence parsers =
    Parser.loop { parsers = parsers, results = [] } sequenceAux


type alias State a =
    { parsers : List (Parser a), results : List a }


sequenceAux : State a -> Parser (Parser.Step (State a) (List a))
sequenceAux state =
    case List.head state.parsers of
        Nothing ->
            Parser.succeed () |> Parser.map (\_ -> Parser.Done (List.reverse state.results))

        Just parser ->
            parser |> Parser.map (\a -> Parser.Loop { state | results = a :: state.results, parsers = List.drop 1 state.parsers })
