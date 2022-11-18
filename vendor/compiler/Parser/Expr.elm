module Parser.Expr exposing
    ( Expr(..)
    , SExpr(..), condenseUrl, getName, simplify
    )

{-| The syntax tree for the parser is of type Expr. In the examples below, we use `Parser.Expression.parse`;
in `parse 0 STRING` 0 is the line number at which the text begins. The Meta component
of an expression gives the position of the text of the expression in the source text and
other information, e.g., the index of the associated token in the list of tokens from
which it is derived.

    - Variant Expr:

    > parse 0 "[italic stuff]"
      [Expr "italic"
        [Text (" stuff") { begin = 7, end = 12, index = 2 }]
        { begin = 1, end = 6, index = 1 }
      ]

    - Variant Text:

    > parse 0 "this is a test"
      [Text ("this is a test") { begin = 0, end = 13, index = 0 }]

    - Variant Verbatim: =

    > parse 0 "$x^2 = y^3$"
      [Verbatim "math" ("x^2 = y^3") { begin = 0, end = 0, index = 0 }]

    > parse 0 "`a[0] := 17;`"
      [Verbatim "code" ("a[0] := 17;") { begin = 0, end = 0, index = 0 }]

@docs Expr

-}

import List.Extra
import Parser.Meta exposing (Meta)


{-| -}
type Expr
    = Fun String (List Expr) Meta
    | Text String Meta
    | Verbatim String String Meta


getName : Expr -> Maybe String
getName expr =
    case expr of
        Fun name _ _ ->
            Just name

        Text _ _ ->
            Nothing

        Verbatim name _ _ ->
            Just name


{-| Use to transform image urls for export and PDF generation
-}
condenseUrl : Expr -> Expr
condenseUrl expr =
    case expr of
        Fun "image" ((Text url meta1) :: rest) meta2 ->
            Fun "image" (Text (smashUrl url) meta1 :: rest) meta2

        _ ->
            expr


smashUrl url =
    url |> String.replace "https://" "" |> String.replace "http://" ""


type SExpr
    = SFun String (List SExpr)
    | SText String
    | SVerbatim String String


simplify : Expr -> SExpr
simplify expr =
    case expr of
        Fun str exprs _ ->
            SFun str (List.map simplify exprs)

        Text str _ ->
            SText str

        Verbatim a b _ ->
            SVerbatim a b



-- | EL (List Expr)
