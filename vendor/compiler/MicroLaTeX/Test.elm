module MicroLaTeX.Test exposing
    ( check1
    , check2
    , checkErrorHandling
    , fromExpr1
    , fromExpr2
    , p
    , pp
    , print
    )

import Dict exposing (Dict)
import MicroLaTeX.Parser.Expression as Expression
import Parser.Expr exposing (Expr(..))


check1 : String -> Result String String
check1 str =
    let
        str2 =
            pp str
    in
    case str == str2 of
        True ->
            Ok str

        False ->
            Err str2


check2 : String -> Result String String
check2 str =
    let
        str2 =
            pp2 str
    in
    case str == str2 of
        True ->
            Ok str

        False ->
            Err str2


checkErrorHandling : String -> String -> Result String String
checkErrorHandling input output =
    let
        str2 =
            pp input
    in
    case output == str2 of
        True ->
            Ok output

        False ->
            Err str2


p : String -> List Expr
p str =
    Expression.parse 0 str |> Tuple.first


pp : String -> String
pp str =
    str
        |> Expression.parse 0
        |> Tuple.first
        |> print


pp2 : String -> String
pp2 str =
    str
        |> Expression.parse 0
        |> Tuple.first
        |> print2


print : List Expr -> String
print expressions =
    List.map fromExpr1 expressions |> String.join ""


print2 : List Expr -> String
print2 expressions =
    List.map fromExpr2 expressions |> String.join ""


enclose str =
    [ "{", str, "}" ] |> String.join ""


arityDict : Dict String Int
arityDict =
    Dict.fromList [ ( "italic", 1 ) ]


{-|

    > parse 0 "\\f{\\foo{a} = \\foo{b}}" |> Tuple.first  |> List.map MicroLaTeX.Test.fromExpr1
    ["\\f{\\foo{a} = \\foo{b}}"]

    > parse 0 "\\foo{bar}{baz}" |> Tuple.first |> List.map MicroLaTeX.Test.fromExpr2
    -- INCORRECT

-}
fromExpr1 : Expr -> String
fromExpr1 expr =
    case expr of
        Fun name expressions _ ->
            case Dict.get name arityDict of
                Just 1 ->
                    [ "\\", name, List.map fromExpr1 expressions |> String.join "" |> enclose ] |> String.join ""

                _ ->
                    case expressions of
                        [] ->
                            [ "\\", name ] |> String.join ""

                        _ ->
                            [ "\\", name, List.map fromExpr1 expressions |> String.join "" |> enclose ] |> String.join ""

        Text str _ ->
            str

        Verbatim name str _ ->
            case name of
                "math" ->
                    "$" ++ str ++ "$"

                "code" ->
                    "`" ++ str ++ "`"

                _ ->
                    "error: verbatim " ++ name ++ " not recognized"


{-|

    > parse 0 "\\foo{bar}{baz}" |> Tuple.first |> List.map MicroLaTeX.Test.fromExpr2
    ["\\foo{bar}{baz}"]

    > parse 0 "\\f{\\foo{a} = \\foo{b}}" |> Tuple.first  |> List.map MicroLaTeX.Test.fromExpr1
    -- INCORRECT

-}
fromExpr2 : Expr -> String
fromExpr2 expr =
    case expr of
        Fun name expressions _ ->
            case Dict.get name arityDict of
                Just 1 ->
                    [ "\\", name, List.map fromExpr2 expressions |> String.join "" |> enclose ] |> String.join ""

                _ ->
                    case expressions of
                        [] ->
                            [ "\\", name ] |> String.join ""

                        _ ->
                            [ "\\", name, List.map (fromExpr2 >> enclose) expressions |> String.join "" ] |> String.join ""

        Text str _ ->
            str

        Verbatim name str _ ->
            case name of
                "math" ->
                    "$" ++ str ++ "$"

                "code" ->
                    "`" ++ str ++ "`"

                _ ->
                    "error: verbatim " ++ name ++ " not recognized"
