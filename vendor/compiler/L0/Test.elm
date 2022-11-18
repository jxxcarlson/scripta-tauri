module L0.Test exposing
    ( check1
    , check2
    , checkErrorHandling
    , p
    , pp
    , toString
    , toStringFromList
    )

import L0.Parser.Expression as Expression
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


check2 : String -> Result (List Expr) String
check2 str =
    let
        e1 =
            p str

        e2 =
            ppp str
    in
    case e1 == e2 of
        True ->
            Ok str

        False ->
            Err e2


p : String -> List Expr
p str =
    Expression.parse 0 str


pp : String -> String
pp str =
    str
        |> Expression.parse 0
        |> toStringFromList


ppp : String -> List Expr
ppp str =
    str |> p |> toStringFromList |> p


toStringFromList : List Expr -> String
toStringFromList expressions =
    List.map toString expressions |> String.join ""


toString : Expr -> String
toString expr =
    case expr of
        Fun name expressions _ ->
            let
                body_ =
                    List.map toString expressions |> String.join ""

                body =
                    if body_ == "" then
                        body_

                    else if String.left 1 body_ == "[" then
                        body_

                    else if String.left 1 body_ == " " then
                        body_

                    else
                        " " ++ body_
            in
            "[" ++ name ++ body ++ "]"

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
