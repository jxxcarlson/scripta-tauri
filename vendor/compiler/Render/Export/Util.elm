module Render.Export.Util exposing (getArgs, getOneArg, getTwoArgs)

import Compiler.ASTTools as ASTTools
import Parser.Expr exposing (Expr)


getArgs : List Expr -> List String
getArgs =
    ASTTools.exprListToStringList >> List.map String.words >> List.concat >> List.filter (\x -> x /= "")


getOneArg : List Expr -> String
getOneArg exprs =
    case List.head (getArgs exprs) of
        Nothing ->
            ""

        Just str ->
            str


getTwoArgs : List Expr -> { first : String, second : String }
getTwoArgs exprs =
    let
        args =
            getArgs exprs

        n =
            List.length args

        first =
            List.take (n - 1) args |> String.join " "

        second =
            List.drop (n - 1) args |> String.join ""
    in
    { first = first, second = second }
