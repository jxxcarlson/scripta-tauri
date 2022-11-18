module Render.XMarkdown exposing (export, exportExpr)

import Compiler.ASTTools as ASTTools
import Compiler.TextMacro as Lambda
import Dict exposing (Dict)
import Either exposing (Either(..))
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr(..))
import Parser.Forest exposing (Forest)
import Render.Utility as Utility
import Tree


export : Forest ExpressionBlock -> String
export ast =
    ast
        |> List.map Tree.flatten
        |> List.concat
        |> List.map exportBlock
        |> String.join "\n\n"
        -- TODO: the next two lines are bad code; find a better solution
        |> String.replace "\n\n\n\n" "\n\n"
        |> String.replace "\n\n\n" "\n\n"


exportBlock : ExpressionBlock -> String
exportBlock (ExpressionBlock { blockType, indent, name, args, content }) =
    case blockType of
        Paragraph ->
            case content of
                Left str ->
                    str

                Right exprs_ ->
                    exportExprList exprs_

        OrdinaryBlock _ ->
            case content of
                Left _ ->
                    ""

                Right exprs_ ->
                    let
                        name_ =
                            name |> Maybe.withDefault "anon"
                    in
                    case Dict.get name_ blockDict of
                        Just f ->
                            f args indent (exportExprList exprs_)

                        Nothing ->
                            if name_ == "textmacros" then
                                "((not implemented))"

                            else
                                environment name_ (exportExprList exprs_)

        VerbatimBlock _ ->
            case content of
                Left str ->
                    case name of
                        Just "math" ->
                            -- TODO: there should be a trailing "$$"
                            [ "$$", str, "$$" ] |> String.join "\n"

                        Just "code" ->
                            [ "```", str, "```" ] |> String.join "\n"

                        _ ->
                            environment "anon" str

                Right _ ->
                    "???(13)"



-- DICIONARIES


verbatimExprDict =
    Dict.empty


macroDict : Dict String (List Expr -> String)
macroDict =
    Dict.fromList
        [ ( "link", link )
        , ( "ilink", ilink )
        , ( "italic", italic )
        , ( "i", italic )
        , ( "bold", bold )
        , ( "b", bold )
        ]


getArgs : List Expr -> List String
getArgs =
    ASTTools.exprListToStringList >> List.map String.words >> List.concat >> List.filter (\x -> x /= "")


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


link : List Expr -> String
link exprs =
    let
        args =
            getTwoArgs exprs
    in
    [ "[", args.first, "](", args.second, ")" ] |> String.join ""


italic : List Expr -> String
italic exprs =
    "_" ++ String.trim (exportExprList exprs) ++ "_"


bold : List Expr -> String
bold exprs =
    "**" ++ String.trim (exportExprList exprs) ++ "**"


ilink : List Expr -> String
ilink exprs =
    let
        args =
            getTwoArgs exprs
    in
    [ "\\href{", "https://l0-lab-demo.lamdera.app/i/", args.second, "}{", args.first, "}" ] |> String.join ""


blockDict : Dict String (List String -> Int -> String -> String)
blockDict =
    Dict.fromList
        [ ( "title", \_ _ body -> "# " ++ body )
        , ( "subtitle", \_ _ body -> "@subtitle " ++ body )
        , ( "author", \_ _ body -> "@author " ++ body )
        , ( "date", \_ _ body -> "@date " ++ body )
        , ( "contents", \_ _ _ -> "@contents" )
        , ( "section", \args _ body -> section args body )
        , ( "item", \_ indent body -> indentBy indent "- " ++ String.trimLeft body )
        , ( "numbered", \_ indent body -> indentBy indent ". " ++ String.trimLeft body )
        ]


indentBy : Int -> String -> String
indentBy k str =
    String.repeat k " " ++ str


section : List String -> String -> String
section args body =
    case Utility.getArg "4" 0 args of
        "1" ->
            "## " ++ body

        "2" ->
            "### " ++ body

        "3" ->
            "#### " ++ body

        _ ->
            "##### " ++ body


macro1 : String -> String -> String
macro1 name arg =
    if name == "math" then
        "$" ++ arg ++ "$"

    else if name == "group" then
        arg

    else if name == "code" then
        "`" ++ arg ++ "`"

    else
        "\\" ++ name ++ "{" ++ String.trimLeft arg ++ "}"


exportExprList : List Expr -> String
exportExprList exprs =
    List.map exportExpr exprs |> String.join ""


exportExpr : Expr -> String
exportExpr expr =
    case expr of
        Fun name exps_ _ ->
            if name == "lambda" then
                case Lambda.extract expr of
                    Just lambda ->
                        Lambda.toString exportExpr lambda

                    Nothing ->
                        "Error extracting lambda"

            else
                case Dict.get name macroDict of
                    Just f ->
                        f exps_

                    Nothing ->
                        macro1 name (List.map exportExpr exps_ |> String.join " ")

        Text str _ ->
            str

        Verbatim name body _ ->
            renderVerbatim name body


renderVerbatim : String -> String -> String
renderVerbatim name body =
    case Dict.get name verbatimExprDict of
        Nothing ->
            macro1 name body

        Just macroName ->
            macro1 macroName body



-- HELPERS


tagged name body =
    "\\" ++ name ++ "{" ++ body ++ "}"


environment name body =
    [ tagged "begin" name, body, tagged "end" name ] |> String.join "\n"
