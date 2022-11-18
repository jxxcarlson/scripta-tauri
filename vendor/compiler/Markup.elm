module Markup exposing
    ( parse
    , isVerbatimLine, messagesFromForest, parsePlainText, primitiveBlockToExpressionBlock, toPrimitiveBlockForest, toPrimitiveBlocks
    )

{-| A Parser for the experimental Markup module. See the app folder to see how it is used.
The Render folder in app could have been included with the parser. However, this way
users are free to design their own renderer.

Since this package is still experimental (but needed in various test projects).
The documentation is skimpy.

@docs parse, parseToIntermediateBlocks

-}

import Compiler.Transform
import L0.Parser.Expression
import MicroLaTeX.Parser.Expression
import Parser.Block exposing (ExpressionBlock)
import Parser.BlockUtil
import Parser.Expr exposing (Expr(..))
import Parser.Forest exposing (Forest)
import Parser.PrimitiveBlock exposing (PrimitiveBlock)
import Parser.Tree
import Scripta.Language exposing (Language(..))
import Tree
import XMarkdown.Expression


{-| -}
parse : Language -> String -> Forest ExpressionBlock
parse lang sourceText =
    let
        parser =
            case lang of
                MicroLaTeXLang ->
                    MicroLaTeX.Parser.Expression.parse

                L0Lang ->
                    L0.Parser.Expression.parseWithMessages

                PlainTextLang ->
                    \_ s -> ( parsePlainText s, [] )

                XMarkdownLang ->
                    \i s -> ( XMarkdown.Expression.parse i s, [] )
    in
    sourceText
        |> toPrimitiveBlockForest lang
        |> Parser.Forest.map (Parser.BlockUtil.toExpressionBlock lang parser)


primitiveBlockToExpressionBlock : Language -> PrimitiveBlock -> ExpressionBlock
primitiveBlockToExpressionBlock lang block =
    let
        parser =
            case lang of
                MicroLaTeXLang ->
                    MicroLaTeX.Parser.Expression.parse

                L0Lang ->
                    L0.Parser.Expression.parseWithMessages

                PlainTextLang ->
                    \_ s -> ( parsePlainText s, [] )

                XMarkdownLang ->
                    \i s -> ( XMarkdown.Expression.parse i s, [] )
    in
    Parser.BlockUtil.toExpressionBlock lang parser block


messagesFromTree : Tree.Tree ExpressionBlock -> List String
messagesFromTree tree =
    List.map Parser.BlockUtil.getMessages (Tree.flatten tree) |> List.concat


messagesFromForest : Forest ExpressionBlock -> List String
messagesFromForest forest =
    List.map messagesFromTree forest |> List.concat


parsePlainText : String -> List Parser.Expr.Expr
parsePlainText str =
    [ Text str { begin = 0, end = 0, index = 0, id = "??" } ]


emptyBlock =
    Parser.PrimitiveBlock.empty


toPrimitiveBlocks lang str =
    str
        |> String.lines
        |> Parser.PrimitiveBlock.parse lang isVerbatimLine


toPrimitiveBlockForest : Language -> String -> Forest PrimitiveBlock
toPrimitiveBlockForest lang str =
    str
        |> String.lines
        |> Parser.PrimitiveBlock.parse lang isVerbatimLine
        |> List.map (Compiler.Transform.transform lang)
        |> Parser.Tree.forestFromBlocks { emptyBlock | indent = -2 } .indent
        |> Result.withDefault []


isVerbatimLine : String -> Bool
isVerbatimLine str =
    (String.left 2 str == "||")
        || (String.left 3 str == "```")
        || (String.left 16 str == "\\begin{equation}")
        || (String.left 15 str == "\\begin{aligned}")
        || (String.left 15 str == "\\begin{comment}")
        || (String.left 12 str == "\\begin{code}")
        || (String.left 12 str == "\\begin{verbatim}")
        || (String.left 18 str == "\\begin{mathmacros}")
        || (String.left 14 str == "\\begin{iframe}")
        || (String.left 2 str == "$$")
