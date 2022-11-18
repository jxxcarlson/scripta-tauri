module Compiler.ASTTools exposing
    ( banner
    , blockNameInList
    , blockNames
    , existsBlockWithName
    , exprListToStringList
    , expressionNames
    , extractTextFromSyntaxTreeByKey
    , filterASTOnName
    , filterBlocks
    , filterBlocksByArgs
    , filterBlocksOnName
    , filterExpressionsOnName
    , filterExpressionsOnName_
    , filterExprs
    , filterForestForExpressionsWithName
    , filterForestOnLabelNames
    , filterNotBlocksOnName
    , filterOutExpressionsOnName
    , frontMatterDict
    , getBlockArgsByName
    , getText
    , getValue
    , getVerbatimBlockValue
    , isBlank
    , matchingIdsInAST
    , normalize
    , rawBlockNames
    , runninghead
    , stringValueOfList
    , tableOfContents
    , title
    , titleTOC
    , toExprRecord
    )

import Bool.Extra
import Dict exposing (Dict)
import Either exposing (Either(..))
import List.Extra
import Maybe.Extra
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr(..))
import Parser.Forest exposing (Forest)
import Tree exposing (Tree)


normalize : Either String (List Expr) -> Either String (List Expr)
normalize exprs =
    case exprs of
        Right ((Text _ _) :: rest) ->
            Right rest

        _ ->
            exprs


filterForestForExpressionsWithName : String -> Forest Expr -> List Expr
filterForestForExpressionsWithName name forest =
    filterExpressionsOnName name (List.map Tree.flatten forest |> List.concat)


blockNames : List (Tree.Tree Parser.Block.ExpressionBlock) -> List String
blockNames forest =
    forest
        |> rawBlockNames
        |> List.Extra.unique
        |> List.sort


rawBlockNames : List (Tree.Tree Parser.Block.ExpressionBlock) -> List String
rawBlockNames forest =
    List.map Tree.flatten forest
        |> List.concat
        |> List.map Parser.Block.getName
        |> Maybe.Extra.values


expressionNames : List (Tree.Tree ExpressionBlock) -> List String
expressionNames forest =
    List.map Tree.flatten forest
        |> List.concat
        |> List.map Parser.Block.getContent
        |> List.concat
        |> List.map Parser.Expr.getName
        |> Maybe.Extra.values
        |> List.Extra.unique
        |> List.sort


filterExpressionsOnName : String -> List Expr -> List Expr
filterExpressionsOnName name exprs =
    List.filter (matchExprOnName name) exprs


filterOutExpressionsOnName : String -> List Expr -> List Expr
filterOutExpressionsOnName name exprs =
    List.filter (\expr -> not (matchExprOnName name expr)) exprs


filterExpressionsOnName_ : String -> List Expr -> List Expr
filterExpressionsOnName_ name exprs =
    List.filter (matchExprOnName_ name) exprs


filterExprs : (Expr -> Bool) -> List Expr -> List Expr
filterExprs predicate list =
    List.filter (\item -> predicate item) list


isBlank : Expr -> Bool
isBlank expr =
    case expr of
        Text content _ ->
            if String.trim content == "" then
                True

            else
                False

        _ ->
            False


filterBlocksOnName : String -> List ExpressionBlock -> List ExpressionBlock
filterBlocksOnName name blocks =
    List.filter (matchBlockName name) blocks


blockNameInList : ExpressionBlock -> List String -> Bool
blockNameInList block names =
    Bool.Extra.any (List.map (\name -> matchBlockName name block) names)


filterBlocks : (ExpressionBlock -> Bool) -> List ExpressionBlock -> List ExpressionBlock
filterBlocks predicate blocks =
    List.filter predicate blocks


filterNotBlocksOnName : String -> List ExpressionBlock -> List ExpressionBlock
filterNotBlocksOnName name blocks =
    List.filter (matchBlockName name >> not) blocks


filterForestOnLabelNames : (Maybe String -> Bool) -> Forest ExpressionBlock -> Forest ExpressionBlock
filterForestOnLabelNames predicate forest =
    List.filter (\tree -> predicate (labelName tree)) forest


labelName : Tree ExpressionBlock -> Maybe String
labelName tree =
    let
        (ExpressionBlock { name }) =
            Tree.label tree
    in
    name


matchBlockName : String -> ExpressionBlock -> Bool
matchBlockName key (ExpressionBlock { name }) =
    Just key == name


matchExprOnName : String -> Expr -> Bool
matchExprOnName name expr =
    case expr of
        Fun name2 _ _ ->
            name == name2

        Verbatim name2 _ _ ->
            name == name2

        _ ->
            False


matchExprOnName_ : String -> Expr -> Bool
matchExprOnName_ name expr =
    case expr of
        Fun name2 _ _ ->
            String.startsWith name name2

        Verbatim name2 _ _ ->
            String.startsWith name name2

        _ ->
            False


matchingIdsInAST : String -> Forest ExpressionBlock -> List String
matchingIdsInAST key ast =
    ast |> List.map Tree.flatten |> List.concat |> List.filterMap (idOfMatchingBlockContent key)


idOfMatchingBlockContent : String -> ExpressionBlock -> Maybe String
idOfMatchingBlockContent key (ExpressionBlock { sourceText, id }) =
    if String.contains key sourceText then
        Just id

    else
        Nothing


titleTOC : Forest ExpressionBlock -> List ExpressionBlock
titleTOC ast =
    filterBlocksByArgs "title" ast


existsBlockWithName : List (Tree.Tree ExpressionBlock) -> String -> Bool
existsBlockWithName ast name =
    let
        mBlock =
            ast
                |> List.map Tree.flatten
                |> List.concat
                |> filterBlocksOnName name
                |> List.head
    in
    case mBlock of
        Nothing ->
            False

        Just _ ->
            True


{-| Return the text content of the first element with the given name
-}
filterASTOnName : List (Tree.Tree ExpressionBlock) -> String -> List String
filterASTOnName ast name =
    let
        mBlock =
            ast
                |> List.map Tree.flatten
                |> List.concat
                |> filterBlocksOnName name
                |> List.head
    in
    case mBlock of
        Nothing ->
            []

        Just (ExpressionBlock { content }) ->
            case content of
                Left str ->
                    [ str ]

                Right exprList ->
                    List.map getText exprList |> Maybe.Extra.values


getBlockByName : String -> List (Tree.Tree ExpressionBlock) -> Maybe ExpressionBlock
getBlockByName name ast =
    ast
        |> List.map Tree.flatten
        |> List.concat
        |> filterBlocksOnName name
        |> List.head


runninghead ast =
    getBlockByName "runninghead" ast


banner ast =
    getBlockByName "banner" ast


frontMatterDict : List (Tree ExpressionBlock) -> Dict String String
frontMatterDict ast =
    keyValueDict (getVerbatimBlockValue "docinfo" ast |> String.split "\n" |> fixFrontMatterList)


keyValueDict : List String -> Dict String String
keyValueDict strings_ =
    List.map (String.split ":") strings_
        |> List.map (List.map String.trim)
        |> List.map pairFromList
        |> Maybe.Extra.values
        |> Dict.fromList


pairFromList : List String -> Maybe ( String, String )
pairFromList strings =
    case strings of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


fixFrontMatterList : List String -> List String
fixFrontMatterList strings =
    loop { count = 1, input = strings, output = [] } nextStepFix
        |> List.reverse
        |> handleEmptyDocInfo


handleEmptyDocInfo : List String -> List String
handleEmptyDocInfo strings =
    if strings == [ "(docinfo)" ] then
        [ "date:" ]

    else
        strings


type alias FixState =
    { count : Int, input : List String, output : List String }


nextStepFix : FixState -> Step FixState (List String)
nextStepFix state =
    case List.head state.input of
        Nothing ->
            Done state.output

        Just line ->
            if line == "" then
                Loop { state | input = List.drop 1 state.input }

            else if String.left 7 line == "author:" then
                Loop
                    { state
                        | input = List.drop 1 state.input
                        , output = String.replace "author:" ("author" ++ String.fromInt state.count ++ ":") line :: state.output
                        , count = state.count + 1
                    }

            else
                Loop { state | input = List.drop 1 state.input, output = line :: state.output }


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s nextState_ =
    case nextState_ s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b


getVerbatimBlockValue : String -> List (Tree.Tree ExpressionBlock) -> String
getVerbatimBlockValue key ast =
    case getBlockByName key ast of
        Nothing ->
            "(" ++ key ++ ")"

        Just (ExpressionBlock { content }) ->
            case content of
                Left str ->
                    str

                Right _ ->
                    "(" ++ key ++ ")"


getBlockArgsByName : String -> List (Tree.Tree ExpressionBlock) -> List String
getBlockArgsByName key ast =
    case getBlockByName key ast of
        Nothing ->
            []

        Just (ExpressionBlock { args }) ->
            args


getValue : String -> List (Tree.Tree ExpressionBlock) -> String
getValue key ast =
    case getBlockByName key ast of
        Nothing ->
            "(" ++ key ++ ")"

        Just (ExpressionBlock { content }) ->
            case content of
                Left str ->
                    str

                Right exprList ->
                    List.map getText exprList |> Maybe.Extra.values |> String.join ""


title : Forest ExpressionBlock -> String
title ast =
    getValue "title" ast


extractTextFromSyntaxTreeByKey : String -> Forest ExpressionBlock -> String
extractTextFromSyntaxTreeByKey key syntaxTree =
    syntaxTree |> filterBlocksByArgs key |> expressionBlockToText


tableOfContents : Int -> Forest ExpressionBlock -> List ExpressionBlock
tableOfContents maximumLevel ast =
    filterBlocksOnName "section" (List.map Tree.flatten ast |> List.concat)


filterBlocksByArgs : String -> Forest ExpressionBlock -> List ExpressionBlock
filterBlocksByArgs key ast =
    ast
        |> List.map Tree.flatten
        |> List.concat
        |> List.filter (matchBlock key)


matchBlock : String -> ExpressionBlock -> Bool
matchBlock key (ExpressionBlock { blockType }) =
    case blockType of
        Paragraph ->
            False

        OrdinaryBlock args ->
            List.any (String.contains key) args

        VerbatimBlock args ->
            List.any (String.contains key) args


exprListToStringList : List Expr -> List String
exprListToStringList exprList =
    List.map getText exprList
        |> Maybe.Extra.values
        |> List.map String.trim
        |> List.filter (\s -> s /= "")


getText : Expr -> Maybe String
getText text =
    case text of
        Text str _ ->
            Just str

        Verbatim _ str _ ->
            Just (String.replace "`" "" str)

        Fun _ expressions _ ->
            List.map getText expressions |> Maybe.Extra.values |> String.join " " |> Just


stringValueOfList : List Expr -> String
stringValueOfList textList =
    String.join " " (List.map stringValue textList)


stringValue : Expr -> String
stringValue text =
    case text of
        Text str _ ->
            str

        Fun _ textList _ ->
            String.join " " (List.map stringValue textList)

        Verbatim _ str _ ->
            str


expressionBlockToText : List ExpressionBlock -> String
expressionBlockToText =
    toExprRecord >> List.map .content >> List.concat >> List.filterMap getText >> String.join " "



-- toExprListList : List L0BlockE -> List (List Expr)


toExprRecord : List ExpressionBlock -> List { content : List Expr, blockType : BlockType }
toExprRecord blocks =
    List.map toExprList_ blocks



-- toExprList_ : L0BlockE -> List Expr


toExprList_ : ExpressionBlock -> { content : List Expr, blockType : BlockType }
toExprList_ (ExpressionBlock { blockType, content }) =
    { content = content |> Either.toList |> List.concat, blockType = blockType }
