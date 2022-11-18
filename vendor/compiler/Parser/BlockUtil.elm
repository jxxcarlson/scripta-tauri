module Parser.BlockUtil exposing
    ( getMessages
    , l0Empty
    , toExpressionBlock
    )

import Compiler.Util
import Dict
import Either exposing (Either(..))
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr)
import Parser.Line exposing (PrimitiveBlockType(..))
import Parser.PrimitiveBlock exposing (PrimitiveBlock)
import Scripta.Language exposing (Language)


l0Empty =
    ExpressionBlock
        { name = Nothing
        , args = []
        , properties = Dict.empty
        , indent = 0
        , lineNumber = 0
        , id = "0"
        , tag = ""
        , numberOfLines = 0
        , blockType = Paragraph
        , content = Left "YYY"
        , messages = []
        , sourceText = "YYY"
        , error = Nothing
        }


getMessages : ExpressionBlock -> List String
getMessages (ExpressionBlock { messages }) =
    messages


toExpressionBlock : Language -> (Int -> String -> ( List Expr, List String )) -> PrimitiveBlock -> ExpressionBlock
toExpressionBlock lang parse { name, args, properties, indent, error, lineNumber, blockType, content, sourceText } =
    let
        blockType_ =
            toBlockType blockType (List.drop 1 args)

        ( exprs, messages ) =
            mapContent parse lineNumber blockType_ (String.join "\n" content)
    in
    ExpressionBlock
        { name = name
        , args = args
        , properties = properties
        , indent = indent
        , lineNumber = lineNumber
        , numberOfLines = List.length content
        , id = String.fromInt lineNumber
        , tag = Compiler.Util.getItem lang "label" sourceText
        , blockType = blockType_
        , content = exprs
        , messages = messages -- MicroLaTeX.Parser.Expression.parseToState lineNumber sourceText |> MicroLaTeX.Parser.Expression.extractMessages
        , sourceText = sourceText
        , error = error
        }


mapContent : (Int -> String -> ( List Expr, List String )) -> Int -> BlockType -> String -> ( Either String (List Expr), List String )
mapContent parse lineNumber blockType content =
    let
        ( parsed, messages ) =
            parse lineNumber content
    in
    ( mapContentAux blockType parsed content, messages )


mapContentAux : BlockType -> List Expr -> String -> Either String (List Expr)
mapContentAux blockType parsed content =
    case blockType of
        Paragraph ->
            Right parsed

        OrdinaryBlock _ ->
            Right parsed

        VerbatimBlock _ ->
            let
                content_ =
                    if blockType == VerbatimBlock [ "code" ] then
                        Left (String.replace "```" "" content)

                    else
                        Left content
            in
            content_


toBlockType : PrimitiveBlockType -> List String -> BlockType
toBlockType pbt args =
    case pbt of
        PBParagraph ->
            Paragraph

        PBOrdinary ->
            OrdinaryBlock args

        PBVerbatim ->
            VerbatimBlock args



-- UNUSED
