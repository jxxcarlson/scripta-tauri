module L0.Parser.Classify exposing (classify, isVerbatimLine)

import Parser.Block exposing (BlockType(..))
import Parser.Common exposing (Classification)
import Parser.Line exposing (PrimitiveBlockType(..))
import Parser.PrimitiveBlock exposing (PrimitiveBlock)


isVerbatimLine : String -> Bool
isVerbatimLine str =
    (String.left 2 str == "||")
        || (String.left 3 str == "```")
        || (String.left 2 str == "$$")


classify : PrimitiveBlock -> Classification
classify block =
    let
        bt =
            case block.blockType of
                PBParagraph ->
                    Paragraph

                PBOrdinary ->
                    OrdinaryBlock args

                PBVerbatim ->
                    VerbatimBlock args

        args =
            block.args

        name =
            block.name
    in
    { blockType = bt, args = args, name = name }
