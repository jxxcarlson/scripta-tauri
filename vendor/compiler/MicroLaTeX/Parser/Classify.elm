module MicroLaTeX.Parser.Classify exposing (classify)

import List.Extra
import Parser.Block exposing (BlockType(..))
import Parser.Common exposing (Classification)
import Parser.PrimitiveBlock exposing (PrimitiveBlock)


classify : PrimitiveBlock -> Classification
classify block =
    case block.name of
        Just "item" ->
            { blockType = OrdinaryBlock [ "item" ], args = [], name = Just "item" }

        Just "index" ->
            { blockType = OrdinaryBlock [ "index" ], args = [], name = Just "index" }

        Just "abstract" ->
            { blockType = OrdinaryBlock [ "abstract" ], args = [], name = Just "abstract" }

        Just "numbered" ->
            { blockType = OrdinaryBlock [ "numbered" ], args = [], name = Just "numbered" }

        Just "desc" ->
            { blockType = OrdinaryBlock [ "desc" ], args = block.args, name = Just "desc" }

        Just name_ ->
            if List.member name_ Parser.Common.verbatimBlockNames then
                { blockType = VerbatimBlock [ name_ ], args = block.args, name = Just name_ }

            else
                { blockType = OrdinaryBlock [ name_ ], args = block.args, name = Just name_ }

        Nothing ->
            case List.Extra.getAt 1 block.content of
                Just "$$" ->
                    { blockType = VerbatimBlock [ "math" ], args = [], name = Just "math" }

                Just "```" ->
                    { blockType = VerbatimBlock [ "code" ], args = [], name = Just "code" }

                _ ->
                    { blockType = Paragraph, args = [], name = block.name }
