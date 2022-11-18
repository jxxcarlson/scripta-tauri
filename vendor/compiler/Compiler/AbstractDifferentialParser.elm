module Compiler.AbstractDifferentialParser exposing (EditRecord, UpdateFunctions, differentialParser, init, update)

import Compiler.Acc
import Compiler.Differ
import Compiler.DifferEq
import Dict exposing (Dict)
import Scripta.Language exposing (Language)
import Tree exposing (Tree)


type alias EditRecord chunk parsedChunk accumulator =
    { chunks : List chunk
    , parsed : List parsedChunk
    , tree : List (Tree parsedChunk)
    , accumulator : accumulator
    , lang : Language
    , messages : List String
    , initialData : InitialData
    }


type alias UpdateFunctions chunk parsedChunk acc =
    { chunker : String -> List chunk
    , chunkEq : chunk -> chunk -> Bool
    , chunkLevel : chunk -> Int
    , chunkParser : chunk -> parsedChunk
    , forestFromBlocks : List parsedChunk -> List (Tree parsedChunk)
    , getMessages : List (Tree parsedChunk) -> List String
    , accMaker : Compiler.Acc.InitialAccumulatorData -> List (Tree parsedChunk) -> ( acc, List (Tree parsedChunk) )
    }


type alias InitialData =
    { language : Language
    , mathMacros : String
    , textMacros : String
    , vectorSize : Int
    }


init :
    UpdateFunctions chunk parsedChunk acc
    -> InitialData
    -> String
    -> EditRecord chunk parsedChunk acc
init f initialData content =
    let
        chunks =
            f.chunker content

        parsed_ =
            List.map f.chunkParser chunks

        tree_ =
            f.forestFromBlocks parsed_

        ( newAccumulator, tree ) =
            f.accMaker initialData tree_
    in
    { lang = initialData.language
    , chunks = chunks
    , parsed = parsed_
    , tree = tree
    , accumulator = newAccumulator
    , messages = f.getMessages tree
    , initialData = initialData
    }


{-| The update function takes an EditRecord and a string, the "text",
breaks the text into a list of logical paragraphs, diffs it with the list
of paragraphs held by the EditRecord, uses `differentialRender` to
render the changed paragraphs while copying the unchanged rendered paragraphs to
produce an updated list of rendered paragraphs. The 'differentialRender'
accomplishes this using the transformer. The seed is used to produces
a differential idList. This last step is perhaps unnecessary. To investigate.
(This was part of an optimization scheme.)
-}
update :
    UpdateFunctions chunk parsedChunk acc
    -> EditRecord chunk parsedChunk accumulator
    -> String
    -> EditRecord chunk parsedChunk acc
update f editRecord sourceText =
    let
        newChunks =
            f.chunker sourceText

        diffRecord : Compiler.Differ.DiffRecord chunk
        diffRecord =
            Compiler.DifferEq.diff f.chunkEq f.chunkLevel editRecord.chunks newChunks

        parsed_ =
            differentialParser f.chunkParser diffRecord editRecord

        tree_ =
            f.forestFromBlocks parsed_

        ( newAccumulator, tree ) =
            f.accMaker editRecord.initialData tree_
    in
    -- TODO: real update of accumulator
    { lang = editRecord.lang
    , chunks = newChunks
    , parsed = parsed_
    , tree = tree
    , accumulator = newAccumulator
    , messages = f.getMessages tree
    , initialData = editRecord.initialData
    }


differentialParser :
    (chunk -> parsedChunk)
    -> Compiler.Differ.DiffRecord chunk
    -> EditRecord chunk parsedChunk acc
    -> List parsedChunk
differentialParser parser diffRecord editRecord =
    let
        ii =
            List.length diffRecord.commonInitialSegment

        it =
            List.length diffRecord.commonTerminalSegment

        initialSegmentParsed =
            List.take ii editRecord.parsed

        terminalSegmentParsed =
            takeLast it editRecord.parsed

        middleSegmentParsed =
            List.map parser diffRecord.middleSegmentInTarget
    in
    initialSegmentParsed ++ middleSegmentParsed ++ terminalSegmentParsed


takeLast : Int -> List a -> List a
takeLast k x =
    x |> List.reverse |> List.take k |> List.reverse
