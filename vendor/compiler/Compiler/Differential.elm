module Compiler.Differential exposing (EditRecord, differentialCompiler, init, update)

import Compiler.Differ as Differ


type alias EditRecord chunk parsedChunk renderedChunk =
    { chunks : List chunk
    , parsed : List parsedChunk
    , rendered : List renderedChunk
    }


init : (String -> List chunk) -> (chunk -> parsedChunk) -> (parsedChunk -> renderedChunk) -> String -> EditRecord chunk parsedChunk renderedChunk
init chunker parser renderer text =
    let
        chunks =
            chunker text

        parsed =
            List.map parser chunks

        rendered =
            List.map renderer parsed
    in
    { chunks = chunks, parsed = parsed, rendered = rendered }


{-| The update function takes an EditRecord and a string, the "text",
breaks the text into a list of logical paragraphs, diffs it with the list
of paragraphs held by the EditRecord, uses `differentialRender` to
render the changed paragraphs while copying the unchanged rendered paragraphsto
prodduce an updated list of rendered paragraphs. The 'differentialRender'
accomplishes this using the transformer. The seed is used to produces
a differential idList. This last step is perhaps unnecessary. To investigate.
(This was part of an optimization scheme.)
-}
update :
    (String -> List chunk)
    -> (chunk -> parsedChunk)
    -> (parsedChunk -> renderedChunk)
    -> EditRecord chunk parsedChunk renderedChunk
    -> String
    -> EditRecord chunk parsedChunk renderedChunk
update chunker parser renderer editRecord text =
    let
        newChunks =
            chunker text

        diffRecord =
            Differ.diff editRecord.chunks newChunks

        ( parsed, rendered ) =
            differentialCompiler parser renderer diffRecord editRecord
    in
    { chunks = newChunks, parsed = parsed, rendered = rendered }


differentialCompiler :
    (chunk -> parsedChunk)
    -> (parsedChunk -> renderedChunk)
    -> Differ.DiffRecord chunk
    -> EditRecord chunk parsedChunk renderedChunk
    -> ( List parsedChunk, List renderedChunk )
differentialCompiler parser renderer diffRecord editRecord =
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

        initialSegmentRendered =
            List.take ii editRecord.rendered

        terminalSegmentRendered =
            takeLast it editRecord.rendered

        middleSegmentRendered =
            -- TODO: to improve!
            List.map (parser >> renderer) diffRecord.middleSegmentInTarget
    in
    ( initialSegmentParsed ++ middleSegmentParsed ++ terminalSegmentParsed
    , initialSegmentRendered ++ middleSegmentRendered ++ terminalSegmentRendered
    )


takeLast : Int -> List a -> List a
takeLast k x =
    x |> List.reverse |> List.take k |> List.reverse
