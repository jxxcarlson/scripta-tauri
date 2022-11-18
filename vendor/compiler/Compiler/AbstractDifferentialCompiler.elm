module Compiler.AbstractDifferentialCompiler exposing (EditRecord, differentialParser, init, update)

-- See commit 5e24aba for why we can't use Element from elm-ui in the edit record (hidden type definition)

import Compiler.Differ as Differ
import Scripta.Language exposing (Language)


type alias EditRecord chunk parsedChunk renderedChunk accumulator =
    { chunks : List chunk
    , parsed : List parsedChunk
    , rendered : List renderedChunk
    , accumulator : accumulator
    , lang : Language
    }


init :
    Language
    -> (String -> List chunk)
    -> (Language -> List chunk -> ( acc, List parsedChunk ))
    -> (Language -> ( acc, List parsedChunk ) -> List renderedChunk)
    -> String
    -> EditRecord chunk parsedChunk renderedChunk acc
init lang chunker accMaker renderer text =
    let
        chunks =
            chunker text

        ( newAccumulator, parsed ) =
            accMaker lang chunks

        rendered =
            renderer lang ( newAccumulator, parsed )
    in
    { lang = lang, chunks = chunks, parsed = parsed, rendered = rendered, accumulator = newAccumulator }


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
    (String -> List chunk)
    -> (chunk -> parsedChunk)
    -> (Language -> List parsedChunk -> ( acc, List parsedChunk ))
    -> (Language -> ( acc, List parsedChunk ) -> List renderedChunk)
    -> EditRecord chunk parsedChunk renderedChunk acc
    -> String
    -> EditRecord chunk parsedChunk renderedChunk acc
update chunker parser accMaker renderer editRecord text =
    let
        newChunks =
            chunker text

        diffRecord =
            Differ.diff editRecord.chunks newChunks

        parseRecord : DiffRecord parsedChunk
        parseRecord =
            differentialParser parser diffRecord editRecord

        ( newAccumulator, parsed ) =
            accMaker editRecord.lang (parseRecord.initial ++ parseRecord.middle ++ parseRecord.terminal)

        renderRecord =
            differentialRenderer (renderer editRecord.lang) parseRecord editRecord

        rendered =
            renderRecord.initial ++ renderRecord.middle ++ renderRecord.terminal
    in
    -- TODO: real update of accumulator
    { lang = editRecord.lang, chunks = newChunks, parsed = parsed, accumulator = newAccumulator, rendered = rendered }


type alias DiffRecord chunk =
    { initial : List chunk, middle : List chunk, terminal : List chunk }


differentialParser :
    (chunk -> parsedChunk)
    -> Differ.DiffRecord chunk
    -> EditRecord chunk parsedChunk renderedChunk acc
    -> DiffRecord parsedChunk
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
    { initial = initialSegmentParsed, middle = middleSegmentParsed, terminal = terminalSegmentParsed }


differentialRenderer :
    (( acc, List parsedChunk ) -> List renderedChunk)
    -> DiffRecord parsedChunk
    -> EditRecord chunk parsedChunk renderedChunk acc
    -> DiffRecord renderedChunk
differentialRenderer renderer diffRecord editRecord =
    let
        ii =
            List.length diffRecord.initial

        it =
            List.length diffRecord.terminal

        initialSegmentRendered =
            List.take ii editRecord.rendered

        terminalSegmentRendered =
            takeLast it editRecord.rendered

        middleSegmentRendered =
            renderer ( editRecord.accumulator, diffRecord.middle )
    in
    { initial = initialSegmentRendered, middle = middleSegmentRendered, terminal = terminalSegmentRendered }


takeLast : Int -> List a -> List a
takeLast k x =
    x |> List.reverse |> List.take k |> List.reverse
