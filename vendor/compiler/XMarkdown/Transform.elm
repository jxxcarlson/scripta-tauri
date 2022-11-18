module XMarkdown.Transform exposing (transform)

import Compiler.Util
import List.Extra
import Parser.Line exposing (PrimitiveBlockType(..))
import Parser.PrimitiveBlock exposing (PrimitiveBlock)


transform : PrimitiveBlock -> PrimitiveBlock
transform block =
    let
        normalizedContent =
            block.content |> List.map (String.dropLeft block.indent) |> normalize
    in
    case normalizedContent of
        firstLine :: rest_ ->
            if String.left 1 firstLine == "#" then
                handleTitle block firstLine

            else if String.left 2 firstLine == "$$" then
                handleMath block

            else if String.left 3 firstLine == "```" then
                handleVerbatim block rest_

            else if String.left 2 firstLine == "- " then
                handleItem block (String.dropLeft 2 firstLine) rest_

            else if String.left 2 firstLine == ". " then
                handleNumberedItem block (String.dropLeft 2 firstLine) rest_

            else if String.left 2 firstLine == "> " then
                handleQuotation block firstLine

            else if block.blockType == PBVerbatim then
                { block | name = Just "code", content = List.filter (\line -> line /= "```") block.content }

            else
                block

        _ ->
            block


handleItem block firstLine rest =
    { block | name = Just "item", blockType = PBOrdinary, content = firstLine :: rest }


handleNumberedItem block firstLine rest =
    { block | name = Just "numbered", blockType = PBOrdinary, content = firstLine :: rest }


handleVerbatim : PrimitiveBlock -> List String -> PrimitiveBlock
handleVerbatim block rest =
    { block
        | name = Just "code"
        , blockType = PBVerbatim
        , content =
            if Maybe.map String.trim (List.Extra.last rest) == Just "```" then
                Compiler.Util.dropLast block.content

            else
                block.content
    }


handleMath : PrimitiveBlock -> PrimitiveBlock
handleMath block =
    { block | name = Just "math", blockType = PBVerbatim, content = List.filter (\item -> item /= "") block.content }


handleTitle : PrimitiveBlock -> String -> PrimitiveBlock
handleTitle block firstLine =
    let
        words =
            String.split " " firstLine
    in
    case Maybe.map String.length (List.head words) of
        Nothing ->
            block

        Just 0 ->
            block

        --Just 1 ->
        --    { block | args = [ "1" ], blockType = PBOrdinary, name = Just "title", content = [ "| title", String.join " " (List.drop 1 words) ] }
        Just n ->
            let
                first =
                    "| section " ++ String.fromInt n

                level =
                    String.fromInt n
            in
            { block | args = [ level ], blockType = PBOrdinary, name = Just "section", content = [ String.join " " (List.drop 1 words) ] }


handleQuotation block firstLine =
    let
        args : List String
        args =
            firstLine |> String.dropLeft 2 |> String.words
    in
    { block | args = args, content = String.join " " args :: List.drop 1 block.content, blockType = PBOrdinary, name = Just "quotation" }



-- RESIDUE


normalize : List String -> List String
normalize list =
    case list of
        "" :: rest ->
            rest

        _ ->
            list
