module MicroLaTeX.Parser.Transform exposing (macroArg, transform)

import Compiler.Util
import Dict exposing (Dict)
import Parser as P
import Parser.Line exposing (PrimitiveBlockType(..))
import Parser.PrimitiveBlock exposing (PrimitiveBlock)


pseudoBlockNamesWithContent =
    [ "title", "section", "subsection", "subsubsection", "subheading", "setcounter", "contents" ]


sectionDict : Dict String String
sectionDict =
    Dict.fromList
        [ ( "section", "1" )
        , ( "subsection", "2" )
        , ( "subsubsection", "3" )
        , ( "subheading", "4" )
        ]


{-|

        { indent = 0
        , lineNumber = 123
        , position = 4561
        , content = ["\section{Introduction}"]
        , name = Nothing
        , args = []
        , properties = Dict.empty
        , sourceText "\section{Introduction}"
        , blockType = PBParagraph
        }

        -->

        { indent = 0
        , lineNumber = 123
        , position = 4561
        , content = ["Introduction"]
        , name = Just "section"
        , args = ["1"]
        , properties = Dict.empty
        , sourceText "\section{Introduction}"
        , blockType = PBOrdinaryBlock
        }

-}
transform : PrimitiveBlock -> PrimitiveBlock
transform block =
    let
        normalizedContent =
            block.content
                |> List.map String.trimLeft
                |> normalize
    in
    case normalizedContent of
        firstLine :: _ ->
            let
                name =
                    if String.left 1 firstLine == "\\" then
                        String.dropLeft 1 firstLine |> String.split "{" |> List.head |> Maybe.withDefault "---"

                    else
                        firstLine

                arg : Maybe String
                arg =
                    case P.run (Compiler.Util.macroValParserX name) firstLine of
                        Ok result ->
                            Just (result |> String.dropRight 1)

                        Err _ ->
                            Nothing
            in
            if List.member name pseudoBlockNamesWithContent then
                handlePseudoBlockWithContent block name arg

            else
                block

        _ ->
            block


macroArg : String -> String -> String
macroArg macroName str =
    String.replace ("\\" ++ macroName ++ "{") "" str |> String.dropRight 1


handlePseudoBlockWithContent : PrimitiveBlock -> String -> Maybe String -> PrimitiveBlock
handlePseudoBlockWithContent block name maybeArg =
    case maybeArg of
        Nothing ->
            { block
                | content = [] -- ("| section " ++ val) :: [ str ]
                , args = []
                , name = Just name
                , blockType = PBOrdinary
            }

        Just arg ->
            case Dict.get name sectionDict of
                Nothing ->
                    { block
                        | content = [ arg ] --("| " ++ macroName) :: [ str ]
                        , name = Just name
                        , args = [ arg ]
                        , blockType = PBOrdinary
                    }

                Just val ->
                    { block
                        | content = [ arg ] -- ("| section " ++ val) :: [ str ]
                        , args = val :: []
                        , name = Just "section"
                        , blockType = PBOrdinary
                    }


normalize : List String -> List String
normalize list =
    case list of
        "" :: rest ->
            rest

        _ ->
            list
