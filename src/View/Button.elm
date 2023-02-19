module View.Button exposing
    ( cancelNewFile
    , createFile
    , newFile
    , openFile
    , printToPDF
    , rawExport
    , refresh
    , saveDocument
    , setDocument
    , setLanguage
    , setMode
    , syncLR
    , tarFile
    )

import Color
import Document exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Events
import Element.Font as Font
import Html.Attributes
import Model exposing (AppMode(..), Model, Msg(..))
import PDF
import Scripta.API
import Scripta.Language exposing (Language(..))
import View.ButtonTemplate



-- BUTTONS


buttonWidth =
    160


setMode : AppMode -> AppMode -> Element Msg
setMode currentAppMode newAppMode =
    let
        bg =
            if currentAppMode == newAppMode then
                Background.color Color.dullRed

            else
                Background.color Color.black

        label =
            case newAppMode of
                EditorMode ->
                    "Edit"

                ReaderMode ->
                    "Read"

        tooltipText =
            case newAppMode of
                EditorMode ->
                    "Go to Edit mode"

                ReaderMode ->
                    "go to Read mode"
    in
    View.ButtonTemplate.template
        { tooltipText = tooltipText
        , tooltipPlacement = below
        , attributes = [ Font.color white, bg, width (px 60) ]
        , msg = SetAppMode newAppMode
        , label = label
        }


rawExport : Element Msg
rawExport =
    View.ButtonTemplate.template
        { tooltipText = "export raw LaTeX"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = RawExport
        , label = "Raw"
        }


syncLR : Element Msg
syncLR =
    View.ButtonTemplate.template
        { tooltipText = "Sync rendered text to source text"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = StartSync
        , label = "->"
        }


refresh : Element Msg
refresh =
    View.ButtonTemplate.template
        { tooltipText = "Recompile source text"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = Refresh
        , label = "Refresh"
        }


setLanguage : Language -> Language -> Element Msg
setLanguage currentLanguage newLanguage =
    let
        bgColor =
            if currentLanguage == newLanguage then
                darkRed

            else
                gray

        labelName =
            case newLanguage of
                L0Lang ->
                    "L0"

                MicroLaTeXLang ->
                    "MicroLaTeX"

                XMarkdownLang ->
                    "XMarkdown"

                _ ->
                    "??"
    in
    View.ButtonTemplate.template
        { tooltipText = "Set markup language"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color bgColor, width (px buttonWidth) ]
        , msg = SetLanguage newLanguage
        , label = labelName
        }


printToPDF : Model -> Element Msg
printToPDF model =
    case model.printingState of
        PDF.PrintWaiting ->
            View.ButtonTemplate.simpleTemplate [ width (px buttonWidth), elementAttribute "title" "Export PDF", Font.color Color.black ] PrintToPDF "PDF"

        PDF.PrintProcessing ->
            el [ Font.size 14, padding 8, height (px 30), Background.color Color.blue ] (text "Please wait ...")

        PDF.PrintReady ->
            link
                [ Font.size 14
                , Background.color Color.white
                , paddingXY 8 8
                , Font.color Color.blue
                , Element.Events.onClick (ChangePrintingState PDF.PrintWaiting)
                , elementAttribute "target" "_blank"
                ]
                { url = PDF.pdfServUrl ++ Scripta.API.fileNameForExport model.editRecord.tree, label = el [] (text "Click for PDF") }


tarFile : Model -> Element Msg
tarFile model =
    case model.tarFileState of
        PDF.TarFileWaiting ->
            View.ButtonTemplate.simpleTemplate [ width (px buttonWidth), elementAttribute "title" "Export LaTeX" ] GetTarFile "Export"

        PDF.TarFileProcessing ->
            el [ Font.size 14, padding 8, height (px 30), Background.color Color.blue, Font.color Color.white ] (text "Please wait ...")

        PDF.TarFileReady ->
            link
                [ Font.size 14
                , Background.color Color.white
                , paddingXY 8 8
                , Font.color Color.blue
                , Element.Events.onClick (ChangeTarFileState PDF.TarFileProcessing)
                , elementAttribute "target" "_blank"
                ]
                { url = PDF.tarArchiveUrl ++ (Scripta.API.fileNameForExport model.editRecord.tree |> String.replace ".tex" ".tar"), label = el [] (text "Click for Tar file") }


elementAttribute : String -> String -> Attribute msg
elementAttribute key value =
    htmlAttribute (Html.Attributes.attribute key value)


setDocument labelName documentName currentDocumentName =
    let
        bgColor =
            if documentName == currentDocumentName then
                darkRed

            else
                gray
    in
    View.ButtonTemplate.template
        { tooltipText = "Read-only doc"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color bgColor, width (px buttonWidth) ]
        , msg = SetExampleDocument documentName
        , label = labelName
        }


newFile : Element Msg
newFile =
    View.ButtonTemplate.template
        { tooltipText = "Make new file"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = NewFile
        , label = "New"
        }


createFile : Element Msg
createFile =
    View.ButtonTemplate.template
        { tooltipText = "Create new file"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = CreateFile
        , label = "Create"
        }


cancelNewFile : Element Msg
cancelNewFile =
    View.ButtonTemplate.template
        { tooltipText = "Cancel new file"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = ClosePopup
        , label = "Cancel"
        }


openFile : Element Msg
openFile =
    let
        foo =
            1
    in
    View.ButtonTemplate.template
        { tooltipText = "Open document"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = OpenFile "scripta"
        , label = "Open"
        }


saveDocument : Document -> Element Msg
saveDocument document =
    let
        foo =
            1
    in
    View.ButtonTemplate.template
        { tooltipText = "Save docuemnt"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = SendDocument
        , label = "Save"
        }


darkRed : Color
darkRed =
    rgb255 140 0 0


gray : Color
gray =
    rgb255 60 60 60


white =
    rgb255 255 255 255
