module View.Button exposing (
            printToPDF
            , tarFile
            , setDocument
            , newFile
            , createFile
            , cancelNewFile
            , openFile
            , saveDocument
            , setLanguage
            , refresh
            , rawExport
      )

import View.ButtonTemplate
import Element exposing (..)
import Model exposing(Model, Msg(..))
import PDF
import Scripta.API
import Element.Font as Font
import Document exposing(Document)
import Element.Background as Background
import Element.Events
import Color
import Html.Attributes
import Scripta.Language exposing (Language(..))

-- BUTTONS


buttonWidth =
    160


rawExport :Element Msg
rawExport = 
    View.ButtonTemplate.template
        { tooltipText = "export raw LaTeX"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = RawExport
        , label = "Raw"
        }    

refresh :Element Msg
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

        labelName = case newLanguage of 
          L0Lang -> "L0"
          MicroLaTeXLang -> "MicroLaTeX"
          XMarkdownLang -> "XMarkdown"
          _ -> "??"

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
            View.ButtonTemplate.simpleTemplate [ width (px buttonWidth), elementAttribute "title" "Export PDF" , Font.color Color.black] PrintToPDF "PDF"

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

newFile :  Element Msg
newFile  =
    View.ButtonTemplate.template
        { tooltipText = "Make new file"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = NewFile
        , label = "New"
        }

createFile :  Element Msg
createFile  =
    View.ButtonTemplate.template
        { tooltipText = "Create new file"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = CreateFile
        , label = "Create"
        }

cancelNewFile :  Element Msg
cancelNewFile  =
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
        foo = 1
    in
    View.ButtonTemplate.template
        { tooltipText = "Open document"
        , tooltipPlacement = below
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = ListDirectory "scripta"
        , label = "Open"
        }        
saveDocument : Document -> Element Msg
saveDocument document =
    let
        foo = 1
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
