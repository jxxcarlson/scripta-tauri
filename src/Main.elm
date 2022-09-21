module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is revers
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Browser.Dom
import Button
import Color
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Events
import Element.Font as Font
import Element.Input as Input
import File.Download
import Html exposing (Html)
import Html.Attributes
import Http
import PDF exposing (PDFMsg(..))
import Scripta.API
import Scripta.Language exposing (Language(..))
import Document exposing(Document)
import Task
import Text
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 180 Tick


type alias Model =
    { input : String
    , count : Int
    , document : Document
    , editRecord : Scripta.API.EditRecord
    , language : Language
    , documentType : DocumentType
    , currentTime : Time.Posix
    , printingState : PDF.PrintingState
    , tarFileState : PDF.TarFileState
    , message : String
    , ticks : Int
    }


type DocumentType
    = InfoDocument
    | TestDocument
    | Example


type Msg
    = NoOp
    | InputText String
    | Render Scripta.API.Msg
    | PDF PDFMsg
    | SetLanguage Language
    | SetDocument
    | Info
    | Export
    | PrintToPDF
    | GotPdfLink (Result Http.Error String)
    | GotTarFile (Result Http.Error String)
    | GetTarFile
    | ChangePrintingState PDF.PrintingState
    | ChangeTarFileState PDF.TarFileState
    | Tick Time.Posix


type alias Flags =
    {}


settings : a -> { windowWidth : number, counter : a, selectedId : String, selectedSlug : Maybe b, scale : Float }
settings counter =
    { windowWidth = 500
    , counter = counter
    , selectedId = "--"
    , selectedSlug = Nothing
    , scale = 0.8
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = Text.microLaTeXDemo
      , count = 0
      , document = Document.default
      , editRecord = Scripta.API.init Dict.empty MicroLaTeXLang Text.microLaTeXDemo
      , language = MicroLaTeXLang
      , documentType = Example
      , currentTime = Time.millisToPosix 0
      , printingState = PDF.PrintWaiting
      , tarFileState = PDF.TarFileWaiting
      , message = "Starting up"
      , ticks = 0
      }
    , Cmd.batch [ jumpToTop "scripta-output", jumpToTop "input-text" ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            let
                printingState =
                    if model.printingState == PDF.PrintProcessing && model.ticks > 2 then
                        PDF.PrintReady

                    else if model.printingState == PDF.PrintReady && model.ticks > 10 then
                        PDF.PrintWaiting

                    else
                        model.printingState

                tarFileState =
                    if model.tarFileState == PDF.TarFileProcessing && model.ticks > 2 then
                        PDF.TarFileReady

                    else if model.tarFileState == PDF.TarFileReady && model.ticks > 10 then
                        PDF.TarFileWaiting

                    else
                        model.tarFileState

                ticks =
                    if model.ticks > 10 then
                        0

                    else
                        model.ticks + 1
            in
            ( { model
                | currentTime = newTime
                , ticks = ticks
                , tarFileState = tarFileState
                , printingState = printingState
              }
            , Cmd.none
            )

        InputText str ->
            ( { model
                | input = str
                , count = model.count + 1
                , editRecord = Scripta.API.update model.editRecord str
              }
            , Cmd.none
            )

        SetDocument ->
            ( { model
                | language = MicroLaTeXLang
                , editRecord = Scripta.API.init Dict.empty MicroLaTeXLang Text.testFile
                , input = Text.testFile
                , count = model.count + 1
                , documentType = TestDocument
              }
            , Cmd.batch [ jumpToTop "scripta-output", jumpToTop "input-text" ]
            )

        SetLanguage lang ->
            let
                docText =
                    case lang of
                        L0Lang ->
                            Text.l0Demo

                        MicroLaTeXLang ->
                            Text.microLaTeXDemo

                        XMarkdownLang ->
                            Text.xMarkdown

                        PlainTextLang ->
                            ""
            in
            ( { model
                | language = lang
                , editRecord = Scripta.API.init Dict.empty lang docText
                , input = docText
                , count = model.count + 1
                , documentType = Example
              }
            , Cmd.batch [ jumpToTop "scripta-output", jumpToTop "input-text" ]
            )

        Info ->
            ( { model
                | language = L0Lang
                , editRecord = Scripta.API.init Dict.empty L0Lang Text.info
                , input = Text.info
                , count = model.count + 1
                , documentType = InfoDocument
              }
            , Cmd.batch [ jumpToTop "scripta-output", jumpToTop "input-text" ]
            )

        GetTarFile ->
            let
                defaultSettings_ =
                    Scripta.API.defaultSettings

                exportSettings_ =
                    { defaultSettings_ | isStandaloneDocument = True }
            in
            if Scripta.API.getImageUrls model.editRecord.tree == [] then
                let
                    defaultSettings =
                        Scripta.API.defaultSettings

                    exportSettings =
                        { defaultSettings | isStandaloneDocument = True }

                    exportText =
                        Scripta.API.prepareContentForExport model.currentTime exportSettings model.editRecord.tree

                    fileName =
                        Scripta.API.fileNameForExport model.editRecord.tree
                in
                ( model, download fileName exportText )

            else
                ( { model
                    | ticks = 0
                    , tarFileState = PDF.TarFileProcessing
                    , message = "requesting TAR file"
                  }
                , PDF.tarCmd model.currentTime exportSettings_ model.editRecord.tree
                    |> Cmd.map PDF
                )

        --
        PDF _ ->
            ( model, Cmd.none )

        GotPdfLink result ->
            ( { model | printingState = PDF.PrintReady, message = "Got PDF Link" }, Cmd.none )

        ChangePrintingState printingState ->
            ( { model | printingState = printingState, message = "Changing printing state" }, Cmd.none )

        PrintToPDF ->
            let
                defaultSettings =
                    Scripta.API.defaultSettings

                exportSettings =
                    { defaultSettings | isStandaloneDocument = True }
            in
            ( { model | ticks = 0, printingState = PDF.PrintProcessing, message = "requesting PDF" }, PDF.printCmd model.currentTime exportSettings model.editRecord.tree |> Cmd.map PDF )

        GotTarFile result ->
            ( { model | printingState = PDF.PrintReady, message = "Got TarFile" }, Cmd.none )

        ChangeTarFileState tarFileState ->
            ( { model | tarFileState = tarFileState, message = "Changing tar file state" }, Cmd.none )

        Render _ ->
            ( model, Cmd.none )

        Export ->
            ( model, Cmd.none )


download : String -> String -> Cmd msg
download fileName fileContents =
    File.Download.string fileName "application/x-tex" fileContents



--
-- VIEW
--


noFocus : FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)


view : Model -> Html Msg
view model =
    layoutWith { options = [ focusStyle noFocus ] }
        [ bgGray 0.2, height fill, Element.htmlAttribute (Html.Attributes.style "max-height" "100vh")  ]
        (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ spacing 18, width (px 1200), height fill, Element.htmlAttribute (Html.Attributes.style "max-height" "100vh")  ]
            [ --  title "Compiler Demo"
                row [ spacing 18, height fill, Element.htmlAttribute (Html.Attributes.style "max-height" "100vh") ]
                [ inputText model
                , displayRenderedText model
                , controls model
                ]
            , row [ width (px 1200), Font.color (rgb 1 1 1), height (px 40), Font.size 14]
                [ text <| model.message
                ]
            ]
       ]


controls model =
    column [ alignTop, spacing 18, paddingXY 16 22, height fill, Element.htmlAttribute (Html.Attributes.style "max-height" "100vh"), scrollbarY, width (px 120)  ]
        [ saveDocumentButton model.document
        , el [paddingXY 0 10]  (text "")
        , setLanguageButton "L0" model.documentType L0Lang model.language
        , setLanguageButton "MicroLaTeX" model.documentType MicroLaTeXLang model.language
        , setLanguageButton "XMarkdown" model.documentType XMarkdownLang model.language
        , el [ paddingXY 0 40 ] (infoButton model.documentType)
        , testFileButton model.documentType
        , tarFileButton model
        , printToPDF model
        ]


title : String -> Element msg
title str =
    row [ centerX, height (px 40), Font.bold, fontGray 0.9 ] [ text str ]

windowHeight = 650

displayRenderedText : Model -> Element Msg
displayRenderedText model =
    column [ spacing 8, Font.size 14,  alignTop,  height (px windowHeight), Element.htmlAttribute (Html.Attributes.style "max-height" "100vh"), scrollbarY, width (px 500) ]
        [ el [ fontGray 0.9 ] (text "Rendered Text")
         ,column
        [ spacing 18
        , Font.size 14
        , Background.color (Element.rgb 1.0 1.0 1.0)
        , width (px 500)
        , height (px windowHeight), Element.htmlAttribute (Html.Attributes.style "max-height" "100vh")
        , paddingXY 16 32
        , scrollbarY
        , htmlId "scripta-output"
        ]
        (Scripta.API.render (settings model.count) model.editRecord |> List.map (Element.map Render))
        ]


inputText : Model -> Element Msg
inputText model =
 el [ width (px 500),  height (px windowHeight), alignTop, Element.htmlAttribute (Html.Attributes.style "max-height" "100vh"),  scrollbarY] (
    Input.multiline [ width (px 500), height (px windowHeight),  scrollbarY, Font.size 14, alignTop, htmlId "input-text" ]
        { onChange = InputText
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelAbove [ fontGray 0.9 ] <| el [] (text "Source text")
        , spellcheck = False
        }
 )



-- VIEWPORT


htmlId : String -> Attribute msg
htmlId str =
    htmlAttribute (Html.Attributes.id str)


jumpToTop : String -> Cmd Msg
jumpToTop id =
    Browser.Dom.getViewportOf id
        |> Task.andThen (\info -> Browser.Dom.setViewportOf id 0 0)
        |> Task.attempt (\_ -> NoOp)



-- BUTTONS


buttonWidth =
    105


printToPDF : Model -> Element Msg
printToPDF model =
    case model.printingState of
        PDF.PrintWaiting ->
            Button.simpleTemplate [ width (px buttonWidth), elementAttribute "title" "Generate PDF" ] PrintToPDF "PDF"

        PDF.PrintProcessing ->
            el [ Font.size 14, padding 8, height (px 30), Background.color Color.blue, Font.color Color.white ] (text "Please wait ...")

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


tarFileButton : Model -> Element Msg
tarFileButton model =
    case model.tarFileState of
        PDF.TarFileWaiting ->
            Button.simpleTemplate [ width (px buttonWidth), elementAttribute "title" "Get Tar File" ] GetTarFile "Export"

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


testFileButton : DocumentType -> Element Msg
testFileButton documentType =
    let
        bgColor =
            case documentType of
                InfoDocument ->
                    gray

                Example ->
                    gray

                TestDocument ->
                    darkRed
    in
    Button.template
        { tooltipText = "Load Test file"
        , tooltipPlacement = above
        , attributes = [ Font.color white, Background.color bgColor, width (px buttonWidth) ]
        , msg = SetDocument
        , label = "Test Doc"
        }


infoButton : DocumentType -> Element Msg
infoButton documentType =
    let
        bgColor =
            case documentType of
                InfoDocument ->
                    darkRed

                Example ->
                    gray

                TestDocument ->
                    gray
    in
    Button.template
        { tooltipText = "Info on the Scripta compiler"
        , tooltipPlacement = above
        , attributes = [ Font.color white, Background.color bgColor, width (px buttonWidth) ]
        , msg = Info
        , label = "About"
        }


setLanguageButton : String -> DocumentType -> Language -> Language -> Element Msg
setLanguageButton label documentType language currentLanguage =
    let
        bgColor =
            if language == currentLanguage && documentType == Example then
                darkRed

            else
                gray
    in
    Button.template
        { tooltipText = "Set the markup language"
        , tooltipPlacement = above
        , attributes = [ Font.color white, Background.color bgColor, width (px buttonWidth) ]
        , msg = SetLanguage language
        , label = label
        }

saveDocumentButton : Document -> Element Msg
saveDocumentButton document =
    let
        foo = 1
    in
    Button.template
        { tooltipText = "Save current docuemnt"
        , tooltipPlacement = above
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = NoOp
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



--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , bgGray 0.4
    , paddingXY 20 20
    , height fill, Element.htmlAttribute (Html.Attributes.style "max-height" "100vh")
    ]
