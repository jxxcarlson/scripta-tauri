port module Main exposing (main)

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

{-| 
    Outbound port
-}
port sendDocument : Document -> Cmd a

type alias Model =
    { count : Int
    , document : Document
    , editRecord : Scripta.API.EditRecord
    , language : Language
    , documentType : DocumentType
    , currentTime : Time.Posix
    , printingState : PDF.PrintingState
    , tarFileState : PDF.TarFileState
    , message : String
    , ticks : Int
    , popupState : PopupState
    }

type PopupState = NewDocumentWindowOpen | NoPopups

type DocumentType
    = InfoDocument
    | TestDocument
    | Example


type Msg
    = NoOp
    | InputText String
    | Render Scripta.API.Msg
    | PDF PDFMsg
    | SetExampleDocument String
    | Info
    | Export
    | PrintToPDF
    | GotPdfLink (Result Http.Error String)
    | GotTarFile (Result Http.Error String)
    | GetTarFile
    | ChangePrintingState PDF.PrintingState
    | ChangeTarFileState PDF.TarFileState
    | SendDocument
    | Tick Time.Posix
    | NewFile
    | ClosePopup


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
    ( { count = 0
      , document = { content = Text.about, name = "about.L0"}
      , editRecord = Scripta.API.init Dict.empty L0Lang Text.about
      , language = L0Lang
      , documentType = Example
      , currentTime = Time.millisToPosix 0
      , printingState = PDF.PrintWaiting
      , tarFileState = PDF.TarFileWaiting
      , message = "Starting up"
      , ticks = 0
      , popupState = NoPopups
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
                | document = Document.updateContent str model.document
                , count = model.count + 1
                , editRecord = Scripta.API.update model.editRecord str
              }
            , Cmd.none
            )


        SetExampleDocument documentName ->
            let
                doc =
                    case documentName of
                        "demo.L0" ->
                            { content = Text.l0Demo, name = documentName}

                        "demo.tex" ->
                          { content = Text.microLaTeXDemo, name = documentName}
                            

                        "demo.md" ->
                          { content = Text.xMarkdown, name = documentName}
                          

                        _ ->
                             { content = Text.nada, name = "nada.L0"}
            in
            model |> loadDocument doc |> (\m -> (m, Cmd.batch [ jumpToTop "scripta-output", jumpToTop "input-text" ]))
            

        Info ->
         model |> loadDocument {content = Text.about, name = "info.L0"} 
               |> (\m -> (m, Cmd.batch [ jumpToTop "scripta-output", jumpToTop "input-text" ]))
            

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

       -- PORTS

        SendDocument -> 
            let 
               message = "Document " ++ model.document.name ++ " saved to Desktop/scripta"
            in
            ( {model | message = message }, sendDocument model.document)

        NewFile ->
          ({model | popupState = NewDocumentWindowOpen}, Cmd.none)

        ClosePopup -> 
           ({model | popupState = NoPopups}, Cmd.none)

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
        [ column [  spacing 18, width (px 1200), height fill, Element.htmlAttribute (Html.Attributes.style "max-height" "100vh")  ]
            [   header model
                ,row [ spacing 18, height fill, Element.htmlAttribute (Html.Attributes.style "max-height" "100vh") ]
                [ inputText model
                , displayRenderedText model
                , controls model
                ]
            , footer model
            ]
       ]



header model = row [paddingXY 20 0
    , spacing 18
    , width fill
    , height (px 40)
    , Font.size 14
    , Background.color Color.black
    , Font.color Color.white
   
    ]  
  [
     el [] (text <| "Document: " ++ model.document.name)
  ]


footer model = row [ inFront (newDocument model), paddingXY 20 0, spacing 18, width fill, height (px 40), Font.size 14, Background.color Color.black, Font.color Color.white]  [
     el [] (text <| model.message)
  ]  
controlSpacing = 6

controls model =
    column [ alignTop
           , spacing 8
           , paddingXY 16 22
           , height fill
           , Element.htmlAttribute (Html.Attributes.style "max-height" "100vh")
           , scrollbarY
           , width (px 120)  ]
        [ 
           setDocumentButton "about.L0" model.document.name
        , el [paddingXY 0 controlSpacing]  (text "")
        , newFileButton
        , openFileButton
        , saveDocumentButton model.document
        , el [ paddingXY 0 controlSpacing ] (text "")
        , tarFileButton model
        , printToPDF model
        , el [paddingXY 0 controlSpacing]  (text "")
        , el [Font.size 16, Font.color Color.white] (text "Sample docs")
        , setDocumentButton "demo.L0" model.document.name
        , setDocumentButton "demo.tex" model.document.name
        , setDocumentButton "demo.md" model.document.name
       
        ]


newDocument model = 
  case model.popupState of 
     NewDocumentWindowOpen -> 
        column [  Background.color Color.jetBlack
                , Font.color Color.white
                , moveRight 740
                , moveUp 720
                , paddingXY 20 20
                , width (px 300)
                , height (px 450) ] [
            el [] (text "New document")
            , el [alignBottom] (cancelNewFileButton)

            ]

     _ -> Element.none

windowHeight = 700

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
        , text = model.document.content
        , placeholder = Nothing
        , label = Input.labelAbove [ fontGray 0.9 ] <| el [] (text "Source text")
        , spellcheck = False
        }
 )



-- HELPERS

loadDocument : Document -> Model -> Model
loadDocument doc model = 
  { model | document = doc
          , editRecord = Scripta.API.init Dict.empty ( Document.language doc) doc.content
          , language = Document.language doc 
          , count = model.count + 1}


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




setDocumentButton : String -> String -> Element Msg
setDocumentButton documentName currentDocumentName =
    let
        bgColor =
            if documentName == currentDocumentName then
                darkRed

            else
                gray
    in
    Button.template
        { tooltipText = "Set the markup language"
        , tooltipPlacement = above
        , attributes = [ Font.color white, Background.color bgColor, width (px buttonWidth) ]
        , msg = SetExampleDocument documentName
        , label = documentName
        }

newFileButton :  Element Msg
newFileButton  =
    Button.template
        { tooltipText = "Make new file"
        , tooltipPlacement = above
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = NewFile
        , label = "New"
        }

cancelNewFileButton :  Element Msg
cancelNewFileButton  =
    Button.template
        { tooltipText = "Cancel new file"
        , tooltipPlacement = above
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = ClosePopup
        , label = "Cancel"
        }

openFileButton : Element Msg
openFileButton =
    let
        foo = 1
    in
    Button.template
        { tooltipText = "Open docuemnt"
        , tooltipPlacement = above
        , attributes = [ Font.color white, Background.color gray, width (px buttonWidth) ]
        , msg = NoOp
        , label = "Open"
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
