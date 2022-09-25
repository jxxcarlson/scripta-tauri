port module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is revers
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Browser.Dom
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
import Json.Encode
import Json.Decode
import PDF exposing (PDFMsg(..))
import Scripta.API
import Scripta.Language exposing (Language(..))
import Document exposing(Document)
import Task
import Text
import Time
import List.Extra
import Browser.Navigation exposing (load)
import String exposing (toInt)
import Button
import Model exposing(Model, Msg(..), Flags, PopupState(..))

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Time.every 500 ExportTick
        , Time.every 3000 DocumentSaveTick
        , receiveDocument (Json.Decode.decodeValue documentDecoder >> DocumentReceived)
    ]

autosave model = 
  if model.documentNeedsSaving  then
     ({model | documentNeedsSaving = False }, sendDocument model.document)
  else
     (model, Cmd.none)

-- PORTS, OUTBOUND
port sendDocument : Document -> Cmd a

port listDirectory : String -> Cmd a

-- PORTS, INBOUND
port receiveDocument : (Json.Encode.Value -> msg) -> Sub msg

documentDecoder : Json.Decode.Decoder Document
documentDecoder =
    Json.Decode.map3 Document 
      (Json.Decode.field "content" Json.Decode.string)
      (Json.Decode.field "name" Json.Decode.string)
      (Json.Decode.field "path" Json.Decode.string)



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
      , document = { content = Text.about, name = "about.L0", path = "NONE"}
      , documentNeedsSaving = False
      , editRecord = Scripta.API.init Dict.empty L0Lang Text.about
      , language = L0Lang
      , currentTime = Time.millisToPosix 0
      , printingState = PDF.PrintWaiting
      , tarFileState = PDF.TarFileWaiting
      , message = "Starting up"
      , ticks = 0
      , popupState = NoPopups
      , newFilename = ""
      }
    , Cmd.batch [ jumpToTop "scripta-output", jumpToTop "input-text" ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DocumentSaveTick _ -> 
          autosave model

        ExportTick newTime ->
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
            ({ model
                | currentTime = newTime
                , ticks = ticks
                , tarFileState = tarFileState
                , printingState = printingState
              } , Cmd.none)

        InputText str ->
            ( { model
                | document = Document.updateContent str model.document
                , count = model.count + 1
                , editRecord = Scripta.API.update model.editRecord str
                , documentNeedsSaving = True
              }
            , Cmd.none
            )


        SetExampleDocument documentName ->
            let
                doc =
                    case documentName of
                        "demo.L0" ->
                            { content = Text.l0Demo, name = documentName, path = "NONE"}

                        "demo.tex" ->
                          { content = Text.microLaTeXDemo, name = documentName,  path = "NONE"}
                            

                        "demo.md" ->
                          { content = Text.xMarkdown, name = documentName, path = "NONE"}

                        "about.L0" ->
                          { content = Text.about, name = documentName, path = "NONE"}
                          

                        _ ->
                             { content = Text.nada, name = "nada.L0",  path = "NONE"}
            in
            model |> loadDocument doc |> (\m -> (m, Cmd.batch [ jumpToTop "scripta-output", jumpToTop "input-text" ]))
               

        GetTarFile ->
           let
             defaultSettings = Scripta.API.defaultSettings
            in
                ( { model
                    | ticks = 0
                    , tarFileState = PDF.TarFileProcessing
                    , message = "requesting TAR file"
                  }
                , PDF.tarCmd model.currentTime { defaultSettings | isStandaloneDocument = True } model.editRecord.tree
                    |> Cmd.map PDF
                )

        --
        PDF _ ->
            ( model, Cmd.none )

        Model.GotPdfLink result ->
            ( { model | printingState = PDF.PrintReady, message = "Got PDF Link" }, Cmd.none )

        Model.ChangePrintingState printingState ->
            ( { model | printingState = printingState, message = "Changing printing state" }, Cmd.none )

        PrintToPDF ->
            let
                defaultSettings =
                    Scripta.API.defaultSettings

                exportSettings =
                    { defaultSettings | isStandaloneDocument = True }
            in
            ( { model | ticks = 0, printingState = PDF.PrintProcessing, message = "requesting PDF" }, PDF.printCmd model.currentTime exportSettings model.editRecord.tree |> Cmd.map PDF )

        Model.GotTarFile result ->
            ( { model | printingState = PDF.PrintReady, message = "Got TarFile" }, Cmd.none )

        Model.ChangeTarFileState tarFileState ->
            ( { model | tarFileState = tarFileState, message = "Changing tar file state" }, Cmd.none )

        Render _ ->
            ( model, Cmd.none )

        Export ->
            ( model, Cmd.none )

       -- PORTS

        SendDocument -> 
            let 
               message =  if model.document.path == "NONE" then
                             "Document is read-only"
                          else "Saved as Desktop/" ++ model.document.path
            in
            if model.document.path == "NONE" then
              ({model | message = message, documentNeedsSaving = False} , Cmd.none)
            else
              ( {model | message = message, documentNeedsSaving = False }, sendDocument model.document)

        ListDirectory dir -> 
            ( model , listDirectory dir)

           

        NewFile ->
          ({model | popupState = NewDocumentWindowOpen}, Cmd.none)

        InputNewFileName str -> 
          ({ model | newFilename = str}, Cmd.none)

        ClosePopup -> 
           ({model | popupState = NoPopups}, Cmd.none)

        CreateFile -> 
          {model | popupState = NoPopups} 
            |> loadDocument { name = model.newFilename, content = "new document\n", path = "scripta/" ++ model.newFilename}
            |> (\m -> (m, Cmd.none))

        DocumentReceived result ->
         case result of 
           Err _ -> ({ model | message = "Error opening document"}, Cmd.none)
           Ok doc -> 
              case List.Extra.unconsLast (doc.name |> String.split "/") of 
                    Nothing -> ({ model | message = "Error opening document"}, Cmd.none)
                    Just (name_, _) -> 
                       {model | message = "Document opened"} 
                       |> loadDocument {doc | name = name_, path = fixPath doc.path}
                       |> (\m -> (m, Cmd.none))
                    
            
             
            
fixPath : String -> String
fixPath str = 
   str 
     |> String.split "/"
     |> List.Extra.dropWhile (\s -> s /= "Desktop")
     |> List.drop 1
     |> String.join "/"

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
        el [] (text <| "Words: " ++ (String.words model.document.content |> List.length |> String.fromInt))
     ,  documentNeedsSavingIndicator model.documentNeedsSaving
     ,  el [] (text <| model.message)
  ]


indicatorSize = 8

documentNeedsSavingIndicator : Bool -> Element Msg
documentNeedsSavingIndicator needsSaving = 
  if needsSaving  then 
     el [width (px indicatorSize), height (px indicatorSize), Background.color Color.red] (text "")
  else 
     el [width (px indicatorSize), height (px indicatorSize), Background.color Color.green] (text "")

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
           Button.setDocument "About" "about.L0" model.document.name
        , el [paddingXY 0 controlSpacing]  (text "")
        , Button.newFile
        , Button.openFile
        , Button.saveDocument model.document
        , el [ paddingXY 0 controlSpacing ] (text "")
        , Button.tarFile model
        , Button.printToPDF model
        , el [paddingXY 0 controlSpacing]  (text "")
        , el [Font.size 16, Font.color Color.white] (text "Sample docs")
        , Button.setDocument "L0" "demo.L0" model.document.name
        , Button.setDocument "MicroLaTeX" "demo.tex" model.document.name
        , Button.setDocument "XMarkdown" "demo.md" model.document.name
       
        ]


newDocument model = 
  case model.popupState of 
     NewDocumentWindowOpen -> 
        column [  Background.color Color.jetBlack
                , Font.color Color.white
                , moveRight 740
                , moveUp 720
                , paddingXY 20 20
                , spacing 24
                , width (px 300)
                , height (px 220) ] [
            el [] (text "New document")
            , inputNewFileName model 
            , Button.createFile
            , el [alignBottom] (Button.cancelNewFile)

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


inputNewFileName : Model -> Element Msg
inputNewFileName model =
    Input.text [ width (px 170), height (px 30), paddingXY 4 5, Font.size 14, alignTop, Font.color Color.black ]
        { onChange = InputNewFileName
        , text = model.newFilename
        , placeholder = Nothing
        , label = Input.labelLeft [ fontGray 0.9 ] <| el [] (text "File name ")
        }
 


-- HELPERS

loadDocument : Document -> Model -> Model
loadDocument doc model = 
  { model | document = doc
          , documentNeedsSaving = False
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
