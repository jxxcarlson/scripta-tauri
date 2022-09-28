module View.Main exposing (view)

import Render.Msg exposing(MarkupMsg(..))
import Browser
import Config
import View.Utility
import Browser.Dom
import Scripta.Language exposing(Language(..))
import Color
import Maybe.Extra
import Model exposing(Model, Msg(..), PopupState(..))
import View.Editor
import Html exposing(Html)
import Html.Attributes
import Element exposing (..)
import Element.Background as Background
import Element.Events
import Element.Font as Font
import Element.Input as Input
import View.Button
import Scripta.API
import Scripta.API exposing (EditRecord)
import Model exposing (AppMode(..))


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
                [ -- inputText model
                View.Editor.view model
                , case model.mode of 
                   EditorMode -> Element.none
                   ReaderMode -> Element.column [width (px 300)] []
                , displayRenderedText model
                , case model.mode of
                    EditorMode -> editorControls model
                    ReaderMode -> readerControls model
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
     , row [alignRight, spacing 8] [
            case model.mode of
                EditorMode -> Element.none
                ReaderMode -> View.Button.openFile
            ]
           , View.Button.setMode model.mode EditorMode
           , View.Button.setMode model.mode ReaderMode
         
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

editorControls : Model -> Element Msg
editorControls model =
    column [ alignTop
           , spacing 8
           , paddingXY 16 22
           , height fill
           , Element.htmlAttribute (Html.Attributes.style "max-height" "100vh")
           , scrollbarY
           , width (px 160)  ]
        [ 
           View.Button.setDocument "About" "about.L0" model.document.name
        , el [paddingXY 0 controlSpacing]  (text "")
        , View.Button.newFile
        , View.Button.openFile
        , View.Button.saveDocument model.document
        , View.Button.refresh
        , el [ paddingXY 0 controlSpacing ] (text "")
        , View.Button.tarFile model
        , View.Button.rawExport
        , View.Button.printToPDF model
        , el [paddingXY 0 controlSpacing]  (text "")
        , el [Font.size 16, Font.color Color.white] (text "Sample docs")
        , View.Button.setDocument "L0" "demo.L0" model.document.name
        , View.Button.setDocument "MicroLaTeX" "demo.tex" model.document.name
        , View.Button.setDocument "XMarkdown" "demo.md" model.document.name
       
        ]

readerControls : Model -> Element Msg
readerControls model =
    column [ alignTop
           , spacing 8
           , paddingEach {left = 120, right = 0, top = 20, bottom = 20}
           , height fill
           , Element.htmlAttribute (Html.Attributes.style "max-height" "100vh")
           , scrollbarY
           , width (px 160)  ]
        [ 
          
        
       
        ]

newDocument : Model -> Element Msg
newDocument model = 
  case model.popupState of 
     NewDocumentWindowOpen -> 
        column [  Background.color Color.jetBlack
                , Font.color Color.white
                , moveRight 740
                , moveUp 720
                , paddingXY 20 20
                , spacing 18
                , width (px 400)
                , height (px 305) ] [
            el [Font.size 18] (text "New document")
            , inputNewFileName model 
            , column [spacing 8, paddingEach {top = 12, bottom = 0, left = 70, right = 0}] [
                  View.Button.setLanguage model.language L0Lang
                , View.Button.setLanguage model.language MicroLaTeXLang
                , View.Button.setLanguage model.language XMarkdownLang
            ]
            , row [spacing 18, alignBottom] [View.Button.createFile, View.Button.cancelNewFile]
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
        , case model.mode of 
            EditorMode -> width (px 500)
            ReaderMode -> width (px 500)
        , height (px windowHeight), Element.htmlAttribute (Html.Attributes.style "max-height" "100vh")
        , paddingXY 16 32
        , scrollbarY
        , htmlId Config.renderedTextViewportID
        ]
        (Scripta.API.render (settings model.selectedId model.count) model.editRecord |> List.map (Element.map (RenderMarkupMsg)))
        ]

htmlId : String -> Attribute msg
htmlId str =
    htmlAttribute (Html.Attributes.id str)


settings : String -> Int -> { windowWidth : number, counter : Int, selectedId : String, selectedSlug : Maybe b, scale : Float }
settings selectedId_ counter =
    { windowWidth = 500
    , counter = counter
    , selectedId = selectedId_
    , selectedSlug = Nothing
    , scale = 0.8
    }


inputNewFileName : Model -> Element Msg
inputNewFileName model =
    Input.text [ width (px 300), height (px 30), paddingXY 4 5, Font.size 14, alignTop, Font.color Color.black ]
        { onChange = InputNewFileName
        , text = model.inputFilename
        , placeholder = Nothing
        , label = Input.labelLeft [ fontGray 0.9 ] <| el [] (text "File name ")
        }
 


--
-- HELPERS
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

