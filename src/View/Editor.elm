module View.Editor exposing (inputCursor, view)

import Document
import Element as E exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Keyed
import Html
import Html.Attributes as HtmlAttr
import Html.Events
import Json.Decode
import Json.Encode
import Model exposing (AppMode(..), Model, Msg(..))
import Scripta.Language
import View.Geometry as Geometry


view : Model -> Element Msg
view model =
    Element.Keyed.el
        [ -- RECEIVE INOFRMATION FROM CODEMIRROR
          E.htmlAttribute onSelectionChange
        , E.htmlAttribute onTextChange
        , E.htmlAttribute onCursorChange

        --
        , htmlId "editor-here"
        , E.width (E.px 550)
        , E.height (E.px Geometry.appHeight)
        , case model.mode of
            EditorMode ->
                E.width (E.px Geometry.editorWidth)

            ReaderMode ->
                E.width (E.px 0)
        , Background.color (E.rgb255 0 68 85)
        , Font.color (E.rgb 0.85 0.85 0.85)
        , Font.size 12
        ]
        ( stringOfBool True
        , E.html
            (Html.node "codemirror-editor"
                [ -- SEND INFORMATION TO CODEMIRROR
                  HtmlAttr.attribute "text" model.initialText
                , HtmlAttr.attribute "editordata" (fixEditorData model.language model.editorData |> encodeEditorData)
                , HtmlAttr.attribute "selection" (stringOfBool model.doSync)
                ]
                []
            )
        )



-- EDITOR


encodeEditorData : { begin : Int, end : Int } -> String
encodeEditorData { begin, end } =
    Json.Encode.object
        [ ( "begin", Json.Encode.int begin )
        , ( "end", Json.Encode.int end )
        ]
        |> Json.Encode.encode 2


fixEditorData : Scripta.Language.Language -> { begin : Int, end : Int } -> { begin : Int, end : Int }
fixEditorData lang { begin, end } =
    case lang of
        Scripta.Language.MicroLaTeXLang ->
            { begin = begin + 1, end = end + 1 }

        _ ->
            { begin = begin, end = end }


stringOfBool bool =
    case bool of
        False ->
            "false"

        True ->
            "true"


inputCursor : { position : Int, source : String } -> Model -> ( Model, Cmd Msg )
inputCursor { position, source } model =
    ( model, Cmd.none )


htmlId str =
    E.htmlAttribute (HtmlAttr.id str)


onCursorChange : Html.Attribute Msg
onCursorChange =
    dataDecoder
        |> Json.Decode.map InputCursor
        |> Html.Events.on "cursor-change"


onTextChange : Html.Attribute Msg
onTextChange =
    dataDecoder
        |> Json.Decode.map InputText
        |> Html.Events.on "text-change"


onSelectionChange : Html.Attribute Msg
onSelectionChange =
    textDecoder
        |> Json.Decode.map SelectedText
        |> Html.Events.on "selected-text"


dataDecoder : Json.Decode.Decoder Document.SourceTextRecord
dataDecoder =
    dataDecoder_
        |> Json.Decode.at [ "detail" ]


dataDecoder_ : Json.Decode.Decoder Document.SourceTextRecord
dataDecoder_ =
    Json.Decode.map2 Document.SourceTextRecord
        (Json.Decode.field "position" Json.Decode.int)
        (Json.Decode.field "source" Json.Decode.string)


textDecoder : Json.Decode.Decoder String
textDecoder =
    Json.Decode.string
        |> Json.Decode.at [ "detail" ]
