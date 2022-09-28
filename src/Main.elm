port module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is revers
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import View.Main
import Process
import View.Utility
import Browser.Dom
import Color
import Maybe.Extra
import Dict exposing(Dict)
import View.Editor
import Element exposing (..)
import Element.Background as Background
import Element.Events
import Element.Font as Font
import Element.Input as Input
import File.Download
import Html exposing (Html)
import Html.Attributes
import Html.Events
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
import Model exposing(Model, Msg(..), Flags, PopupState(..))
import Keyboard

main =
    Browser.element
        { init = init
        , view = View.Main.view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Time.every 500 ExportTick
        , Time.every 3000 DocumentSaveTick
        , receiveDocument (Json.Decode.decodeValue documentDecoder >> DocumentReceived)
        , receivePreferences (Json.Decode.decodeValue preferencesDecoder >> PreferencesReceived)
    ]

autosave model = 
  if model.documentNeedsSaving  then
     ({model | documentNeedsSaving = False }, sendDocument model.document)
  else
     (model, Cmd.none)

-- PORTS, OUTBOUND
port readPreferences : String ->  Cmd a
port writePreferences : String -> Cmd a
port sendDocument : Document -> Cmd a

port listDirectory : String -> Cmd a

-- PORTS, INBOUND
port receiveDocument : (Json.Encode.Value -> msg) -> Sub msg
port receivePreferences : (Json.Encode.Value -> msg) -> Sub msg

documentDecoder : Json.Decode.Decoder Document
documentDecoder =
    Json.Decode.map3 Document 
      (Json.Decode.field "content" Json.Decode.string)
      (Json.Decode.field "name" Json.Decode.string)
      (Json.Decode.field "path" Json.Decode.string)


preferencesDecoder : Json.Decode.Decoder String
preferencesDecoder =
      (Json.Decode.field "preferences" Json.Decode.string)



initialDoc = { content = "\\title{Welcome!}\n\nPress  \\{strong{About} to continue\n", path = "NONE", name = "start.tex"}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { count = 0
      , document = { content = Text.about, name = "about.L0", path = "NONE"}
      , initialText = "??????"
      , linenumber = 0
      , doSync = False
      , pressedKeys = []
      , documentNeedsSaving = False
      , editRecord = Scripta.API.init Dict.empty L0Lang Text.about
      , language = MicroLaTeXLang
      , currentTime = Time.millisToPosix 0
      , printingState = PDF.PrintWaiting
      , tarFileState = PDF.TarFileWaiting
      , message = "Starting up"
      , ticks = 0
      , popupState = NoPopups
      , newFilename = ""
      , inputFilename = ""
      , preferences = Dict.empty
      , homeDirectory = Nothing
      }
    , Cmd.batch [ 
            jumpToTop "scripta-output"
          , jumpToTop "input-text", readPreferences "foo"
          , delayCmd 1 (SetExampleDocument "about.L0")
     ]

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

        InputText { position, source } ->
          ({model |   editRecord = Scripta.API.update model.editRecord source
                    , document = Document.updateContent source model.document
                    , count = model.count + 1
                    , documentNeedsSaving = True}
            , Cmd.none)

        InputCursor { position, source } ->
            View.Editor.inputCursor { position = position, source = source } model

        SelectedText str ->
            firstSyncLR model str



        -- InputText { position, source } ->
        --             Frontend.Editor.inputText model { position = position, source = source }

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

        RawExport ->
          let
                doc = { name = fileName, path = path, content = content}

                defaultSettings =
                    Scripta.API.defaultSettings

                exportSettings =
                    { defaultSettings | isStandaloneDocument = True }
                content = Scripta.API.rawExport  exportSettings model.editRecord.tree


                rawFileName = model.document.name 
                   |> String.split "." 
                   |> List.reverse 
                   |> List.drop 1
                   |> List.reverse 
                   |> String.join "."
                fileName = rawFileName ++ "-raw.tex"

                path = "scripta/" ++ fileName

          in
          
            ( { model | message = "Saved " ++ fileName}, sendDocument doc)

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
          ({model | popupState = NewDocumentWindowOpen, inputFilename = "", newFilename = ""}, Cmd.none)


        SetLanguage lang -> ({ model | language = lang}, Cmd.none)

        InputNewFileName str -> 
          ({ model | inputFilename = str}, Cmd.none)

        ClosePopup -> 
           ({model | popupState = NoPopups}, Cmd.none)

        CreateFile -> 
          let
              newFilename = case model.language of 
                 L0Lang -> model.inputFilename ++ ".L0"
                 MicroLaTeXLang -> model.inputFilename ++ ".tex"
                 XMarkdownLang -> model.inputFilename ++ ".md"
                 _  -> ".tex"

              languageName =  case model.language of 
                 L0Lang -> "L0"
                 MicroLaTeXLang -> "MicroLaTeX"
                 XMarkdownLang -> "XMarkdown"
                 _  -> "MicroLaTeX"
              newPreferences = Dict.insert "language" languageName model.preferences
              preferenceString = Dict.toList newPreferences |> List.map (\(a,b) -> a ++ ": " ++ b) |> String.join "\n"
          in
          {model | popupState = NoPopups, preferences = newPreferences} 
            |> loadDocument { name = newFilename, content = "new document\n", path = "scripta/" ++ newFilename}
            |> (\m -> ({m | newFilename = newFilename, documentNeedsSaving = True}, writePreferences preferenceString))

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

        PreferencesReceived result ->
         case result of 
           Err _ -> ({ model | preferences = Dict.empty, message = "Error getting preferences"}, Cmd.none)
           Ok prefs -> 
             let
                 preferences = extractPrefs prefs
                 language = case getLanguage preferences of 
                    Just lang -> lang
                    Nothing -> model.language

             in
             ({ model | message = "Preferences: " ++ String.replace "\n" ", " prefs, preferences = preferences, language = language}, Cmd.none)
              
                    
        Refresh ->
           ( { model | editRecord = Scripta.API.init Dict.empty ( Document.language model.document) model.document.content }, Cmd.none )
            
        SyncLR ->
            syncLR model

        SetViewPortForElement _ -> (model, Cmd.none)

extractPrefs : String -> Dict String String
extractPrefs data = 
   data
     |> String.lines
     |> List.map (String.split ":")
     |> List.map (List.map String.trim)
     |> List.filter (\line -> List.length line == 2)
     |> List.map listToTuple
     |> Maybe.Extra.values
     |> Dict.fromList

listToTuple : List a -> Maybe (a, a) 
listToTuple list = 
   case list of 
    (first::second::[]) ->Just  (first, second)
    _ -> Nothing



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



-- HELPERS

loadDocument : Document -> Model -> Model
loadDocument doc model = 
  { model | document = Document.updateContent doc.content doc
          , documentNeedsSaving = False
          , initialText = doc.content
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





-- HELPERS

getLanguage : Dict String String -> Maybe Language
getLanguage dict = 
   case Dict.get "language" dict of 
    Just "L0" -> Just L0Lang
    Just "MicroLaTeX" -> Just MicroLaTeXLang
    Just "XMarkdown" -> Just XMarkdownLang
    _ -> Nothing

updateKeys model keyMsg =
    let
        pressedKeys =
            Keyboard.update keyMsg model.pressedKeys

        doSync =
            if List.member Keyboard.Control pressedKeys && List.member (Keyboard.Character "S") pressedKeys then
                not model.doSync

            else
                model.doSync
    in
    ( { model | pressedKeys = pressedKeys, doSync = doSync, lastInteractionTime = model.currentTime }
    , Cmd.none
    )

delayCmd : Float -> msg -> Cmd msg
delayCmd delay msg =
    Task.perform (\_ -> msg) (Process.sleep delay)

syncLR : Model -> (Model, Cmd Msg )
syncLR model =
  (model, Cmd.none)
    -- let
    --     data =
    --         if model.foundIdIndex == 0 then
    --             let
    --                 foundIds_ =
    --                     Scripta.API.matchingIdsInAST model.searchSourceText model.editRecord.tree

    --                 id_ =
    --                     List.head foundIds_ |> Maybe.withDefault "(nothing)"
    --             in
    --             { foundIds = foundIds_
    --             , foundIdIndex = 1
    --             , cmd = View.Utility.setViewportForElement "__RENDERED_TEXT__" id_
    --             , selectedId = id_
    --             , searchCount = 0
    --             }

    --         else
    --             let
    --                 id_ =
    --                     List.Extra.getAt model.foundIdIndex model.foundIds |> Maybe.withDefault "(nothing)"
    --             in
    --             { foundIds = model.foundIds
    --             , foundIdIndex = modBy (List.length model.foundIds) (model.foundIdIndex + 1)
    --             , cmd = View.Utility.setViewportForElement "__RENDERED_TEXT__" id_
    --             , selectedId = id_
    --             , searchCount = model.searchCount + 1
    --             }
    -- in
    -- ( { model
    --     | selectedId = data.selectedId
    --     , foundIds = data.foundIds
    --     , foundIdIndex = data.foundIdIndex
    --     , searchCount = data.searchCount
    --     , messages = [ { txt = ("!![" ++ adjustId data.selectedId ++ "]") :: List.map adjustId data.foundIds |> String.join ", ", status = MSWhite } ]
    --   }
    -- , data.cmd
    -- )





firstSyncLR : Model -> String -> ( Model, Cmd Msg )
firstSyncLR model searchSourceText =
   (model, Cmd.none)
    -- let
    --     data =
    --         let
    --             foundIds_ =
    --                 Compiler.ASTTools.matchingIdsInAST searchSourceText model.editRecord.tree

    --             id_ =
    --                 List.head foundIds_ |> Maybe.withDefault "(nothing)"
    --         in
    --         { foundIds = foundIds_
    --         , foundIdIndex = 1
    --         , cmd = View.Utility.setViewportForElement (View.Utility.viewId model.popupState) id_
    --         , selectedId = id_
    --         , searchCount = 0
    --         }
    -- in
    -- ( { model
    --     | selectedId = data.selectedId
    --     , foundIds = data.foundIds
    --     , foundIdIndex = data.foundIdIndex
    --     , searchCount = data.searchCount
    --     , messages = [ { txt = ("[" ++ adjustId data.selectedId ++ "]") :: List.map adjustId data.foundIds |> String.join ", ", status = MSWhite } ]
    --   }
    -- , data.cmd
    -- )


-- inputText2 : Model -> ( Model, Cmd Msg )
-- inputText2 model =
--   let
--         editRecord =
--             Scripta.API.update model.editRecord model.document.content

--     in
--     ( { model
--         | sourceText = str
--         , editRecord = editRecord
--         , counter = model.counter + 1
--         , documentDirty = True
--       }
--     , Cmd.none
--     )

