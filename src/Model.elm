module Model exposing (Model, Msg(..), Flags, PopupState(..), SelectionState(..))

import Render.Msg exposing(MarkupMsg(..))
import Document exposing(Document, SourceTextRecord)
import Browser.Dom
import Scripta.API
import PDF exposing (PDFMsg(..))
import Scripta.Language exposing (Language(..))
import Time
import Dict exposing(Dict)
import Keyboard

import Html exposing (Html)
import Http

import Json.Decode
import Document exposing (SourceTextRecord)
import Keyboard

type alias Model =
    { count : Int
    , document : Document
    , linenumber : Int
    , doSync : Bool
    , foundIdIndex : Int
    , searchSourceText : String
    , searchCount : Int
    , selectedId : String
    , selectionHighLighted : SelectionState
    , foundIds : List String
    , pressedKeys : List Keyboard.Key
    , documentNeedsSaving: Bool
    , inputFilename : String
    , newFilename : String
    , editRecord : Scripta.API.EditRecord
    , initialText : String
    , language : Language
    , currentTime : Time.Posix
    , printingState : PDF.PrintingState
    , tarFileState : PDF.TarFileState
    , message : String
    , ticks : Int
    , popupState : PopupState
    , preferences : Dict String String
    , homeDirectory : Maybe HomeDirectory
    }


type SelectionState = Unselected | IdSelected String

type HomeDirectory = DesktopDir | DocumentsDir 
type PopupState = NewDocumentWindowOpen | NoPopups

type DocumentType
    = InfoDocument
    | TestDocument
    | Example





type alias Flags =
    {}


type Msg
    = NoOp
    | InputText SourceTextRecord
    | InputCursor { position : Int, source : String }
    | SelectedText String
    | Render MarkupMsg
    | PDF PDFMsg
    | SetExampleDocument String
    | Export
    | RawExport
    | PrintToPDF
    | GotPdfLink (Result Http.Error String)
    | GotTarFile (Result Http.Error String)
    | GetTarFile
    | ChangePrintingState PDF.PrintingState
    | ChangeTarFileState PDF.TarFileState
    | SetLanguage Language
    | SendDocument
    | ListDirectory String
    | DocumentReceived (Result Json.Decode.Error Document)
    | PreferencesReceived (Result Json.Decode.Error String)
    | ExportTick Time.Posix
    | DocumentSaveTick Time.Posix
    | Refresh
    | NewFile
    | InputNewFileName String
    | CreateFile
    | ClosePopup
    | SyncLR
    | SetViewPortForElement (Result Browser.Dom.Error ( Browser.Dom.Element, Browser.Dom.Viewport ))
    | KeyMsg Keyboard.Msg
    | RenderMarkupMsg MarkupMsg

