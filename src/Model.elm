module Model exposing (Model, Msg(..), Flags, PopupState(..))

import Document exposing(Document)
import Scripta.API
import PDF exposing (PDFMsg(..))
import Scripta.Language exposing (Language(..))
import Time

import Html exposing (Html)
import Http

import Json.Decode

type alias Model =
    { count : Int
    , document : Document
    , documentNeedsSaving: Bool
    , inputFilename : String
    , newFilename : String
    , editRecord : Scripta.API.EditRecord
    , language : Language
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





type alias Flags =
    {}


type Msg
    = NoOp
    | InputText String
    | Render Scripta.API.Msg
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
    | ExportTick Time.Posix
    | DocumentSaveTick Time.Posix
    | Refresh
    | NewFile
    | InputNewFileName String
    | CreateFile
    | ClosePopup