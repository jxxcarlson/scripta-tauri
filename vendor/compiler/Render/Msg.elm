module Render.Msg exposing (MarkupMsg(..), SolutionState(..), Handling(..))

{-| The Render.Msg.MarkupMsg type is need for synchronization of the source and rendered
text when using the Codemirror editor.

@docs MarkupMsg, SolutionState, Handling

-}


{-| -}
type MarkupMsg
    = SendMeta { begin : Int, end : Int, index : Int, id : String }
    | SendId String
    | SelectId String
    | HighlightId String
    | GetPublicDocument Handling String
    | GetPublicDocumentFromAuthor Handling String String
    | GetDocumentWithSlug Handling String
    | ProposeSolution SolutionState


{-| -}
type Handling
    = MHStandard
    | MHAsCheatSheet


{-| -}
type SolutionState
    = Unsolved
    | Solved String -- Solved SolutionId
