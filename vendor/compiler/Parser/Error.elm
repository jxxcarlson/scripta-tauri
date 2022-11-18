module Parser.Error exposing (ordinaryBlock)

import L0.Parser.Error
import MicroLaTeX.Parser.Error
import Scripta.Language exposing (Language(..))


ordinaryBlock : Language -> String -> List String -> List String -> Int -> String -> ( String, List String )
ordinaryBlock lang name args currentMessages lineNumber revisedContent =
    case lang of
        L0Lang ->
            L0.Parser.Error.ordinaryBlock name args currentMessages lineNumber revisedContent

        MicroLaTeXLang ->
            MicroLaTeX.Parser.Error.ordinaryBlock name args currentMessages lineNumber revisedContent

        XMarkdownLang ->
            -- TODO: implement this
            ( "((unimplemented))", [] )
