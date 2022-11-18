module Compiler.Transform exposing (transform)

import MicroLaTeX.Parser.Transform
import Parser.PrimitiveBlock exposing (PrimitiveBlock)
import Scripta.Language exposing (Language(..))
import XMarkdown.Transform


transform : Language -> PrimitiveBlock -> PrimitiveBlock
transform lang block =
    case lang of
        L0Lang ->
            block

        MicroLaTeXLang ->
            MicroLaTeX.Parser.Transform.transform block

        PlainTextLang ->
            block

        XMarkdownLang ->
            XMarkdown.Transform.transform block
