module Parser.Common exposing (Classification, bareBlockNames, verbatimBlockNames)

import Parser.Block exposing (BlockType)


type alias Classification =
    { blockType : BlockType, args : List String, name : Maybe String }


verbatimBlockNames =
    [ "comment", "equation", "aligned", "code", "mathmacros", "iframe" ]


bareBlockNames =
    [ "contents" ]
