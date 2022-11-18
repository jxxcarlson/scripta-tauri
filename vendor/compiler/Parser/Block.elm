module Parser.Block exposing
    ( BlockType(..), ExpressionBlock(..)
    , ExpressionBlockData, RawBlock, condenseUrls, empty, empty_, getContent, getName, setName
    )

{-| Source text is parsed into a tree of IntermediateBlocks, where the tree
structure is determined by the indentation level. The expression parser
is mapped over this tree, yielding a tree of ExpressionBlocks. The renderer
consumes trees of ExpressionBlocks to produce Html.

  - The two blocks differ only in their content and children fields.

  - Blocks contain auxiliary information used by editors and IDE's, e.g.,
    the line number in the source text at which the text of the block begins.

@docs BlockType, ExpressionBlock, IntermediateBlock

-}

import Dict exposing (Dict)
import Either exposing (Either)
import Parser.Expr exposing (Expr)


type alias RawBlock =
    { indent : Int
    , lineNumber : Int
    , numberOfLines : Int
    , content : String
    }


type alias ExpressionBlockData =
    { name : Maybe String
    , args : List String
    , properties : Dict String String
    , indent : Int
    , lineNumber : Int
    , numberOfLines : Int
    , id : String
    , tag : String
    , blockType : BlockType
    , content : Either String (List Expr)
    , messages : List String
    , sourceText : String
    , error : Maybe { error : String }
    }


{-| -}
type ExpressionBlock
    = ExpressionBlock
        { name : Maybe String
        , args : List String
        , properties : Dict String String
        , indent : Int
        , lineNumber : Int
        , numberOfLines : Int
        , id : String
        , tag : String
        , blockType : BlockType
        , content : Either String (List Expr)
        , messages : List String
        , sourceText : String
        , error : Maybe { error : String }
        }


empty =
    ExpressionBlock empty_


empty_ =
    { name = Nothing
    , args = []
    , properties = Dict.empty
    , indent = 0
    , lineNumber = 0
    , numberOfLines = 0
    , id = "-"
    , tag = "-"
    , blockType = VerbatimBlock []
    , content = Either.Left "-"
    , messages = []
    , sourceText = "-"
    , error = Nothing
    }


setName : String -> ExpressionBlock -> ExpressionBlock
setName name (ExpressionBlock data) =
    ExpressionBlock { data | name = Just name }


getContent : ExpressionBlock -> List Expr
getContent (ExpressionBlock { content }) =
    case content of
        Either.Left _ ->
            []

        Either.Right exprs ->
            exprs



-- expressionNames : ExpressionBlock -> List String


condenseUrls : ExpressionBlock -> ExpressionBlock
condenseUrls (ExpressionBlock data) =
    case data.content of
        Either.Left _ ->
            ExpressionBlock data

        Either.Right exprList ->
            ExpressionBlock { data | content = Either.Right (List.map Parser.Expr.condenseUrl exprList) }


getName : ExpressionBlock -> Maybe String
getName (ExpressionBlock { name }) =
    name


{-| An ordinary block has the form

    | BLOCK-HEADER
    BODY

Examples:

    | heading 1
    Introduction

    | theorem
    There are infinitely many primes $p \equiv 1\ mod\ 4$.

Verbatim blocks have the form

    || BLOCK-HEADER
    BODY

Examples:

    || equation
    \int_0^1 x^n dx = \frac{1}{n+1}

Paragraphs are "anonymous" blocks.

-}
type BlockType
    = Paragraph
    | OrdinaryBlock (List String)
    | VerbatimBlock (List String)
