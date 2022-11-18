module Compiler.Acc exposing
    ( Accumulator
    , InitialAccumulatorData
    , init
    , initialData
    , transformAccumulate
    , transformST
    )

import Compiler.ASTTools
import Compiler.TextMacro exposing (Macro)
import Compiler.Util
import Compiler.Vector as Vector exposing (Vector)
import Dict exposing (Dict)
import Either exposing (Either(..))
import List.Extra
import Maybe.Extra
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr(..))
import Parser.Forest exposing (Forest)
import Parser.MathMacro
import Parser.Meta exposing (Meta)
import Parser.Settings
import Render.Utility
import Scripta.Language exposing (Language)
import Tree exposing (Tree)


indentationQuantum =
    2


initialData : Scripta.Language.Language -> InitialAccumulatorData
initialData lang =
    { language = lang
    , mathMacros = ""
    , textMacros = ""
    , vectorSize = 4
    }


type alias Accumulator =
    { headingIndex : Vector
    , documentIndex : Vector
    , counter : Dict String Int
    , blockCounter : Int
    , itemVector : Vector
    , numberedItemDict : Dict String { level : Int, index : Int }
    , numberedBlockNames : List String
    , inList : Bool
    , reference : Dict String { id : String, numRef : String }
    , terms : Dict String TermLoc
    , footnotes : Dict String TermLoc
    , footnoteNumbers : Dict String Int
    , mathMacroDict : Parser.MathMacro.MathMacroDict
    , textMacroDict : Dict String Macro
    , keyValueDict : Dict String String
    , qAndAList : List ( String, String )
    , qAndADict : Dict String String
    }


getCounter : String -> Dict String Int -> Int
getCounter name dict =
    Dict.get name dict |> Maybe.withDefault 0


getCounterAsString : String -> Dict String Int -> String
getCounterAsString name dict =
    Dict.get name dict |> Maybe.map String.fromInt |> Maybe.withDefault ""


incrementCounter : String -> Dict String Int -> Dict String Int
incrementCounter name dict =
    Dict.insert name (getCounter name dict + 1) dict


transformST : InitialAccumulatorData -> Forest ExpressionBlock -> Forest ExpressionBlock
transformST data ast =
    ast |> transformAccumulate data |> Tuple.second


{-| Note that function transformAccumulate operates on initialized accumulator.
-}
transformAccumulate : InitialAccumulatorData -> Forest ExpressionBlock -> ( Accumulator, Forest ExpressionBlock )
transformAccumulate data ast =
    List.foldl (\tree ( acc_, ast_ ) -> transformAccumulateTree tree acc_ |> mapper ast_) ( init data, [] ) ast
        |> (\( acc_, ast_ ) -> ( acc_, List.reverse ast_ ))


type alias InitialAccumulatorData =
    { language : Language
    , mathMacros : String
    , textMacros : String
    , vectorSize : Int
    }


init : InitialAccumulatorData -> Accumulator
init data =
    { headingIndex = Vector.init data.vectorSize
    , documentIndex = Vector.init data.vectorSize
    , inList = False
    , counter = Dict.empty
    , blockCounter = 0
    , itemVector = Vector.init data.vectorSize
    , numberedItemDict = Dict.empty
    , numberedBlockNames = Parser.Settings.numberedBlockNames
    , reference = Dict.empty
    , terms = Dict.empty
    , footnotes = Dict.empty
    , footnoteNumbers = Dict.empty
    , mathMacroDict = Dict.empty
    , textMacroDict = Dict.empty
    , keyValueDict = Dict.empty
    , qAndAList = []
    , qAndADict = Dict.empty
    }
        |> updateWithMathMacros data.mathMacros
        |> updateWithTextMacros data.textMacros


mapper ast_ ( acc_, tree_ ) =
    ( acc_, tree_ :: ast_ )


transformAccumulateBlock : Accumulator -> ExpressionBlock -> ( Accumulator, ExpressionBlock )
transformAccumulateBlock =
    \acc_ block_ ->
        let
            newAcc =
                updateAccumulator block_ acc_
        in
        ( newAcc, transformBlock newAcc block_ )


transformAccumulateTree : Tree ExpressionBlock -> Accumulator -> ( Accumulator, Tree ExpressionBlock )
transformAccumulateTree tree acc =
    Tree.mapAccumulate transformAccumulateBlock acc tree


transformBlock : Accumulator -> ExpressionBlock -> ExpressionBlock
transformBlock acc (ExpressionBlock block) =
    case ( block.name, block.args ) of
        ( Just "section", level :: [] ) ->
            ExpressionBlock
                { block | properties = Dict.insert "label" (Vector.toString acc.headingIndex) block.properties }

        ( Just "quiver", _ ) ->
            ExpressionBlock
                { block | properties = Dict.insert "figure" (getCounterAsString "figure" acc.counter) block.properties }

        ( Just "chart", _ ) ->
            ExpressionBlock
                { block | properties = Dict.insert "figure" (getCounterAsString "figure" acc.counter) block.properties }

        ( Just "image", _ ) ->
            ExpressionBlock
                { block | properties = Dict.insert "figure" (getCounterAsString "figure" acc.counter) block.properties }

        ( Just "iframe", _ ) ->
            ExpressionBlock
                { block | properties = Dict.insert "figure" (getCounterAsString "figure" acc.counter) block.properties }

        ( Just "section", level :: "-" :: [] ) ->
            ExpressionBlock
                { block | args = level :: "-" :: [] }

        ( Just "document", _ ) ->
            ExpressionBlock
                { block | properties = Dict.insert "label" (Vector.toString acc.documentIndex) block.properties }

        ( Just "equation", _ ) ->
            let
                prefix =
                    Vector.toString acc.headingIndex

                equationProp =
                    if prefix == "" then
                        getCounterAsString "equation" acc.counter

                    else
                        Vector.toString acc.headingIndex ++ "." ++ getCounterAsString "equation" acc.counter
            in
            ExpressionBlock
                { block | properties = Dict.insert "equation" equationProp block.properties }

        ( Just "aligned", _ ) ->
            let
                prefix =
                    Vector.toString acc.headingIndex

                equationProp =
                    if prefix == "" then
                        getCounterAsString "equation" acc.counter

                    else
                        Vector.toString acc.headingIndex ++ "." ++ getCounterAsString "equation" acc.counter
            in
            ExpressionBlock
                { block | properties = Dict.insert "equation" equationProp block.properties }

        ( Just name_, _ ) ->
            -- ( Just name_, level :: [] ) ->
            -- Insert the numerical counter, e.g,, equation number, in the arg list of the block
            if List.member name_ [ "section" ] then
                let
                    prefix =
                        Vector.toString acc.headingIndex

                    equationProp =
                        if prefix == "" then
                            getCounterAsString "equation" acc.counter

                        else
                            Vector.toString acc.headingIndex ++ "." ++ getCounterAsString "equation" acc.counter
                in
                ExpressionBlock
                    { block
                        | properties = Dict.insert "label" equationProp block.properties
                    }

            else
                ExpressionBlock
                    { block
                        | properties = Dict.insert "label" (vectorPrefix acc.headingIndex ++ String.fromInt acc.blockCounter) block.properties
                    }
                    |> expand acc.textMacroDict

        _ ->
            expand acc.textMacroDict (ExpressionBlock block)


{-|

    Remove any items from 'list' if they contain the ssring
    'alreadyThere', then prepend 'item' to the result

-}
prependAsNew : String -> String -> List String -> List String
prependAsNew alreadyThere str list =
    str :: List.filter (\item -> not <| String.contains alreadyThere item) list


vectorPrefix : Vector -> String
vectorPrefix headingIndex =
    let
        prefix =
            Vector.toString headingIndex
    in
    if prefix == "" then
        ""

    else
        Vector.toString headingIndex ++ "."


{-| Map name to name of counter
-}
reduceName : String -> String
reduceName str =
    if List.member str [ "equation", "aligned" ] then
        "equation"

    else if str == "code" then
        "listing"

    else if List.member str [ "quiver", "image", "iframe", "chart", "datatable", "svg", "tikz", "iframe" ] then
        "figure"

    else
        str


insertInStringList : String -> List String -> List String
insertInStringList str list =
    if str == "" then
        list

    else if List.Extra.notMember str list then
        str :: list

    else
        list


expand : Dict String Macro -> ExpressionBlock -> ExpressionBlock
expand dict (ExpressionBlock block) =
    ExpressionBlock { block | content = Either.map (List.map (Compiler.TextMacro.expand dict)) block.content }


{-| The first component of the return value (Bool, Maybe Vector) is the
updated inList.
-}
listData : Accumulator -> Maybe String -> ( Bool, Maybe Vector )
listData accumulator name =
    case ( accumulator.inList, name ) of
        ( False, Just "numbered" ) ->
            ( True, Just (Vector.init 4 |> Vector.increment 0) )

        ( False, Just "item" ) ->
            ( True, Just (Vector.init 4 |> Vector.increment 0) )

        ( _, Nothing ) ->
            -- Don't change state if there are anonymous blocks
            -- TODO: think about this, consistent with markdown semantics but not LaTeX
            -- TODO: however it does fix a numbering bug (see MicroLaTeX Visual OTNetworkTest)
            -- ( accumulator.inList, Nothing )
            ( False, Nothing )

        ( False, _ ) ->
            ( False, Nothing )

        ( True, Just "numbered" ) ->
            ( True, Nothing )

        ( True, Just "item" ) ->
            ( False, Nothing )

        ( True, _ ) ->
            ( False, Nothing )


{-| Update the references dictionary: add a key-value pair where the
key is defined as in the examples \\label{foo} or [label foo],
and where value is a record with an id and a "numerical" reference,
e.g, "2" or "2.3"
-}
updateReference : String -> String -> String -> Accumulator -> Accumulator
updateReference tag_ id_ numRef_ acc =
    if tag_ /= "" then
        { acc | reference = Dict.insert tag_ { id = id_, numRef = numRef_ } acc.reference }

    else
        acc


updateAccumulator : ExpressionBlock -> Accumulator -> Accumulator
updateAccumulator (ExpressionBlock { name, indent, args, blockType, content, tag, id }) accumulator =
    -- Update the accumulator for expression blocks with selected name
    case ( name, blockType ) of
        -- provide numbering for sections
        ( Just "q", _ ) ->
            { accumulator
                | qAndAList = ( id, "??" ) :: accumulator.qAndAList
                , blockCounter = accumulator.blockCounter + 1
            }
                |> updateReference tag id tag

        ( Just "a", _ ) ->
            case List.Extra.uncons accumulator.qAndAList of
                Just ( ( q, _ ), rest ) ->
                    { accumulator
                        | qAndAList = ( q, id ) :: rest
                        , qAndADict = Dict.fromList (( q, id ) :: rest)
                    }

                _ ->
                    accumulator

        ( Just "set-key", OrdinaryBlock _ ) ->
            case args of
                key :: value :: rest ->
                    { accumulator | keyValueDict = Dict.insert key value accumulator.keyValueDict }

                _ ->
                    accumulator

        ( Just "section", OrdinaryBlock _ ) ->
            let
                level =
                    List.head args |> Maybe.withDefault "1"
            in
            updateWithOrdinarySectionBlock accumulator name content level id

        ( Just "document", OrdinaryBlock _ ) ->
            let
                level =
                    List.head args |> Maybe.withDefault "1"
            in
            updateWithOrdinaryDocumentBlock accumulator name content level id

        ( Just "setcounter", OrdinaryBlock _ ) ->
            let
                n =
                    List.head args |> Maybe.andThen String.toInt |> Maybe.withDefault 1
            in
            { accumulator | headingIndex = { content = [ n, 0, 0, 0 ], size = 4 } }

        ( Just "bibitem", OrdinaryBlock _ ) ->
            updateBibItemBlock accumulator args id

        ( Just name_, OrdinaryBlock _ ) ->
            -- TODO: tighten up
            updateWithOrdinaryBlock accumulator (Just name_) content tag id indent

        -- provide for numbering of equations
        ( Just "mathmacros", VerbatimBlock [] ) ->
            case content of
                Right _ ->
                    accumulator

                Left str ->
                    updateWithMathMacros str accumulator

        ( Just "textmacros", VerbatimBlock [] ) ->
            case content of
                Right _ ->
                    accumulator

                Left str ->
                    updateWithTextMacros str accumulator

        ( Just _, VerbatimBlock _ ) ->
            -- TODO: tighten up
            updateWithVerbatimBlock accumulator name args tag id

        ( Nothing, Paragraph ) ->
            updateWithParagraph accumulator Nothing content tag id

        _ ->
            -- TODO: take care of numberedItemIndex
            let
                ( inList, _ ) =
                    listData accumulator name
            in
            { accumulator | inList = inList }


normalzeLines : List String -> List String
normalzeLines lines =
    List.map (\line -> String.trim line) lines |> List.filter (\line -> line /= "")


updateWithOrdinarySectionBlock : Accumulator -> Maybe String -> Either String (List Expr) -> String -> String -> Accumulator
updateWithOrdinarySectionBlock accumulator name content level id =
    let
        ( inList, _ ) =
            listData accumulator name

        titleWords =
            case content of
                Left str ->
                    [ Compiler.Util.compressWhitespace str ]

                Right expr ->
                    List.map Compiler.ASTTools.getText expr |> Maybe.Extra.values |> List.map Compiler.Util.compressWhitespace

        sectionTag =
            -- TODO: the below is a bad solution
            titleWords |> List.map (String.toLower >> Compiler.Util.compressWhitespace >> Compiler.Util.removeNonAlphaNum >> String.replace " " "-") |> String.join ""

        headingIndex =
            Vector.increment (String.toInt level |> Maybe.withDefault 0) accumulator.headingIndex

        blockCounter =
            0
    in
    -- TODO: take care of numberedItemIndex = 0 here and elsewhere
    { accumulator
        | inList = inList
        , headingIndex = headingIndex
        , blockCounter = blockCounter
        , counter = Dict.insert "equation" 0 accumulator.counter
    }
        |> updateReference sectionTag id (Vector.toString headingIndex)


updateWithOrdinaryDocumentBlock : Accumulator -> Maybe String -> Either String (List Expr) -> String -> String -> Accumulator
updateWithOrdinaryDocumentBlock accumulator name content level id =
    let
        ( inList, _ ) =
            listData accumulator name

        title =
            case content of
                Left str ->
                    str

                Right expr ->
                    List.map Compiler.ASTTools.getText expr |> Maybe.Extra.values |> String.join " "

        sectionTag =
            title |> String.toLower |> String.replace " " "-"

        documentIndex =
            Vector.increment (String.toInt level |> Maybe.withDefault 0) accumulator.documentIndex
    in
    -- TODO: take care of numberedItemIndex = 0 here and elsewhere
    { accumulator | inList = inList, documentIndex = documentIndex } |> updateReference sectionTag id (Vector.toString documentIndex)


updateBibItemBlock accumulator args id =
    case List.head args of
        Nothing ->
            accumulator

        Just label ->
            { accumulator | reference = Dict.insert label { id = id, numRef = "_irrelevant_" } accumulator.reference }


updateWithOrdinaryBlock : Accumulator -> Maybe String -> Either b (List Expr) -> String -> String -> Int -> Accumulator
updateWithOrdinaryBlock accumulator name content tag id indent =
    let
        ( inList, initialNumberedVector ) =
            listData accumulator name
    in
    case name of
        Just "setcounter" ->
            case content of
                Left _ ->
                    accumulator

                Right exprs ->
                    let
                        ctr =
                            case exprs of
                                [ Text val _ ] ->
                                    String.toInt val |> Maybe.withDefault 1

                                _ ->
                                    1

                        headingIndex =
                            Vector.init accumulator.headingIndex.size |> Vector.set 0 (ctr - 1)
                    in
                    { accumulator | headingIndex = headingIndex }

        Just "numbered" ->
            let
                level =
                    indent // indentationQuantum

                itemVector =
                    case initialNumberedVector of
                        Just v ->
                            v

                        Nothing ->
                            Vector.increment level accumulator.itemVector

                index =
                    Vector.get level itemVector

                numberedItemDict =
                    Dict.insert id { level = level, index = index } accumulator.numberedItemDict
            in
            { accumulator | inList = inList, itemVector = itemVector, numberedItemDict = numberedItemDict }
                |> updateReference tag id (String.fromInt (Vector.get level itemVector))

        Just "item" ->
            let
                level =
                    indent // indentationQuantum

                itemVector =
                    case initialNumberedVector of
                        Just v ->
                            v

                        Nothing ->
                            Vector.increment level accumulator.itemVector

                numberedItemDict =
                    Dict.insert id { level = level, index = Vector.get level itemVector } accumulator.numberedItemDict
            in
            { accumulator | inList = inList, itemVector = itemVector, numberedItemDict = numberedItemDict }
                |> updateReference tag id (String.fromInt (Vector.get level itemVector))

        Just name_ ->
            if List.member name_ [ "title", "contents", "banner", "a" ] then
                accumulator

            else
                { accumulator | blockCounter = accumulator.blockCounter + 1 }
                    |> updateReference tag id tag

        _ ->
            accumulator


updateWithTextMacros : String -> Accumulator -> Accumulator
updateWithTextMacros content accumulator =
    { accumulator | textMacroDict = Compiler.TextMacro.buildDictionary (String.lines content |> normalzeLines) }


updateWithMathMacros : String -> Accumulator -> Accumulator
updateWithMathMacros content accumulator =
    let
        definitions =
            content
                |> String.replace "\\begin{mathmacros}" ""
                |> String.replace "\\end{mathmacros}" ""
                |> String.replace "end" ""
                |> String.trim

        mathMacroDict =
            Parser.MathMacro.makeMacroDict (String.trim definitions)
    in
    { accumulator | mathMacroDict = mathMacroDict }


updateWithVerbatimBlock accumulator name_ args tag_ id =
    let
        _ =
            ( name_, tag, id )

        ( inList, _ ) =
            listData accumulator name_

        name =
            Maybe.withDefault "---" name_

        dict =
            Render.Utility.keyValueDict args

        tag =
            Dict.get "label" dict |> Maybe.withDefault tag_

        isSimple =
            List.member name [ "quiver", "image" ]

        -- Increment the appropriate counter, e.g., "equation" and "aligned"
        -- reduceName maps these both to "equation"
        newCounter =
            if List.member name accumulator.numberedBlockNames then
                incrementCounter (reduceName name) accumulator.counter

            else
                accumulator.counter

        _ =
            if isSimple then
                id

            else
                " "
    in
    { accumulator | inList = inList, counter = newCounter }
        |> updateReference tag id (verbatimBlockReference isSimple accumulator.headingIndex name newCounter)


verbatimBlockReference : Bool -> Vector -> String -> Dict String Int -> String
verbatimBlockReference isSimple headingIndex name newCounter =
    let
        a =
            Vector.toString headingIndex
    in
    if a == "" || isSimple then
        getCounter (reduceName name) newCounter |> String.fromInt

    else
        a ++ "." ++ (getCounter (reduceName name) newCounter |> String.fromInt)


updateWithParagraph accumulator name content tag id =
    let
        ( inList, _ ) =
            listData accumulator name

        ( footnotes, footnoteNumbers ) =
            addFootnotesFromContent id content ( accumulator.footnotes, accumulator.footnoteNumbers )
    in
    { accumulator
        | inList = inList
        , terms = addTermsFromContent id content accumulator.terms
        , footnotes = footnotes
        , footnoteNumbers = footnoteNumbers
    }
        |> updateReference tag id tag


type alias TermLoc =
    { begin : Int, end : Int, id : String }


type alias TermData =
    { term : String, loc : TermLoc }


getTerms : String -> Either String (List Expr) -> List TermData
getTerms id content_ =
    case content_ of
        Right expressionList ->
            Compiler.ASTTools.filterExpressionsOnName_ "term" expressionList
                |> List.map (extract id)
                |> Maybe.Extra.values

        Left _ ->
            []



-- TERMS: [Expr "term" [Text "group" { begin = 19, end = 23, index = 4 }] { begin = 13, end = 13, index = 1 }]


extract : String -> Expr -> Maybe TermData
extract id expr =
    case expr of
        Fun "term" [ Text name { begin, end } ] _ ->
            Just { term = name, loc = { begin = begin, end = end, id = id } }

        Fun "term_" [ Text name { begin, end } ] _ ->
            Just { term = name, loc = { begin = begin, end = end, id = id } }

        _ ->
            Nothing


addTerm : TermData -> Dict String TermLoc -> Dict String TermLoc
addTerm termData dict =
    Dict.insert termData.term termData.loc dict


addTerms : List TermData -> Dict String TermLoc -> Dict String TermLoc
addTerms termDataList dict =
    List.foldl addTerm dict termDataList


addTermsFromContent : String -> Either String (List Expr) -> Dict String TermLoc -> Dict String TermLoc
addTermsFromContent id content dict =
    addTerms (getTerms id content) dict



-- FOOTNOTES


getFootnotes : String -> Either String (List Expr) -> List TermData
getFootnotes id content_ =
    case content_ of
        Right expressionList ->
            Compiler.ASTTools.filterExpressionsOnName_ "footnote" expressionList
                |> List.map (extractFootnote id)
                |> Maybe.Extra.values

        Left _ ->
            []


extractFootnote : String -> Expr -> Maybe TermData
extractFootnote id_ expr =
    case expr of
        Fun "footnote" [ Text content { begin, end, index, id } ] _ ->
            Just { term = content, loc = { begin = begin, end = end, id = id } }

        _ ->
            Nothing



-- EXTRACT ??


addFootnote : TermData -> Dict String TermLoc -> Dict String TermLoc
addFootnote footnoteData dict =
    Dict.insert footnoteData.term footnoteData.loc dict


addFootnoteLabel : TermData -> Dict String Int -> Dict String Int
addFootnoteLabel footnoteData dict =
    Dict.insert footnoteData.loc.id (Dict.size dict + 1) dict


addFootnotes : List TermData -> ( Dict String TermLoc, Dict String Int ) -> ( Dict String TermLoc, Dict String Int )
addFootnotes termDataList ( dict1, dict2 ) =
    List.foldl (\data ( d1, d2 ) -> ( addFootnote data d1, addFootnoteLabel data d2 )) ( dict1, dict2 ) termDataList


addFootnotesFromContent : String -> Either String (List Expr) -> ( Dict String TermLoc, Dict String Int ) -> ( Dict String TermLoc, Dict String Int )
addFootnotesFromContent id content ( dict1, dict2 ) =
    addFootnotes (getFootnotes id content) ( dict1, dict2 )
