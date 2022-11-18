module Render.Export.LaTeX exposing (export, exportExpr, rawExport)

import Compiler.ASTTools as ASTTools
import Compiler.TextMacro
import Compiler.Util
import Dict exposing (Dict)
import Either exposing (Either(..))
import List.Extra
import Maybe.Extra
import Parser.Block exposing (BlockType(..), ExpressionBlock(..))
import Parser.Expr exposing (Expr(..))
import Parser.Forest exposing (Forest)
import Parser.Helpers exposing (Step(..), loop)
import Render.Export.Image
import Render.Export.Preamble
import Render.Export.Util
import Render.Settings exposing (Settings)
import Render.Utility as Utility
import Time
import Tree exposing (Tree)


counterValue : Forest ExpressionBlock -> Maybe Int
counterValue ast =
    ast
        |> ASTTools.getBlockArgsByName "setcounter"
        |> List.head
        |> Maybe.andThen String.toInt


export : Time.Posix -> Settings -> Forest ExpressionBlock -> String
export currentTime settings_ ast =
    let
        rawBlockNames =
            ASTTools.rawBlockNames ast

        expressionNames =
            ASTTools.expressionNames ast ++ macrosInTextMacroDefinitions

        textMacroDefinitions =
            ASTTools.getVerbatimBlockValue "textmacros" ast

        macrosInTextMacroDefinitions =
            Compiler.TextMacro.getTextMacroFunctionNames textMacroDefinitions
    in
    Render.Export.Preamble.make
        rawBlockNames
        expressionNames
        ++ frontMatter currentTime ast
        ++ ("\n\\setcounter{section}{" ++ (counterValue ast |> zeroOrSome |> String.fromInt) ++ "}\n")
        ++ tableofcontents rawBlockNames
        ++ "\n\n"
        ++ rawExport settings_ ast
        ++ "\n\n\\end{document}\n"


frontMatter : Time.Posix -> Forest ExpressionBlock -> String
frontMatter currentTime ast =
    let
        dict =
            ASTTools.frontMatterDict ast

        author1 =
            Dict.get "author1" dict

        author2 =
            Dict.get "author2" dict

        author3 =
            Dict.get "author3" dict

        author4 =
            Dict.get "author4" dict

        authors =
            [ author1, author2, author3, author4 ]
                |> Maybe.Extra.values
                |> String.join "\n\\and\n"
                |> (\s -> "\\author{\n" ++ s ++ "\n}")

        title =
            ASTTools.title ast |> (\title_ -> "\\title{" ++ title_ ++ "}")

        date =
            Dict.get "date" dict |> Maybe.map (\date_ -> "\\date{" ++ date_ ++ "}") |> Maybe.withDefault ""
    in
    ("\\begin{document}"
        :: title
        :: date
        :: authors
        :: "\\maketitle\n\n"
        :: []
        |> String.join "\n\n"
    )
        ++ "\\maketitle\n\n"


today : Time.Posix -> String
today currenTime =
    "currentTime: not implemented"


tableofcontents rawBlockNames_ =
    if List.length (List.filter (\name -> name == "section") rawBlockNames_) > 1 then
        "\n\n\\tableofcontents"

    else
        ""


zeroOrSome : Maybe Int -> Int
zeroOrSome mInt =
    case mInt of
        Nothing ->
            0

        Just k ->
            k


oneOrTwo : Maybe Int -> Int
oneOrTwo mInt =
    case mInt of
        Nothing ->
            1

        Just _ ->
            2


{-| In a standalone MicroLaTeX document, sections correspond to sections in the
exported document.

If a document is part of a collection or "notebook", where we have
set the section number using \\setcounter{N}, sections correspond
to subsections IF the document is exported as a standalone document.

Function shiftSection makes the adjustments needed for export.

-}
shiftSection : Int -> ExpressionBlock -> ExpressionBlock
shiftSection delta ((ExpressionBlock data) as block) =
    if data.name == Just "section" then
        case data.args of
            level :: rest ->
                case String.toInt level of
                    Nothing ->
                        block

                    Just kk ->
                        let
                            newLevel =
                                String.fromInt (kk + delta)
                        in
                        ExpressionBlock { data | args = newLevel :: rest }

            _ ->
                block

    else
        block


exportTree : Settings -> Tree ExpressionBlock -> String
exportTree settings tree =
    case Tree.children tree of
        [] ->
            exportBlock settings (Tree.label tree)

        children ->
            let
                renderedChildren : List String
                renderedChildren =
                    List.map (exportTree settings) children |> List.map String.lines |> List.concat

                root =
                    exportBlock settings (Tree.label tree) |> String.lines
            in
            case List.Extra.unconsLast root of
                Nothing ->
                    ""

                Just ( lastLine, firstLines ) ->
                    let
                        _ =
                            firstLines

                        _ =
                            renderedChildren

                        _ =
                            lastLine
                    in
                    firstLines ++ renderedChildren ++ [ lastLine ] |> String.join "\n"


rawExport : Settings -> List (Tree ExpressionBlock) -> String
rawExport settings ast =
    ast
        |> ASTTools.filterForestOnLabelNames (\name -> not (name == Just "runninghead"))
        |> Parser.Forest.map Parser.Block.condenseUrls
        |> encloseLists
        |> Parser.Forest.map (counterValue ast |> oneOrTwo |> shiftSection)
        |> List.map (exportTree settings)
        |> String.join "\n\n"



--unravel : Tree (Element MarkupMsg) -> Element MarkupMsg
--unravel tree =
--    let
--        children =
--            Tree.children tree
--    in
--    if List.isEmpty children then
--        Tree.label tree
--
--    else
--        Element.column []
--            --  Render.Settings.leftIndentation,
--            [ Tree.label tree
--            , Element.column [ Element.paddingEach { top = Render.Settings.topMarginForChildren, left = Render.Settings.leftIndent, right = 0, bottom = 0 } ] (List.map unravel children)
--            ]


type Status
    = InsideItemizedList
    | InsideNumberedList
    | InsideDescriptionList
    | OutsideList


encloseLists : Forest ExpressionBlock -> Forest ExpressionBlock
encloseLists blocks =
    loop { status = OutsideList, input = blocks, output = [], itemNumber = 0 } nextStep |> List.reverse


type alias State =
    { status : Status, input : Forest ExpressionBlock, output : Forest ExpressionBlock, itemNumber : Int }


nextStep : State -> Step State (Forest ExpressionBlock)
nextStep state =
    case List.head state.input of
        Nothing ->
            Done state.output

        Just tree ->
            Loop (nextState tree state)


beginItemizedBlock : ExpressionBlock
beginItemizedBlock =
    ExpressionBlock
        { args = []
        , properties = Dict.empty
        , blockType = OrdinaryBlock [ "beginBlock" ]
        , content = Right [ Text "itemize" { begin = 0, end = 7, index = 0, id = "" } ]
        , messages = []
        , id = "0"
        , tag = ""
        , indent = 1
        , lineNumber = 0
        , name = Just "beginBlock"
        , numberOfLines = 2
        , sourceText = "| beginBlock\nitemize"
        , error = Nothing
        }


endItemizedBlock : ExpressionBlock
endItemizedBlock =
    ExpressionBlock
        { args = []
        , properties = Dict.empty
        , blockType = OrdinaryBlock [ "endBlock" ]
        , content = Right [ Text "itemize" { begin = 0, end = 7, index = 0, id = "" } ]
        , messages = []
        , id = "0"
        , tag = ""
        , indent = 1
        , lineNumber = 0
        , name = Just "endBlock"
        , numberOfLines = 2
        , sourceText = "| endBlock\nitemize"
        , error = Nothing
        }


beginNumberedBlock : ExpressionBlock
beginNumberedBlock =
    ExpressionBlock
        { args = []
        , properties = Dict.empty
        , blockType = OrdinaryBlock [ "beginNumberedBlock" ]
        , content = Right [ Text "enumerate" { begin = 0, end = 7, index = 0, id = "begin" } ]
        , messages = []
        , id = "0"
        , tag = ""
        , indent = 1
        , lineNumber = 0
        , name = Just "beginNumberedBlock"
        , numberOfLines = 2
        , sourceText = "| beginBlock\nitemize"
        , error = Nothing
        }


endNumberedBlock : ExpressionBlock
endNumberedBlock =
    ExpressionBlock
        { args = []
        , properties = Dict.empty
        , blockType = OrdinaryBlock [ "endNumberedBlock" ]
        , content = Right [ Text "enumerate" { begin = 0, end = 7, index = 0, id = "end" } ]
        , messages = []
        , id = "0"
        , tag = ""
        , indent = 1
        , lineNumber = 0
        , name = Just "endNumberedBlock"
        , numberOfLines = 2
        , sourceText = "| endBlock\nitemize"
        , error = Nothing
        }


beginDescriptionBlock : ExpressionBlock
beginDescriptionBlock =
    ExpressionBlock
        { args = []
        , properties = Dict.empty
        , blockType = OrdinaryBlock [ "beginDescriptionBlock" ]
        , content = Right [ Text "description" { begin = 0, end = 7, index = 0, id = "begin" } ]
        , messages = []
        , id = "0"
        , tag = ""
        , indent = 1
        , lineNumber = 0
        , name = Just "beginDescriptionBlock"
        , numberOfLines = 2
        , sourceText = "| beginBlock\ndescription"
        , error = Nothing
        }


endDescriptionBlock : ExpressionBlock
endDescriptionBlock =
    ExpressionBlock
        { args = []
        , properties = Dict.empty
        , blockType = OrdinaryBlock [ "endDescriptionBlock" ]
        , content = Right [ Text "description" { begin = 0, end = 7, index = 0, id = "end" } ]
        , messages = []
        , id = "0"
        , tag = ""
        , indent = 1
        , lineNumber = 0
        , name = Just "endDescriptionBlock"
        , numberOfLines = 2
        , sourceText = "| endBlock\ndescription"
        , error = Nothing
        }


nextState : Tree ExpressionBlock -> State -> State
nextState tree state =
    let
        name_ =
            case Tree.label tree of
                ExpressionBlock { name } ->
                    name
    in
    case ( state.status, name_ ) of
        -- ITEMIZED LIST
        ( OutsideList, Just "item" ) ->
            { state | status = InsideItemizedList, itemNumber = 1, output = tree :: Tree.singleton beginItemizedBlock :: state.output, input = List.drop 1 state.input }

        ( InsideItemizedList, Just "item" ) ->
            { state | output = tree :: state.output, itemNumber = state.itemNumber + 1, input = List.drop 1 state.input }

        ( InsideItemizedList, _ ) ->
            { state | status = OutsideList, itemNumber = 0, output = tree :: Tree.singleton endItemizedBlock :: state.output, input = List.drop 1 state.input }

        -- NUMBERED LIST
        ( OutsideList, Just "numbered" ) ->
            { state | status = InsideNumberedList, itemNumber = 1, output = tree :: Tree.singleton beginNumberedBlock :: state.output, input = List.drop 1 state.input }

        ( InsideNumberedList, Just "numbered" ) ->
            { state | output = tree :: state.output, itemNumber = state.itemNumber + 1, input = List.drop 1 state.input }

        ( InsideNumberedList, _ ) ->
            { state | status = OutsideList, itemNumber = 0, output = tree :: Tree.singleton endNumberedBlock :: state.output, input = List.drop 1 state.input }

        -- DESCRIPTION LIST
        ( OutsideList, Just "desc" ) ->
            { state | status = InsideDescriptionList, itemNumber = 1, output = tree :: Tree.singleton beginDescriptionBlock :: state.output, input = List.drop 1 state.input }

        ( InsideDescriptionList, Just "desc" ) ->
            { state | output = tree :: state.output, itemNumber = state.itemNumber + 1, input = List.drop 1 state.input }

        ( InsideDescriptionList, _ ) ->
            { state | status = OutsideList, itemNumber = 0, output = tree :: Tree.singleton endDescriptionBlock :: state.output, input = List.drop 1 state.input }

        --- OUTSIDE
        ( OutsideList, _ ) ->
            { state | output = tree :: state.output, input = List.drop 1 state.input }


exportBlock : Settings -> ExpressionBlock -> String
exportBlock settings ((ExpressionBlock { blockType, name, args, content }) as block) =
    case blockType of
        Paragraph ->
            case content of
                Left str ->
                    mapChars2 str

                Right exprs_ ->
                    exportExprList settings exprs_

        OrdinaryBlock _ ->
            case content of
                Left _ ->
                    ""

                Right exprs_ ->
                    let
                        name_ =
                            name |> Maybe.withDefault "anon"
                    in
                    case Dict.get name_ blockDict of
                        Just f ->
                            f settings args (exportExprList settings exprs_)

                        Nothing ->
                            environment name_ (exportExprList settings exprs_)

        VerbatimBlock _ ->
            case content of
                Left str ->
                    case name of
                        Just "math" ->
                            let
                                fix_ : String -> String
                                fix_ str_ =
                                    str_
                                        |> String.lines
                                        |> List.filter (\line -> String.left 2 line /= "$$")
                                        |> String.join "\n"
                                        |> Compiler.Util.transformLabel
                            in
                            -- TODO: This should be fixed upstream
                            [ "$$", fix_ str, "$$" ] |> String.join "\n"

                        Just "equation" ->
                            -- TODO: there should be a trailing "$$"
                            -- TODO: equation numbers and label
                            [ "\\begin{equation}", str |> Compiler.Util.transformLabel, "\\end{equation}" ] |> String.join "\n"

                        Just "aligned" ->
                            -- TODO: equation numbers and label
                            [ "\\begin{align}", str |> Compiler.Util.transformLabel, "\\end{align}" ] |> String.join "\n"

                        Just "code" ->
                            str |> fixChars |> (\s -> "\\begin{verbatim}\n" ++ s ++ "\n\\end{verbatim}")

                        Just "verbatim" ->
                            str |> fixChars |> (\s -> "\\begin{verbatim}\n" ++ s ++ "\n\\end{verbatim}")

                        Just "verse" ->
                            str |> fixChars |> (\s -> "\\begin{verbatim}\n" ++ s ++ "\n\\end{verbatim}")

                        Just "load-files" ->
                            ""

                        Just "mathmacros" ->
                            str

                        Just "texComment" ->
                            str |> String.lines |> texComment

                        Just "textmacros" ->
                            Compiler.TextMacro.exportTexMacros str

                        Just "image" ->
                            Render.Export.Image.exportBlock settings block

                        Just "quiver" ->
                            let
                                lines =
                                    String.split "---" str
                                        |> List.drop 1
                                        |> String.join "\n"
                                        |> String.lines
                                        |> List.filter (\line -> line /= "")

                                line1 =
                                    List.head lines |> Maybe.withDefault "%%" |> String.trim

                                line1b =
                                    if String.contains "\\hide{" line1 then
                                        -- preserve comment with quiver url
                                        line1 |> String.replace "\\hide{" "" |> String.dropRight 1 |> (\x -> "%% " ++ x)

                                    else
                                        line1

                                data =
                                    lines
                                        |> List.drop 1
                                        -- now normalize the data
                                        |> List.filter (\line -> not <| String.contains "\\[\\begin{tikzcd}" line)
                                        |> List.filter (\line -> not <| String.contains "\\end{tikzcd}\\]" line)
                                        |> (\x -> line1b :: "\\[\\begin{tikzcd}" :: x ++ [ "\\end{tikzcd}\\]" ])
                                        |> String.join "\n"
                            in
                            data

                        Just "tikz" ->
                            let
                                renderedAsLaTeX =
                                    String.contains "\\hide{" str

                                data =
                                    String.split "---" str
                                        |> List.drop 1
                                        |> String.join ""
                                        |> String.lines
                                        |> List.map (hideToPercentComment >> commentBlankLine)
                                        |> String.join "\n"
                                        |> addTikzPictureClosing renderedAsLaTeX
                            in
                            [ "\\[\n", data, "\n\\]" ]
                                |> String.join ""

                        Just "docinfo" ->
                            ""

                        _ ->
                            Maybe.withDefault "??" name ++ ": export of this block is unimplemented"

                Right _ ->
                    "???(13)"


addTikzPictureClosing flagUp str =
    if flagUp then
        str ++ "\n\\end{tikzpicture}"

    else
        str


commentBlankLine : String -> String
commentBlankLine line =
    if line == "" then
        "%"

    else
        line


hideToPercentComment : String -> String
hideToPercentComment str =
    if String.left 6 str == "\\hide{" then
        str |> String.dropLeft 6 |> String.dropRight 1 |> (\s -> "%% " ++ s)

    else
        str


fixChars str =
    str |> String.replace "{" "\\{" |> String.replace "}" "\\}"


renderDefs settings exprs =
    "%% Macro definitions from Markup text:\n"
        ++ exportExprList settings exprs


mapChars1 : String -> String
mapChars1 str =
    str
        |> String.replace "\\term_" "\\termx"


mapChars2 : String -> String
mapChars2 str =
    str
        |> String.replace "_" "\\_"



-- BEGIN DICTIONARIES


functionDict : Dict String String
functionDict =
    Dict.fromList
        [ ( "italic", "textit" )
        , ( "i", "textit" )
        , ( "bold", "textbf" )
        , ( "b", "textbf" )
        , ( "image", "imagecenter" )
        , ( "contents", "tableofcontents" )
        ]



-- MACRODICT


macroDict : Dict String (Settings -> List Expr -> String)
macroDict =
    Dict.fromList
        [ ( "link", \_ -> link )
        , ( "ilink", \_ -> ilink )
        , ( "index_", \_ _ -> blindIndex )
        , ( "image", Render.Export.Image.export )
        , ( "vspace", \_ -> vspace )
        , ( "bolditalic", \_ -> bolditalic )
        , ( "brackets", \_ -> brackets )
        , ( "lb", \_ -> lb )
        , ( "rb", \_ -> rb )
        , ( "bt", \_ -> bt )
        , ( "underscore", \_ -> underscore )
        , ( "tags", dontRender )
        ]


dontRender : Settings -> List Expr -> String
dontRender _ _ =
    ""



-- BLOCKDICT


blockDict : Dict String (Settings -> List String -> String -> String)
blockDict =
    Dict.fromList
        [ ( "title", \_ _ _ -> "" )
        , ( "subtitle", \_ _ _ -> "" )
        , ( "author", \_ _ _ -> "" )
        , ( "date", \_ _ _ -> "" )
        , ( "contents", \_ _ _ -> "" )
        , ( "hide", \_ _ _ -> "" )
        , ( "texComment", \_ lines _ -> texComment lines )
        , ( "tags", \_ _ _ -> "" )
        , ( "docinfo", \_ _ _ -> "" )
        , ( "banner", \_ _ _ -> "" )
        , ( "set-key", \_ _ _ -> "" )
        , ( "endnotes", \_ _ _ -> "" )
        , ( "index", \_ _ _ -> "Index: not implemented" )

        --
        , ( "section", \settings_ args body -> section settings_ args body )
        , ( "subheading", \settings_ args body -> subheading settings_ args body )
        , ( "item", \_ _ body -> macro1 "item" body )
        , ( "descriptionItem", \_ args body -> descriptionItem args body )
        , ( "numbered", \_ _ body -> macro1 "item" body )
        , ( "desc", \_ args body -> descriptionItem args body )
        , ( "beginBlock", \_ _ _ -> "\\begin{itemize}" )
        , ( "endBlock", \_ _ _ -> "\\end{itemize}" )
        , ( "beginNumberedBlock", \_ _ _ -> "\\begin{enumerate}" )
        , ( "endNumberedBlock", \_ _ _ -> "\\end{enumerate}" )
        , ( "beginDescriptionBlock", \_ _ _ -> "\\begin{description}" )
        , ( "endDescriptionBlock", \_ _ _ -> "\\end{description}" )
        , ( "mathmacros", \_ _ body -> body ++ "\nHa ha ha!" )
        , ( "setcounter", \_ _ _ -> "" )
        ]


verbatimExprDict =
    Dict.fromList
        [ ( "code", inlineCode )
        , ( "math", inlineMath )
        ]


texComment lines =
    lines |> List.map putPercent |> String.join "\n"


putPercent str =
    if String.left 1 str == "%" then
        str

    else
        "% " ++ str



-- END DICTIONARIES


inlineMath : String -> String
inlineMath str =
    "$" ++ str ++ "$"


inlineCode : String -> String
inlineCode str =
    "\\verb`" ++ str ++ "`"


link : List Expr -> String
link exprs =
    let
        args =
            Render.Export.Util.getTwoArgs exprs
    in
    [ "\\href{", args.second, "}{", args.first, "}" ] |> String.join ""


vspace : List Expr -> String
vspace exprs =
    let
        arg =
            Render.Export.Util.getOneArg exprs
                |> String.toFloat
                |> Maybe.withDefault 0
                |> (\x -> x / 4.0)
                |> String.fromFloat
                |> (\x -> x ++ "mm")
    in
    [ "\\vspace{", arg, "}" ] |> String.join ""


ilink : List Expr -> String
ilink exprs =
    let
        args =
            Render.Export.Util.getTwoArgs exprs
    in
    [ "\\href{", "https://scripta.io/s/", args.second, "}{", args.first, "}" ] |> String.join ""


bolditalic : List Expr -> String
bolditalic exprs =
    let
        args =
            Render.Export.Util.getArgs exprs |> String.join " "
    in
    "\\textbf{\\emph{" ++ args ++ "}}"


brackets : List Expr -> String
brackets exprs =
    "[" ++ (Render.Export.Util.getArgs exprs |> String.join " ") ++ "]"


lb : List Expr -> String
lb _ =
    "["


rb : List Expr -> String
rb _ =
    "]"


bt : List Expr -> String
bt _ =
    "`"


underscore : List Expr -> String
underscore _ =
    "$\\_$"


blindIndex : String
blindIndex =
    ""


setcounter : List String -> String
setcounter args =
    [ "\\setcounter{section}{", Utility.getArg "0" 0 args, "}" ] |> String.join ""


subheading : Settings -> List String -> String -> String
subheading settings args body =
    "\\subheading{" ++ body ++ "}"


descriptionItem : List String -> String -> String
descriptionItem args body =
    let
        arg =
            argString args
    in
    case args of
        [] ->
            "\\item{" ++ body ++ "}"

        _ ->
            "\\item[" ++ arg ++ "]{" ++ body ++ "}"


argString : List String -> String
argString args =
    List.filter (\arg -> not <| String.contains "label:" arg) args |> String.join " "


section : Settings -> List String -> String -> String
section settings args body =
    if settings.isStandaloneDocument then
        section1 args body

    else
        section2 args body


section1 : List String -> String -> String
section1 args body =
    let
        tag =
            body
                |> String.words
                |> Compiler.Util.normalizedWord

        label =
            " \\label{" ++ tag ++ "}"

        suffix =
            case List.Extra.getAt 1 args of
                Nothing ->
                    ""

                Just "-" ->
                    "*"

                Just _ ->
                    ""
    in
    case Utility.getArg "4" 0 args of
        "1" ->
            macro1 ("title" ++ suffix) body ++ label

        "2" ->
            macro1 ("section" ++ suffix) body ++ label

        "3" ->
            macro1 ("subsection" ++ suffix) body ++ label

        "4" ->
            macro1 ("subsubsection" ++ suffix) body ++ label

        _ ->
            macro1 ("subheading" ++ suffix) body ++ label


section2 : List String -> String -> String
section2 args body =
    let
        tag =
            body
                |> String.words
                |> Compiler.Util.normalizedWord

        label =
            " \\label{" ++ tag ++ "}"

        suffix =
            case List.Extra.getAt 1 args of
                Nothing ->
                    ""

                Just "-" ->
                    "*"

                Just _ ->
                    ""
    in
    case Utility.getArg "4" 0 args of
        "1" ->
            macro1 ("section" ++ suffix) body ++ label

        "2" ->
            macro1 ("subsection" ++ suffix) body ++ label

        "3" ->
            macro1 ("subsubsection" ++ suffix) body ++ label

        _ ->
            macro1 ("subheading" ++ suffix) body ++ label


macro1 : String -> String -> String
macro1 name arg =
    if name == "math" then
        "$" ++ arg ++ "$"

    else if name == "group" then
        arg

    else if name == "tags" then
        ""

    else
        case Dict.get name functionDict of
            Nothing ->
                "\\" ++ name ++ "{" ++ mapChars2 (String.trimLeft arg) ++ "}"

            Just realName ->
                "\\" ++ realName ++ "{" ++ mapChars2 (String.trimLeft arg) ++ "}"


exportExprList : Settings -> List Expr -> String
exportExprList settings exprs =
    List.map (exportExpr settings) exprs |> String.join "" |> mapChars1


exportExpr : Settings -> Expr -> String
exportExpr settings expr =
    case expr of
        Fun name exps_ _ ->
            if name == "lambda" then
                case Compiler.TextMacro.extract expr of
                    Just lambda ->
                        Compiler.TextMacro.toString (exportExpr settings) lambda

                    Nothing ->
                        "Error extracting lambda"

            else
                case Dict.get name macroDict of
                    Just f ->
                        f settings exps_

                    Nothing ->
                        "\\" ++ name ++ (List.map (encloseWithBraces << exportExpr settings) exps_ |> String.join "")

        Text str _ ->
            mapChars2 str

        Verbatim name body _ ->
            renderVerbatim name body


encloseWithBraces : String -> String
encloseWithBraces str_ =
    "{" ++ String.trim str_ ++ "}"


renderVerbatim : String -> String -> String
renderVerbatim name body =
    case Dict.get name verbatimExprDict of
        Nothing ->
            name ++ "(" ++ body ++ ") — unimplemented "

        Just f ->
            if List.member name [ "equation", "aligned", "math" ] then
                body |> Compiler.Util.transformLabel |> f

            else
                body |> fixChars |> Compiler.Util.transformLabel |> f



-- HELPERS


tagged name body =
    "\\" ++ name ++ "{" ++ body ++ "}"


environment name body =
    [ tagged "begin" name, body, tagged "end" name ] |> String.join "\n"
