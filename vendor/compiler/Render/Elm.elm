module Render.Elm exposing (render)

import Compiler.ASTTools as ASTTools
import Compiler.Acc exposing (Accumulator)
import Dict exposing (Dict)
import Element exposing (Element, column, el, newTabLink, spacing)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import List.Extra
import Maybe.Extra
import Parser.Expr exposing (Expr(..))
import Parser.MathMacro
import Render.Graphics
import Render.Math
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Utility as Utility


render : Int -> Accumulator -> Settings -> Expr -> Element MarkupMsg
render generation acc settings expr =
    case expr of
        Text string meta ->
            Element.el [ Events.onClick (SendMeta meta), htmlId meta.id ] (Element.text string)

        Fun name exprList meta ->
            Element.el [ htmlId meta.id ] (renderMarked name generation acc settings exprList)

        Verbatim name str meta ->
            renderVerbatim name generation acc settings meta str


renderVerbatim name generation acc settings meta str =
    case Dict.get name verbatimDict of
        Nothing ->
            errorText 1 name

        Just f ->
            f generation acc settings meta str


renderMarked name generation acc settings exprList =
    case Dict.get name markupDict of
        Nothing ->
            Element.paragraph [ spacing 8 ] (Element.el [ Background.color errorBackgroundColor, Element.paddingXY 4 2 ] (Element.text name) :: List.map (render generation acc settings) exprList)

        Just f ->
            f generation acc settings exprList


errorBackgroundColor =
    Element.rgb 1 0.8 0.8



-- DICTIONARIES


markupDict : Dict String (Int -> Accumulator -> Settings -> List Expr -> Element MarkupMsg)
markupDict =
    Dict.fromList
        [ ( "bibitem", \_ _ _ exprList -> bibitem exprList )

        -- STYLE
        , ( "strong", \g acc s exprList -> strong g acc s exprList )
        , ( "bold", \g acc s exprList -> strong g acc s exprList )
        , ( "textbf", \g acc s exprList -> strong g acc s exprList )
        , ( "b", \g acc s exprList -> strong g acc s exprList )
        , ( "var", \g acc s exprList -> var g acc s exprList )
        , ( "italic", \g acc s exprList -> italic g acc s exprList )
        , ( "textit", \g acc s exprList -> italic g acc s exprList )
        , ( "bi", \g acc s exprList -> boldItalic g acc s exprList )
        , ( "i", \g acc s exprList -> italic g acc s exprList )
        , ( "boldItalic", \g acc s exprList -> boldItalic g acc s exprList )
        , ( "strike", \g acc s exprList -> strike g acc s exprList )
        , ( "underscore", \g acc s exprList -> underscore g acc s exprList )
        , ( "ref", \_ acc _ exprList -> ref acc exprList )
        , ( "reflink", \_ acc _ exprList -> reflink acc exprList )
        , ( "eqref", \_ acc _ exprList -> eqref acc exprList )
        , ( "underline", \g acc s exprList -> underline g acc s exprList )
        , ( "hide", \_ _ _ _ -> Element.none )
        , ( "author", \_ _ _ _ -> Element.none )
        , ( "date", \_ _ _ _ -> Element.none )
        , ( "today", \_ _ _ _ -> Element.none )
        , ( "comment", \g acc s exprList -> blue g acc s exprList )
        , ( "lambda", \_ _ _ _ -> Element.none )

        -- LATEX
        , ( "title", \g acc s exprList -> title g acc s exprList )
        , ( "setcounter", \_ _ _ _ -> Element.none )

        -- COLOR
        , ( "red", \g acc s exprList -> red g acc s exprList )
        , ( "blue", \g acc s exprList -> blue g acc s exprList )
        , ( "green", \g acc s exprList -> green g acc s exprList )
        , ( "pink", \g acc s exprList -> pink g acc s exprList )
        , ( "magenta", \g acc s exprList -> magenta g acc s exprList )
        , ( "violet", \g acc s exprList -> violet g acc s exprList )
        , ( "highlight", \g acc s exprList -> highlight g acc s exprList )
        , ( "gray", \g acc s exprList -> gray g acc s exprList )
        , ( "errorHighlight", \g acc s exprList -> errorHighlight g acc s exprList )

        --
        , ( "skip", \_ _ _ exprList -> skip exprList )
        , ( "link", \g acc s exprList -> link g acc s exprList )
        , ( "href", \g acc s exprList -> href g acc s exprList )
        , ( "ilink", \g acc s exprList -> ilink g acc s exprList )
        , ( "ulink", \g acc s exprList -> ulink g acc s exprList )
        , ( "cslink", \g acc s exprList -> cslink g acc s exprList )
        , ( "abstract", \g acc s exprList -> abstract g acc s exprList )
        , ( "large", \g acc s exprList -> large g acc s exprList )
        , ( "mdash", \_ _ _ _ -> Element.el [] (Element.text "—") )
        , ( "ndash", \_ _ _ _ -> Element.el [] (Element.text "–") )
        , ( "label", \_ _ _ _ -> Element.none )
        , ( "cite", \_ acc _ exprList -> cite acc exprList )
        , ( "table", \g acc s exprList -> table g acc s exprList )
        , ( "image", \_ _ s exprList -> Render.Graphics.image s exprList )
        , ( "tags", \_ _ _ _ -> Element.none )
        , ( "vspace", vspace )
        , ( "par", par )

        -- MiniLaTeX stuff
        , ( "term", \g acc s exprList -> term g acc s exprList )
        , ( "term_", \_ _ _ _ -> Element.none )
        , ( "footnote", \_ acc _ exprList -> footnote acc exprList )
        , ( "emph", \g acc s exprList -> emph g acc s exprList )
        , ( "group", \g acc s exprList -> identityFunction g acc s exprList )

        --
        , ( "dollarSign", \_ _ _ _ -> Element.el [] (Element.text "$") )
        , ( "dollar", \_ _ _ _ -> Element.el [] (Element.text "$") )
        , ( "brackets", \g acc s exprList -> brackets g acc s exprList )
        , ( "rb", \_ _ _ _ -> rightBracket )
        , ( "lb", \_ _ _ _ -> leftBracket )
        , ( "bt", \_ _ _ _ -> backTick )
        , ( "ds", \_ _ _ _ -> Element.el [] (Element.text "$") )
        , ( "bs", \g acc s exprList -> Element.paragraph [] (Element.text "\\" :: List.map (render g acc s) exprList) )
        , ( "texarg", \g acc s exprList -> Element.paragraph [] ((Element.text "{" :: List.map (render g acc s) exprList) ++ [ Element.text " }" ]) )
        , ( "backTick", \_ _ _ _ -> Element.el [] (Element.text "`") )
        ]


verbatimDict =
    Dict.fromList
        [ ( "$", \g a _ m str -> math g a m str )
        , ( "`", \_ _ _ m str -> code m str )
        , ( "code", \_ _ _ m str -> code m str )
        , ( "math", \g a _ m str -> math g a m str )
        ]



-- FUNCTIONS


identityFunction g acc s exprList =
    Element.paragraph [] (List.map (render g acc s) exprList)


abstract g acc s exprList =
    Element.paragraph [] [ Element.el [ Font.size 18 ] (Element.text "Abstract."), simpleElement [] g acc s exprList ]


large : Int -> Accumulator -> Settings -> List Expr -> Element MarkupMsg
large g acc s exprList =
    simpleElement [ Font.size 18 ] g acc s exprList


link : Int -> Accumulator -> Settings -> List Expr -> Element MarkupMsg
link _ _ _ exprList =
    case List.head <| ASTTools.exprListToStringList exprList of
        Nothing ->
            errorText_ "Please provide label and url"

        Just argString ->
            let
                args =
                    String.words argString

                n =
                    List.length args

                label =
                    List.take (n - 1) args |> String.join " "

                url =
                    List.drop (n - 1) args |> String.join " "
            in
            newTabLink []
                { url = url
                , label = el [ Font.color linkColor ] (Element.text label)
                }


href : Int -> Accumulator -> Settings -> List Expr -> Element MarkupMsg
href _ _ _ exprList =
    let
        url =
            List.Extra.getAt 0 exprList |> Maybe.andThen ASTTools.getText |> Maybe.withDefault ""

        label =
            List.Extra.getAt 1 exprList |> Maybe.andThen ASTTools.getText |> Maybe.withDefault ""
    in
    newTabLink []
        { url = url
        , label = el [ Font.color linkColor ] (Element.text label)
        }


{-|

    An ilink element ("internal link") links to another scripta document.

    Usage: [ilink LINK TEXT USERNAME:SLUG]

    Example: [ilink Read more about it here. jxxcarlson:smart-folders]

-}
ilink _ _ _ exprList =
    case List.head <| ASTTools.exprListToStringList exprList of
        Nothing ->
            errorText_ "Please provide label and url"

        Just argString ->
            let
                args =
                    String.words argString

                n =
                    List.length args

                slug =
                    List.Extra.last args |> Maybe.withDefault "((nothing))"

                label =
                    List.take (n - 1) args |> String.join " "
            in
            Input.button []
                { onPress = Just (GetDocumentWithSlug Render.Msg.MHStandard slug)
                , label = Element.el [ Element.centerX, Element.centerY, Font.size 14, Font.color (Element.rgb 0 0 0.8) ] (Element.text label)
                }


ulink _ _ _ exprList =
    case List.head <| ASTTools.exprListToStringList exprList of
        Nothing ->
            errorText_ "Please provide label and url"

        Just argString ->
            let
                args =
                    String.words argString

                n =
                    List.length args

                label =
                    List.take (n - 1) args |> String.join " "

                fragment =
                    List.drop (n - 1) args |> String.join " "

                username =
                    String.split ":" fragment |> List.head |> Maybe.withDefault "---"
            in
            Input.button []
                { onPress = Just (GetPublicDocumentFromAuthor Render.Msg.MHStandard username fragment)
                , label = Element.el [ Element.centerX, Element.centerY, Font.size 14, Font.color (Element.rgb 0 0 0.8) ] (Element.text label)
                }


cslink _ _ _ exprList =
    case List.head <| ASTTools.exprListToStringList exprList of
        Nothing ->
            errorText_ "Please: id or slug"

        Just argString ->
            let
                args =
                    String.words argString

                n =
                    List.length args

                label =
                    List.take (n - 1) args |> String.join " "

                fragment =
                    List.drop (n - 1) args |> String.join " "

                username =
                    String.split ":" fragment |> List.head |> Maybe.withDefault "---"
            in
            Input.button []
                { onPress = Just (GetPublicDocumentFromAuthor Render.Msg.MHAsCheatSheet username fragment)
                , label = Element.el [ Element.centerX, Element.centerY, Font.size 14, Font.color (Element.rgb 0 0 0.8) ] (Element.text label)
                }


bibitem : List Expr -> Element MarkupMsg
bibitem exprs =
    Element.paragraph [ Element.width Element.fill ] [ Element.text (ASTTools.exprListToStringList exprs |> String.join " " |> (\s -> "[" ++ s ++ "]")) ]


cite : Accumulator -> List Expr -> Element MarkupMsg
cite acc str =
    let
        tag : String
        tag =
            ASTTools.exprListToStringList str |> String.join ""

        id =
            Dict.get tag acc.reference |> Maybe.map .id |> Maybe.withDefault ""
    in
    Element.paragraph
        [ Element.width Element.fill
        , Events.onClick (SendId id)
        , Events.onClick (SelectId id)
        , Font.color (Element.rgb 0.2 0.2 1.0)
        ]
        [ Element.text (tag |> (\s -> "[" ++ s ++ "]")) ]


code m str =
    verbatimElement codeStyle m str


math g a m str =
    mathElement g a m str


table : Int -> Accumulator -> Settings -> List Expr -> Element MarkupMsg
table g acc s rows =
    Element.column [ Element.spacing 8 ] (List.map (tableRow g acc s) rows)


tableRow : Int -> Accumulator -> Settings -> Expr -> Element MarkupMsg
tableRow g acc s expr =
    case expr of
        Fun "tableRow" items _ ->
            Element.row [ spacing 8 ] (List.map (tableItem g acc s) items)

        _ ->
            Element.none


tableItem : Int -> Accumulator -> Settings -> Expr -> Element MarkupMsg
tableItem g acc s expr =
    case expr of
        Fun "tableItem" exprList _ ->
            Element.paragraph [ Element.width (Element.px 100) ] (List.map (render g acc s) exprList)

        _ ->
            Element.none


skip exprList =
    let
        numVal : String -> Int
        numVal str =
            String.toInt str |> Maybe.withDefault 0

        f : String -> Element MarkupMsg
        f str =
            column [ Element.spacingXY 0 (numVal str) ] [ Element.text "" ]
    in
    f1 f exprList


vspace _ _ _ exprList =
    let
        h =
            ASTTools.exprListToStringList exprList |> String.join "" |> String.toInt |> Maybe.withDefault 1
    in
    -- Element.column [ Element.paddingXY 0 100 ] (Element.text "-")
    Element.column [ Element.height (Element.px h) ] [ Element.text "" ]


par _ _ _ _ =
    Element.column [ Element.height (Element.px 15) ] [ Element.text "" ]


strong g acc s exprList =
    simpleElement [ Font.bold ] g acc s exprList


var g acc s exprList =
    simpleElement [] g acc s exprList


brackets g acc s exprList =
    Element.paragraph [ Element.spacing 8 ] [ Element.text "[", simpleElement [] g acc s exprList, Element.text " ]" ]


rightBracket =
    Element.text "]"


leftBracket =
    Element.text "["


backTick =
    Element.text "`"


italic g acc s exprList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g acc s exprList


boldItalic g acc s exprList =
    simpleElement [ Font.italic, Font.bold, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g acc s exprList


title g acc s exprList =
    simpleElement [ Font.size 36, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g acc s exprList


term g acc s exprList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g acc s exprList


footnote acc exprList =
    case exprList of
        (Text _ meta) :: [] ->
            case Dict.get meta.id acc.footnoteNumbers of
                Just k ->
                    Element.link
                        [ Font.color (Element.rgb 0 0 0.7)
                        , Font.bold
                        , Events.onClick (SelectId (meta.id ++ "_"))
                        ]
                        { url = Utility.internalLink (meta.id ++ "_")
                        , label = Element.el [] (Element.html <| Html.node "sup" [] [ Html.text (String.fromInt k) ])
                        }

                -- Element.el (htmlId meta.id :: []) (Element.text (String.fromInt k))
                _ ->
                    Element.none

        _ ->
            Element.none



-- Element.el (htmlId meta.id :: formatList) (Element.text str)


emph g acc s exprList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g acc s exprList



-- COLOR FUNCTIONS


gray g acc s exprList =
    simpleElement [ Font.color (Element.rgb 0.5 0.5 0.5) ] g acc s exprList


red g acc s exprList =
    simpleElement [ Font.color (Element.rgb255 200 0 0) ] g acc s exprList


blue g acc s exprList =
    simpleElement [ Font.color (Element.rgb255 0 0 200) ] g acc s exprList


green g acc s exprList =
    simpleElement [ Font.color (Element.rgb255 0 140 0) ] g acc s exprList


magenta g acc s exprList =
    simpleElement [ Font.color (Element.rgb255 255 51 192) ] g acc s exprList


pink g acc s exprList =
    simpleElement [ Font.color (Element.rgb255 255 100 100) ] g acc s exprList


violet g acc s exprList =
    simpleElement [ Font.color (Element.rgb255 150 100 255) ] g acc s exprList


highlight g acc s exprList_ =
    let
        colorName =
            ASTTools.filterExpressionsOnName "color" exprList_
                |> List.head
                |> Maybe.andThen ASTTools.getText
                |> Maybe.withDefault "yellow"
                |> String.trim

        exprList =
            ASTTools.filterOutExpressionsOnName "color" exprList_

        colorElement =
            Dict.get colorName colorDict |> Maybe.withDefault (Element.rgb255 255 255 0)
    in
    simpleElement [ Background.color colorElement, Element.paddingXY 6 3 ] g acc s exprList


colorDict : Dict String Element.Color
colorDict =
    Dict.fromList
        [ ( "yellow", Element.rgb255 255 255 0 )
        , ( "blue", Element.rgb255 180 180 255 )
        ]


ref : Accumulator -> List Expr -> Element MarkupMsg
ref acc exprList =
    let
        key =
            List.map ASTTools.getText exprList |> Maybe.Extra.values |> String.join "" |> String.trim

        ref_ =
            Dict.get key acc.reference

        val =
            ref_ |> Maybe.map .numRef |> Maybe.withDefault ""

        id =
            ref_ |> Maybe.map .id |> Maybe.withDefault ""
    in
    Element.link
        [ Font.color (Element.rgb 0 0 0.7)
        , Font.bold
        , Events.onClick (SelectId id)
        ]
        { url = Utility.internalLink id
        , label = Element.paragraph [] [ Element.text val ]
        }


{-|

    \reflink{LINK_TEXT LABEL}

-}
reflink : Accumulator -> List Expr -> Element MarkupMsg
reflink acc exprList =
    let
        argString =
            List.map ASTTools.getText exprList |> Maybe.Extra.values |> String.join " "

        args =
            String.words argString

        n =
            List.length args

        key =
            List.drop (n - 1) args |> String.join ""

        label =
            List.take (n - 1) args |> String.join " "

        ref_ =
            Dict.get key acc.reference

        id =
            ref_ |> Maybe.map .id |> Maybe.withDefault ""
    in
    Element.link
        [ Font.color (Element.rgb 0 0 0.7)

        -- , Events.onClick (SendId id)
        , Events.onClick (SelectId id)
        ]
        { url = Utility.internalLink id
        , label = Element.paragraph [] [ Element.text label ]
        }


eqref : Accumulator -> List Expr -> Element MarkupMsg
eqref acc exprList =
    let
        key =
            List.map ASTTools.getText exprList
                |> Maybe.Extra.values
                |> String.join ""
                |> String.trim
                |> String.replace "label:" ""

        ref_ =
            Dict.get key acc.reference

        val =
            ref_ |> Maybe.map .numRef |> Maybe.withDefault ""

        id =
            ref_ |> Maybe.map .id |> Maybe.withDefault ""
    in
    Element.link
        [ Font.color (Element.rgb 0 0 0.7)
        , Events.onClick (SelectId id)
        ]
        { url = Utility.internalLink id
        , label = Element.paragraph [] [ Element.text ("(" ++ val ++ ")") ]
        }



-- FONT STYLE FUNCTIONS


strike g acc s exprList =
    simpleElement [ Font.strike ] g acc s exprList


underscore _ _ _ _ =
    Element.el [] (Element.text "_")


underline g acc s exprList =
    simpleElement [ Font.underline ] g acc s exprList


errorHighlight g acc s exprList =
    simpleElement [ Background.color (Element.rgb255 255 200 200), Element.paddingXY 4 2 ] g acc s exprList



-- HELPERS


simpleElement : List (Element.Attribute MarkupMsg) -> Int -> Accumulator -> Settings -> List Expr -> Element MarkupMsg
simpleElement formatList g acc s exprList =
    Element.paragraph formatList (List.map (render g acc s) exprList)


{-| For one-element functions
-}
f1 : (String -> Element MarkupMsg) -> List Expr -> Element MarkupMsg
f1 f exprList =
    case ASTTools.exprListToStringList exprList of
        -- TODO: temporary fix: parse is producing the args in reverse order
        arg1 :: _ ->
            f arg1

        _ ->
            el [ Font.color errorColor ] (Element.text "Invalid arguments")


verbatimElement formatList meta str =
    Element.el (htmlId meta.id :: formatList) (Element.text str)


htmlId str =
    Element.htmlAttribute (Html.Attributes.id str)


errorText index str =
    Element.el [ Font.color (Element.rgb255 200 40 40) ] (Element.text <| "(" ++ String.fromInt index ++ ") not implemented: " ++ str)


errorText_ str =
    Element.el [ Font.color (Element.rgb255 200 40 40) ] (Element.text str)


mathElement generation acc meta str =
    -- "width" is not used for inline math, but some string needs to be there
    Render.Math.mathText generation "width" meta.id Render.Math.InlineMathMode (Parser.MathMacro.evalStr acc.mathMacroDict str)



-- DEFINITIONS


codeStyle =
    [ Font.family
        [ Font.typeface "Inconsolata"
        , Font.monospace
        ]
    , Font.unitalicized
    , Font.color Render.Settings.codeColor
    , Element.paddingEach { left = 2, right = 2, top = 0, bottom = 0 }
    ]


errorColor =
    Element.rgb 0.8 0 0


linkColor =
    Element.rgb 0 0 0.8
