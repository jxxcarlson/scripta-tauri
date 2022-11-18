module Render.Math exposing
    ( DisplayMode(..)
    , aligned
    , displayedMath
    , equation
    , mathText
    )

import Compiler.Acc exposing (Accumulator)
import Dict exposing (Dict)
import Either exposing (Either(..))
import Element exposing (Element)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode
import Parser.Block exposing (ExpressionBlock(..))
import Parser.MathMacro
import Parser.TextMacro
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Utility


type DisplayMode
    = InlineMathMode
    | DisplayMathMode



-- MATH


displayedMath : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
displayedMath count acc settings ((ExpressionBlock { id, error }) as block) =
    let
        w =
            String.fromInt settings.width ++ "px"

        filteredLines =
            -- lines of math text to be rendered: filter stuff out
            String.lines (getContent block)
                |> List.filter (\line -> not (String.left 2 (String.trim line) == "$$"))
                |> List.filter (\line -> not (String.left 6 line == "[label"))
                |> List.filter (\line -> line /= "")
                |> List.map (Parser.MathMacro.evalStr acc.mathMacroDict)

        --adjustedLines =
        --    List.map (Parser.MathMacro.evalStr acc.mathMacroDict) filteredLines
        --        |> List.filter (\line -> line /= "")
        --        |> List.map (\line -> line ++ "\\\\")
        leftPadding =
            Element.paddingEach { left = 45, right = 0, top = 0, bottom = 0 }
    in
    Element.column [ leftPadding ]
        [ mathText count w id DisplayMathMode (filteredLines |> String.join "\n") ]


getContent : ExpressionBlock -> String
getContent (ExpressionBlock { content }) =
    case content of
        Left str ->
            str

        Right _ ->
            ""


equation : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
equation count acc settings ((ExpressionBlock { id, args, error, properties }) as block) =
    let
        w =
            String.fromInt settings.width ++ "px"

        filteredLines =
            -- lines of math text to be rendered: filter stuff out
            String.lines (getContent block)
                |> List.filter (\line -> not (String.left 2 line == "$$") && not (String.left 6 line == "[label") && not (line == "end"))
                |> List.map (Parser.MathMacro.evalStr acc.mathMacroDict)

        adjustedLines =
            -- TODO: we need a better solution than the below for not messing up
            -- TODO internal \\begin-\\end pairs
            List.map (Parser.MathMacro.evalStr acc.mathMacroDict) filteredLines
                |> List.filter (\line -> line /= "")
                |> List.map
                    (\line ->
                        if String.left 6 line /= "\\begin" then
                            line ++ "\\\\"

                        else
                            line
                    )

        content =
            --String.join "\n" adjustedLines
            String.join "\n" filteredLines

        leftPadding =
            Element.paddingEach { left = 45, right = 0, top = 0, bottom = 0 }

        attrs =
            if id == settings.selectedId then
                [ Events.onClick (SendId id), leftPadding, Background.color (Element.rgb 0.8 0.8 1.0) ]

            else
                [ Events.onClick (SendId id), leftPadding ]

        attrs2 =
            if List.member "highlight" args then
                Background.color (Element.rgb 0.85 0.85 1.0) :: [ Element.centerX ]

            else
                [ Element.centerX ]
    in
    Element.column []
        [ Element.row ([ Element.width (Element.px settings.width), Render.Utility.elementAttribute "id" id ] ++ attrs)
            [ Element.el attrs2 (mathText count w id DisplayMathMode content)
            , Element.el [ Element.alignRight, Font.size 12, equationLabelPadding ] (Element.text <| "(" ++ getLabel "equation" properties ++ ")")
            ]
        ]


getCounter : String -> Dict String Int -> String
getCounter counterName dict =
    Dict.get counterName dict |> Maybe.withDefault 0 |> String.fromInt


getLabel : String -> Dict String String -> String
getLabel label dict =
    Dict.get label dict |> Maybe.withDefault ""


aligned : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
aligned count acc settings ((ExpressionBlock { id, args, properties, error }) as block) =
    Element.column []
        [ Element.row [ Element.width (Element.px settings.width), Render.Utility.elementAttribute "id" id ]
            [ Element.el [ Element.centerX ] (aligned_ count acc settings args id (getContent block))
            , Element.el [ Element.alignRight, Font.size 12, equationLabelPadding ] (Element.text <| "(" ++ getLabel "equation" properties ++ ")")
            ]
        ]


aligned_ count acc settings _ id str =
    let
        w =
            String.fromInt settings.width ++ "px"

        filteredLines =
            -- lines of math text to be rendered: filter stuff out
            String.lines str
                |> List.filter (\line -> not (String.left 6 line == "[label") && not (line == ""))

        leftPadding =
            Element.paddingEach { left = 45, right = 0, top = 0, bottom = 0 }

        attrs =
            if id == settings.selectedId then
                [ Events.onClick (SendId id), leftPadding, Background.color (Element.rgb 0.8 0.8 1.0) ]

            else
                [ Events.onClick (SendId id), leftPadding ]

        deleteTrailingSlashes str_ =
            if String.right 2 str_ == "\\\\" then
                String.dropRight 2 str_

            else
                str_

        adjustedLines_ =
            List.map (deleteTrailingSlashes >> Parser.MathMacro.evalStr acc.mathMacroDict) filteredLines
                |> List.filter (\line -> line /= "")
                |> List.map (\line -> line ++ "\\\\")

        adjustedLines =
            "\\begin{aligned}" :: adjustedLines_ ++ [ "\\end{aligned}" ]

        content =
            String.join "\n" adjustedLines
    in
    Element.column attrs
        [ mathText count w id DisplayMathMode content ]


equationLabelPadding =
    Element.paddingEach { left = 0, right = 18, top = 0, bottom = 0 }


mathText : Int -> String -> String -> DisplayMode -> String -> Element msg
mathText generation width id displayMode content =
    -- the code 'String.replace "\\ \\" "\\\\"'
    -- is needed because for some reason "\\\\" gets expanded to "\\ \\"
    -- TODO Track this down at the source.
    Html.Keyed.node "span"
        [ HA.style "padding-top" "14px"
        , HA.style "padding-bottom" "14px"
        , HA.id id
        , HA.style "width" width
        ]
        [ ( String.fromInt generation, mathText_ displayMode (eraseLabeMacro content) )
        ]
        |> Element.html


eraseLabeMacro content =
    content |> String.lines |> List.map (Parser.TextMacro.eraseLeadingMacro "label") |> String.join "\n"


mathText_ : DisplayMode -> String -> Html msg
mathText_ displayMode content =
    Html.node "math-text"
        -- active meta selectedId  ++
        [ HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string content)

        -- , clicker meta
        -- , HA.id (makeId meta)
        ]
        []


isDisplayMathMode : DisplayMode -> Bool
isDisplayMathMode displayMode =
    case displayMode of
        InlineMathMode ->
            False

        DisplayMathMode ->
            True
