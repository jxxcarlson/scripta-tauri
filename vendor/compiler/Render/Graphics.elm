module Render.Graphics exposing (image, image2, quiver, svg, tikz)

import Compiler.ASTTools as ASTTools
import Compiler.Acc exposing (Accumulator)
import Dict exposing (Dict)
import Either exposing (Either(..))
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, px, rgb255, spacing)
import Element.Font as Font
import Parser.Block exposing (ExpressionBlock(..))
import Parser.Expr exposing (Expr)
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Utility
import SvgParser


red : Element.Color
red =
    rgb255 255 0 0


type alias ImageParameters msg =
    { caption : Element msg
    , description : String
    , placement : Element.Attribute msg
    , width : Element.Length
    , url : String
    }


image : Render.Settings.Settings -> List Expr -> Element msg
image settings body =
    let
        params =
            body |> argumentsFromAST |> imageParameters settings

        inner =
            column [ spacing 8, Element.width (px settings.width), params.placement, Element.paddingXY 0 18 ]
                [ Element.image [ Element.width params.width, params.placement ]
                    { src = params.url, description = params.description }
                , el [ params.placement ] params.caption
                ]
    in
    Element.newTabLink []
        { url = params.url
        , label = inner
        }


image2 : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
image2 _ _ settings (ExpressionBlock { id, args, properties, content }) =
    let
        caption =
            getCaption properties

        label =
            case caption of
                "" ->
                    "Figure " ++ getFigureLabel properties

                _ ->
                    "Figure " ++ getFigureLabel properties ++ ". " ++ caption

        url =
            case content of
                Left str ->
                    str

                Right _ ->
                    "bad block"

        params =
            parameters settings properties

        inner =
            column
                [ spacing 8
                , Element.width (px settings.width)
                , params.placement
                , Element.paddingXY 0 18
                ]
                [ Element.image [ Element.width params.width, params.placement ]
                    { src = url, description = getDescription properties }
                , el
                    ([ params.placement, Render.Utility.elementAttribute "id" id, Element.paddingXY 12 4 ]
                        ++ Render.Utility.highlightElement id settings.selectedId
                    )
                    (Element.text label)
                ]
    in
    Element.newTabLink []
        { url = url
        , label = inner
        }



-- Property Helpers


getFigureLabel : Dict String String -> String
getFigureLabel dict =
    Dict.get "figure" dict |> Maybe.withDefault ""


getWidth : Dict String String -> Element.Length
getWidth properties =
    Dict.get "width" properties |> Maybe.andThen String.toInt |> Maybe.withDefault 400 |> Element.px


getCaption : Dict String String -> String
getCaption properties =
    Dict.get "caption" properties |> Maybe.withDefault ""


getDescription : Dict String String -> String
getDescription properties =
    Dict.get "description" properties |> Maybe.withDefault ""


getPlacement : Dict String String -> String
getPlacement properties =
    Dict.get "placement" properties |> Maybe.withDefault ""


getVerbatimContent : ExpressionBlock -> String
getVerbatimContent (ExpressionBlock { content }) =
    case content of
        Left str ->
            str

        Right _ ->
            ""


svg : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
svg ount acc settings ((ExpressionBlock { id, args }) as block) =
    case SvgParser.parse (getVerbatimContent block) of
        Ok html_ ->
            Element.column
                [ Element.paddingEach { left = 0, right = 0, top = 24, bottom = 0 }
                , Element.width (Element.px settings.width)
                ]
                [ Element.column [ Element.centerX ] [ html_ |> Element.html ]
                ]

        Err _ ->
            Element.el [] (Element.text "SVG parse error")


{-| Create elements from HTML markup. On parsing error, output no elements.
-}
tikz : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
tikz count acc settings ((ExpressionBlock { id, args }) as block) =
    let
        maybePair =
            case String.split "---" (getVerbatimContent block) of
                a :: b :: [] ->
                    Just ( a, b )

                _ ->
                    Nothing
    in
    case maybePair of
        Nothing ->
            Element.el [ Font.size 16, Font.color red ] (Element.text "Something is wrong")

        Just ( imageData, _ ) ->
            let
                params =
                    String.words imageData |> imageParameters settings
            in
            Element.column [ Element.spacing 8, Element.width (Element.px settings.width), params.placement, Element.paddingXY 0 18 ]
                [ Element.image [ Element.width params.width, params.placement ]
                    { src = params.url, description = params.description }
                , Element.el [ params.placement ] params.caption
                ]


quiver : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
quiver _ _ settings ((ExpressionBlock { id, args, properties }) as block) =
    let
        -- arguments: ["width:250","caption:Fig","1"]
        qArgs : { caption : String, description : String, placement : Element.Attribute a, width : Element.Length }
        qArgs =
            parameters settings properties

        maybePair =
            case String.split "---" (getVerbatimContent block) of
                a :: b :: [] ->
                    Just ( a, b )

                _ ->
                    Nothing
    in
    case maybePair of
        Nothing ->
            Element.el [ Font.size 16, Font.color red ] (Element.text "Something is wrong")

        Just ( imageData, _ ) ->
            let
                params =
                    String.words imageData |> imageParameters settings

                desc =
                    case qArgs.caption of
                        "" ->
                            "Figure " ++ getFigureLabel properties

                        _ ->
                            "Figure " ++ getFigureLabel properties ++ ". " ++ qArgs.caption
            in
            Element.column
                [ Element.spacing 8
                , Element.width (Element.px settings.width)
                ]
                [ Element.image [ Element.width qArgs.width, params.placement ]
                    { src = params.url, description = desc }
                , Element.el
                    ([ params.placement, params.placement, Element.paddingXY 12 4, Render.Utility.elementAttribute "id" id ]
                        ++ Render.Utility.highlightElement id settings.selectedId
                    )
                    (Element.text desc)
                ]


argumentsFromAST : List Expr -> List String
argumentsFromAST body =
    ASTTools.exprListToStringList body |> List.map String.words |> List.concat


imageParameters : Render.Settings.Settings -> List String -> ImageParameters msg
imageParameters settings arguments =
    let
        url =
            List.head arguments |> Maybe.withDefault "no-image"

        remainingArguments =
            List.drop 1 arguments

        keyValueStrings_ =
            List.filter (\s -> String.contains ":" s) remainingArguments

        keyValueStrings : List String
        keyValueStrings =
            List.filter (\s -> not (String.contains "caption" s)) keyValueStrings_

        captionLeadString =
            List.filter (\s -> String.contains "caption" s) keyValueStrings_
                |> String.join ""
                |> String.replace "caption:" ""

        captionPhrase =
            (captionLeadString :: List.filter (\s -> not (String.contains ":" s)) remainingArguments) |> String.join " "

        dict =
            Render.Utility.keyValueDict keyValueStrings

        description : String
        description =
            Dict.get "caption" dict |> Maybe.withDefault ""

        caption : Element msg
        caption =
            if captionPhrase == "" then
                Element.none

            else
                Element.row [ placement, Element.width Element.fill ] [ el [ Element.width Element.fill ] (Element.text captionPhrase) ]

        displayWidth =
            settings.width

        width : Element.Length
        width =
            case Dict.get "width" dict of
                Nothing ->
                    px displayWidth

                Just "fill" ->
                    Element.fill

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            px displayWidth

                        Just w ->
                            px w

        placement =
            case Dict.get "placement" dict of
                Nothing ->
                    centerX

                Just "left" ->
                    alignLeft

                Just "right" ->
                    alignRight

                Just "center" ->
                    centerX

                _ ->
                    centerX
    in
    { caption = caption, description = description, placement = placement, width = width, url = url }


parameters : Settings -> Dict String String -> { caption : String, description : String, placement : Element.Attribute msg, width : Element.Length }
parameters settings properties =
    let
        captionPhrase =
            getCaption properties

        description : String
        description =
            getDescription properties

        displayWidth =
            settings.width

        width : Element.Length
        width =
            case Dict.get "width" properties of
                Nothing ->
                    px displayWidth

                Just "fill" ->
                    Element.fill

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            px displayWidth

                        Just w ->
                            px w

        placement =
            case Dict.get "placement" properties of
                Nothing ->
                    centerX

                Just "left" ->
                    alignLeft

                Just "right" ->
                    alignRight

                Just "center" ->
                    centerX

                _ ->
                    centerX
    in
    { caption = captionPhrase, description = description, placement = placement, width = width }
