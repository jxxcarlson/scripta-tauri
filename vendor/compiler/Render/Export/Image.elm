module Render.Export.Image exposing (export, exportBlock)

import Compiler.ASTTools
import Dict
import Either exposing (Either(..))
import List.Extra
import Parser.Block exposing (ExpressionBlock(..))
import Parser.Expr exposing (Expr)
import Render.Export.Util
import Render.Settings exposing (Settings)
import Render.Utility


exportBlock : Settings -> ExpressionBlock -> String
exportBlock settings ((ExpressionBlock { content, args }) as block) =
    let
        params =
            imageParameters3 settings block

        options =
            [ params.fractionalWidth, ",keepaspectratio" ] |> String.join ""
    in
    exportCenteredFigure (normalizeUrl params.url) options params.caption


fixWidth : String -> String
fixWidth w =
    if w == "" || w == "fill" then
        "500"

    else
        w


export : Settings -> List Expr -> String
export s exprs =
    let
        args =
            Render.Export.Util.getOneArg exprs |> String.words

        params =
            imageParameters s exprs

        options =
            [ params.width |> fixWidth, ",keepaspectratio" ] |> String.join ""
    in
    case List.head args of
        Nothing ->
            "ERROR IN IMAGE"

        Just url_ ->
            if params.placement == "C" then
                exportCenteredFigure (normalizeUrl url_) options params.caption

            else
                exportWrappedFigure params.placement (normalizeUrl url_) params.fractionalWidth params.caption


normalizeUrl : String -> String
normalizeUrl url_ =
    case url_ |> String.split "/" |> List.Extra.last of
        Nothing ->
            url_

        Just url ->
            url


exportCenteredFigure url options caption =
    if caption == "" then
        [ "\\imagecenter{", url, "}{" ++ options ++ "}" ] |> String.join ""

    else
        [ "\\imagecentercaptioned{", url, "}{" ++ options ++ "}{" ++ caption ++ "}" ] |> String.join ""


exportWrappedFigure placement url options caption =
    [ "\\imagefloat{", url, "}{" ++ options ++ "}{" ++ caption ++ "}{" ++ placement ++ "}" ] |> String.join ""


type alias ImageParameters =
    { caption : String
    , description : String
    , placement : String
    , width : String
    , fractionalWidth : String
    , url : String
    }


imageParameters : Render.Settings.Settings -> List Expr -> ImageParameters
imageParameters settings body =
    let
        arguments : List String
        arguments =
            Compiler.ASTTools.exprListToStringList body |> List.map String.words |> List.concat

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

        caption =
            (captionLeadString :: List.filter (\s -> not (String.contains ":" s)) remainingArguments) |> String.join " "

        dict =
            Render.Utility.keyValueDict keyValueStrings

        description =
            Dict.get "caption" dict |> Maybe.withDefault ""

        displayWidth =
            settings.width

        width : String
        width =
            case Dict.get "width" dict of
                Nothing ->
                    rescale displayWidth displayWidth

                Just "fill" ->
                    rescale displayWidth displayWidth

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            rescale displayWidth displayWidth

                        Just w ->
                            rescale displayWidth w

        fractionalWidth : String
        fractionalWidth =
            case Dict.get "width" dict of
                Nothing ->
                    fractionaRescale displayWidth

                Just "fill" ->
                    fractionaRescale displayWidth

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            fractionaRescale displayWidth

                        Just w ->
                            fractionaRescale w

        placement =
            case Dict.get "placement" dict of
                Nothing ->
                    "C"

                Just "left" ->
                    "L"

                Just "right" ->
                    "R"

                Just "center" ->
                    "C"

                _ ->
                    "C"
    in
    { caption = caption, description = description, placement = placement, width = width, fractionalWidth = fractionalWidth, url = url }


imageParameters3 : Render.Settings.Settings -> ExpressionBlock -> ImageParameters
imageParameters3 settings (ExpressionBlock { content, args, properties }) =
    let
        arguments : List String
        arguments =
            args

        url =
            case content of
                Left str ->
                    String.replace "https://" "" str

                Right _ ->
                    "bad block"

        _ =
            url

        caption =
            Dict.get "caption" properties |> Maybe.withDefault ""

        displayWidth =
            settings.width

        width : String
        width =
            case Dict.get "width" properties of
                Nothing ->
                    rescale displayWidth displayWidth

                Just "fill" ->
                    rescale displayWidth displayWidth

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            rescale displayWidth displayWidth

                        Just w ->
                            rescale displayWidth w

        fractionalWidth : String
        fractionalWidth =
            case Dict.get "width" properties of
                Nothing ->
                    "0.51\\textwidth"

                Just "fill" ->
                    fractionaRescale displayWidth

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            fractionaRescale displayWidth

                        Just w ->
                            fractionaRescale w

        placement =
            case Dict.get "placement" properties of
                Nothing ->
                    "C"

                Just "left" ->
                    "L"

                Just "right" ->
                    "R"

                Just "center" ->
                    "C"

                _ ->
                    "C"
    in
    { caption = caption, description = caption, placement = placement, width = width, fractionalWidth = fractionalWidth, url = url }


rescale : Int -> Int -> String
rescale displayWidth k =
    (toFloat k * (6.0 / toFloat displayWidth) |> String.fromFloat) ++ "truein"


fractionaRescale : Int -> String
fractionaRescale k =
    let
        f =
            (toFloat k / 600.0) |> String.fromFloat
    in
    [ f, "\\textwidth" ] |> String.join ""
