module Render.Data exposing (chart, table)

import Chart
import Chart.Attributes as CA
import Chart.Svg exposing (Axis)
import Compiler.Acc exposing (Accumulator)
import Dict
import Either exposing (Either(..))
import Element exposing (Element)
import Element.Font as Font
import List.Extra
import Maybe.Extra
import Parser.Block exposing (ExpressionBlock(..))
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Utility


red =
    Element.rgb255 255 0 0


type alias Options =
    { timeseries : Maybe String
    , reverse : Maybe String
    , columns : Maybe (List Int)
    , lowest : Maybe Float
    , caption : Maybe String
    , label : Maybe String
    , kind : Maybe String -- e.g, kind:line or --kind:scatter
    , domain : Maybe Range
    , range : Maybe Range
    }


type alias Range =
    { lowest : Maybe Float, highest : Maybe Float }


fontWidth =
    10


chart : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
chart count acc settings ((ExpressionBlock { id, args, properties }) as block) =
    let
        options : Options
        options =
            { timeseries = getArg "timeseries" args
            , reverse = getArg "reverse" args
            , columns = Dict.get "columns" properties |> Maybe.map (String.split "," >> List.map String.trim >> List.map String.toInt >> Maybe.Extra.values)
            , lowest = Dict.get "lowest" properties |> Maybe.andThen String.toFloat
            , caption = Dict.get "caption" properties
            , label = Dict.get "figure" properties
            , kind = Dict.get "kind" properties
            , domain = Dict.get "domain" properties |> Maybe.andThen getRange
            , range = Dict.get "range" properties |> Maybe.andThen getRange
            }

        data : Maybe ChartData
        data =
            csvToChartData options (getVerbatimContent block)
    in
    Element.column [ Element.width (Element.px settings.width), Element.paddingEach { left = 48, right = 0, top = 36, bottom = 72 }, Element.spacing 24 ]
        [ Element.el [ Element.width (Element.px settings.width) ]
            (rawLineChart options data)
        , case ( options.label, options.caption ) of
            ( Nothing, Nothing ) ->
                Element.none

            ( Just labelText, Nothing ) ->
                Element.el [ Element.centerX, Font.size 14, Font.color (Element.rgb 0.5 0.5 0.7), Element.paddingEach { left = 0, right = 0, top = 24, bottom = 0 } ] (Element.text <| "Figure " ++ labelText)

            ( Nothing, Just captionText ) ->
                Element.el [ Element.centerX, Font.size 14, Font.color (Element.rgb 0.5 0.5 0.7), Element.paddingEach { left = 0, right = 0, top = 24, bottom = 0 } ] (Element.text <| captionText)

            ( Just labelText, Just captionText ) ->
                Element.el [ Element.centerX, Font.size 14, Font.color (Element.rgb 0.5 0.5 0.7), Element.paddingEach { left = 0, right = 0, top = 24, bottom = 0 } ] (Element.text <| "Figure " ++ labelText ++ ". " ++ captionText)
        ]


type ChartData
    = ChartData2D (List { x : Float, y : Float })
    | ChartData3D (List { x : Float, y : Float, z : Float })
    | ChartData4D (List { x : Float, y : Float, z : Float, w : Float })


select : Maybe (List Int) -> List a -> Maybe (List a)
select columns_ data =
    case columns_ of
        Nothing ->
            Just data

        Just columns ->
            let
                selectors : List (List a -> Maybe a)
                selectors =
                    List.map List.Extra.getAt columns
            in
            applyFunctions selectors data |> Maybe.Extra.combine


selectColumns : Maybe (List Int) -> List (List a) -> Maybe (List (List a))
selectColumns columns data =
    if columns == Just [] then
        Just data

    else
        data
            |> List.Extra.transpose
            |> select columns
            |> Maybe.map List.Extra.transpose


makeTimeseries : List (List String) -> List (List String)
makeTimeseries data =
    List.indexedMap (\i oneList -> String.fromInt i :: oneList) data


csvToChartData : Options -> String -> Maybe ChartData
csvToChartData options str =
    let
        dataLines : List String
        dataLines =
            str
                |> String.lines
                |> List.filter (\line -> String.trim line /= "" && String.left 1 line /= "#")
                |> maybeApply options.reverse List.reverse

        data_ : Maybe (List (List String))
        data_ =
            case options.timeseries of
                Just _ ->
                    List.map (String.split "," >> List.map String.trim) dataLines
                        |> selectColumns options.columns
                        |> Maybe.map makeTimeseries

                Nothing ->
                    List.map (String.split "," >> List.map String.trim) dataLines
                        |> selectColumns options.columns

        dimension : Maybe Int
        dimension =
            data_ |> Maybe.andThen List.head |> Maybe.map List.length
    in
    case ( dimension, data_ ) of
        ( Nothing, _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing

        ( Just 2, Just data ) ->
            Just (ChartData2D (csvTo2DData data))

        ( Just 3, Just data ) ->
            Just (ChartData3D (csvTo3DData data))

        _ ->
            Nothing


csvTo2DData : List (List String) -> List { x : Float, y : Float }
csvTo2DData data =
    data
        |> List.map listTo2DPoint
        |> Maybe.Extra.values


csvTo3DData : List (List String) -> List { x : Float, y : Float, z : Float }
csvTo3DData data =
    data
        |> List.map listTo3DPoint
        |> Maybe.Extra.values


listTo2DPoint : List String -> Maybe { x : Float, y : Float }
listTo2DPoint list =
    case list of
        x :: y :: rest ->
            ( String.toFloat (String.trim x), String.toFloat (String.trim y) ) |> valueOfPair |> Maybe.map (\( u, v ) -> { x = u, y = v })

        _ ->
            Nothing


listTo3DPoint : List String -> Maybe { x : Float, y : Float, z : Float }
listTo3DPoint list =
    case list of
        x :: y :: z :: rest ->
            ( String.toFloat (String.trim x), String.toFloat (String.trim y), String.toFloat (String.trim z) ) |> valueOfTriple |> Maybe.map (\( u, v, w ) -> { x = u, y = v, z = w })

        _ ->
            Nothing


listTo4DPoint : List String -> Maybe { x : Float, y : Float }
listTo4DPoint list =
    case list of
        x :: y :: rest ->
            ( String.toFloat (String.trim x), String.toFloat (String.trim y) ) |> valueOfPair |> Maybe.map (\( u, v ) -> { x = u, y = v })

        _ ->
            Nothing


valueOfPair : ( Maybe a, Maybe b ) -> Maybe ( a, b )
valueOfPair ( ma, mb ) =
    case ( ma, mb ) of
        ( Just a, Just b ) ->
            Just ( a, b )

        _ ->
            Nothing


valueOfTriple : ( Maybe a, Maybe b, Maybe c ) -> Maybe ( a, b, c )
valueOfTriple ( ma, mb, mc ) =
    case ( ma, mb, mc ) of
        ( Just a, Just b, Just c ) ->
            Just ( a, b, c )

        _ ->
            Nothing


rawLineChart : Options -> Maybe ChartData -> Element msg
rawLineChart options mChartData =
    case mChartData of
        Nothing ->
            Element.el [ Font.size 14, Font.color red ] (Element.text "Line chart: Error parsing data")

        Just (ChartData2D data) ->
            rawLineChart2D options data

        Just (ChartData3D data) ->
            rawLineChart3D data

        _ ->
            Element.el [ Font.size 14, Font.color red ] (Element.text "Line chart: Error, can only handle 2D data")


expandRange : { a | lowest : Maybe Float, highest : Maybe Float } -> List (Axis -> Axis)
expandRange { lowest, highest } =
    let
        low =
            case lowest of
                Nothing ->
                    CA.lowest 0 CA.orLower

                Just u ->
                    CA.lowest u CA.exactly

        high =
            case highest of
                Nothing ->
                    CA.highest 100 CA.orHigher

                Just u ->
                    CA.highest u CA.exactly
    in
    [ low, high ]


foo : List (Axis -> Axis)
foo =
    [ CA.lowest -5 CA.orLower

    -- Makes sure that your x-axis begins at -5 or lower, no matter
    -- what your data is like.
    , CA.highest 10 CA.orHigher

    -- Makes sure that your x-axis ends at 10 or higher, no matter
    -- what your data is like.
    ]


rawLineChart2D : Options -> List { x : Float, y : Float } -> Element msg
rawLineChart2D options data =
    let
        domain =
            case options.domain of
                Nothing ->
                    CA.domain []

                Just range_ ->
                    CA.domain (expandRange range_)

        range =
            case options.range of
                Nothing ->
                    CA.range []

                Just range_ ->
                    CA.range (expandRange range_)
    in
    Chart.chart
        [ CA.height 200
        , CA.width 400
        , case options.lowest of
            Nothing ->
                CA.domain []

            Just lowest ->
                CA.domain [ CA.lowest lowest CA.orLower ]

        --, CA.range []
        --, CA.domain []
        ]
        [ Chart.xLabels [ CA.fontSize 10 ]
        , Chart.yLabels [ CA.withGrid, CA.fontSize 10 ]
        , case options.kind of
            Just "line" ->
                Chart.series .x [ Chart.interpolated .y [ CA.color CA.red ] [] ] data

            Just "scatter" ->
                Chart.series .x [ Chart.scatter .y [] ] data

            Just "bar" ->
                Chart.bars []
                    [ Chart.bar .y []
                    ]
                    data

            _ ->
                Chart.series .x [ Chart.interpolated .y [ CA.color CA.red ] [] ] data
        ]
        |> Element.html


rawLineChart3D : List { x : Float, y : Float, z : Float } -> Element msg
rawLineChart3D data =
    Chart.chart
        [ CA.height 200
        , CA.width 400
        ]
        [ Chart.xLabels [ CA.fontSize 10 ]
        , Chart.yLabels [ CA.withGrid, CA.fontSize 10 ]
        , Chart.series .x
            [ Chart.interpolated .y [ CA.color CA.red ] []
            , Chart.interpolated .z [ CA.color CA.darkBlue ] []
            ]
            data
        ]
        |> Element.html



-- UTILTIES


applyFunctions : List (a -> b) -> a -> List b
applyFunctions fs a =
    List.foldl (\f acc -> f a :: acc) [] fs |> List.reverse


applyIf : Bool -> (a -> a) -> a -> a
applyIf flag f x =
    if flag then
        f x

    else
        x


maybeApply : Maybe a -> (b -> b) -> b -> b
maybeApply maybe f x =
    case maybe of
        Just _ ->
            f x

        Nothing ->
            x


maybeChoose : Maybe a -> (b -> b) -> (b -> b) -> b -> b
maybeChoose maybe f g x =
    case maybe of
        Just _ ->
            f x

        Nothing ->
            g x



-- ARG


getColumns : List String -> Maybe (List Int)
getColumns args =
    case getArg "columns" args of
        Nothing ->
            Nothing

        Just argList ->
            parseArg argList |> List.map String.toInt |> Maybe.Extra.values |> Just


getFloat : Maybe String -> Maybe Float
getFloat str =
    str
        --|> Maybe.map (String.split ":")
        --|> Maybe.map (List.drop 1)
        --|> Maybe.andThen List.head
        |> getString
        |> Maybe.andThen String.toFloat


getString : Maybe String -> Maybe String
getString str =
    str
        |> Maybe.map (String.split ":")
        |> Maybe.map (List.drop 1)
        |> Maybe.andThen List.head


getArgAfter : String -> List String -> Maybe String
getArgAfter label args =
    case List.Extra.findIndex (\item -> String.contains label item) args of
        Nothing ->
            Nothing

        Just k ->
            let
                a =
                    List.Extra.getAt k args |> Maybe.withDefault "" |> String.replace (label ++ ":") ""

                b =
                    List.drop (k + 1) args |> String.join " "
            in
            Just (a ++ b)


getArg : String -> List String -> Maybe String
getArg name args =
    List.filter (\item -> String.contains name item) args |> List.head


parseArg : String -> List String
parseArg arg =
    let
        parts =
            String.split ":" arg
    in
    case parts of
        [] ->
            []

        name :: [] ->
            []

        name :: argString :: [] ->
            String.split "," argString

        _ ->
            []


getRange : String -> Maybe Range
getRange str =
    case str |> String.split "," |> List.map String.trim |> List.take 2 of
        low :: high :: [] ->
            Just { lowest = String.toFloat low, highest = String.toFloat high }

        _ ->
            Nothing


getVerbatimContent : ExpressionBlock -> String
getVerbatimContent (ExpressionBlock { content }) =
    case content of
        Left str ->
            str

        Right _ ->
            ""


table : Int -> Accumulator -> Settings -> ExpressionBlock -> Element MarkupMsg
table count acc settings ((ExpressionBlock { id, args, properties }) as block) =
    let
        title =
            case Dict.get "title" properties of
                Nothing ->
                    Element.none

                Just title_ ->
                    Element.el [ Font.bold ] (Element.text title_)

        columnsToDisplay : List Int
        columnsToDisplay =
            Dict.get "columns" properties
                |> Maybe.map (String.split ",")
                |> Maybe.withDefault []
                |> List.map (String.trim >> String.toInt)
                |> Maybe.Extra.values
                |> List.map (\n -> n - 1)

        lines =
            String.split "\n" (getVerbatimContent block)

        rawCells : List (List String)
        rawCells =
            List.map (String.split ",") lines
                |> List.map (List.map String.trim)

        selectedCells : List (List String)
        selectedCells =
            if columnsToDisplay == [] then
                rawCells

            else
                let
                    cols : List ( Int, List String )
                    cols =
                        List.Extra.transpose rawCells |> List.indexedMap (\k col -> ( k, col ))

                    updater : ( Int, List String ) -> List (List String) -> List (List String)
                    updater =
                        \( k, col ) acc_ ->
                            if List.member k columnsToDisplay then
                                col :: acc_

                            else
                                acc_

                    selectedCols =
                        List.foldl updater [] cols
                in
                List.Extra.transpose (List.reverse selectedCols)

        columnWidths : List Int
        columnWidths =
            List.map (List.map String.length) selectedCells
                |> List.Extra.transpose
                |> List.map (\column -> List.maximum column |> Maybe.withDefault 1)
                |> List.map (\w -> fontWidth * w)

        renderRow : Int -> List Int -> List String -> Element MarkupMsg
        renderRow rowNumber widths_ cells_ =
            let
                totalWidth =
                    List.sum widths_ + 0
            in
            if rowNumber == 0 then
                Element.row [ Element.width (Element.px totalWidth) ] (List.map2 (\cell width -> Element.el [ Element.width (Element.px width), Font.underline ] (Element.text <| String.replace "_" "" cell)) cells_ widths_)

            else
                Element.row [ Element.width (Element.px totalWidth) ] (List.map2 (\cell width -> Element.el [ Element.width (Element.px width) ] (Element.text cell)) cells_ widths_)
    in
    -- Element.column [ Element.spacing 12, Element.paddingEach { left = 36, right = 0, top = 18, bottom = 18 } ] (title :: List.map (renderRow 1 columnWidths) selectedCells)
    Element.column [ Element.spacing 12, Element.paddingEach { left = 36, right = 0, top = 18, bottom = 18 } ] (title :: List.indexedMap (\k row -> renderRow k columnWidths row) selectedCells)
