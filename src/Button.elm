module Button exposing (ButtonData, simpleTemplate, template)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes


template : ButtonData msg -> Element msg
template buttonData =
    row ([ bgGray 0.2, pointer, mouseDown [ Background.color darkRed ] ] ++ buttonData.attributes)
        [ Input.button buttonStyle
            { onPress = Just buttonData.msg
            , label = addTooltip buttonData.tooltipPlacement buttonData.tooltipText (el [ centerX, centerY, Font.size 14 ] (text buttonData.label))
            }
        ]


simpleTemplate : List (Attribute msg) -> msg -> String -> Element msg
simpleTemplate attrList msg label_ =
    row ([ bgGray 0.2, pointer, mouseDown [ Background.color darkRed ] ] ++ attrList)
        [ Input.button buttonStyle
            { onPress = Just msg
            , label = el [ centerX, centerY, Font.size 14 ] (text label_)
            }
        ]


type alias ButtonData msg =
    { tooltipText : String
    , tooltipPlacement : Element msg -> Attribute msg
    , attributes : List (Attribute msg)
    , msg : msg
    , label : String
    }


darkRed : Color
darkRed =
    rgb255 140 0 0


addTooltip placement label element =
    el
        [ tooltip placement (myTooltip label) ]
        element


tooltip : (Element msg -> Attribute msg) -> Element Never -> Attribute msg
tooltip usher tooltip_ =
    inFront <|
        el
            [ width fill
            , height fill
            , transparent True
            , mouseOver [ transparent False ]
            , (usher << map never) <|
                el
                    [ htmlAttribute (Html.Attributes.style "pointerEvents" "none") ]
                    tooltip_
            ]
            none


myTooltip : String -> Element msg
myTooltip str =
    el
        [ Background.color (rgb 0 0 0)
        , Font.color (rgb 1 1 1)
        , padding 4
        , Border.rounded 5
        , Font.size 14
        , Border.shadow
            { offset = ( 0, 3 ), blur = 6, size = 0, color = rgba 0 0 0 0.32 }
        ]
        (text str)


buttonStyle : List (Element.Attr () msg)
buttonStyle =
    [ Font.color (Element.rgb255 255 255 255)
    , Element.paddingXY 15 8
    ]


bgGray : Float -> Element.Attr decorative msg
bgGray g =
    Background.color (Element.rgb g g g)
