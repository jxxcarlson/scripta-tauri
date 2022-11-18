module Render.Settings exposing
    ( Settings
    , blueColor
    , codeColor
    , defaultSettings
    , leftIndent
    , leftIndentation
    , leftRightIndentation
    , makeSettings
    , maxHeadingFontSize
    , redColor
    , topMarginForChildren
    , wideLeftIndentation
    , windowWidthScale
    )

import Element


type alias Settings =
    { paragraphSpacing : Int
    , selectedId : String
    , selectedSlug : Maybe String
    , showErrorMessages : Bool
    , showTOC : Bool
    , titleSize : Int
    , width : Int
    , backgroundColor : Element.Color
    , titlePrefix : String
    , isStandaloneDocument : Bool
    }


defaultSettings : Settings
defaultSettings =
    makeSettings "" Nothing 1 600


makeSettings : String -> Maybe String -> Float -> Int -> Settings
makeSettings id selectedSlug scale width =
    { width = round (scale * toFloat width)
    , titleSize = 30
    , paragraphSpacing = 28
    , showTOC = True
    , showErrorMessages = False
    , selectedId = id
    , selectedSlug = selectedSlug
    , backgroundColor = Element.rgb 1 1 1
    , titlePrefix = ""
    , isStandaloneDocument = False
    }


codeColor =
    Element.rgb255 0 0 210


windowWidthScale =
    0.3


maxHeadingFontSize : Float
maxHeadingFontSize =
    32


leftIndent =
    18


topMarginForChildren =
    6


leftIndentation =
    Element.paddingEach { left = 18, right = 0, top = 0, bottom = 0 }


wideLeftIndentation =
    Element.paddingEach { left = 54, right = 0, top = 0, bottom = 0 }


leftRightIndentation =
    Element.paddingEach { left = 18, right = 8, top = 0, bottom = 0 }


redColor =
    Element.rgb 0.7 0 0


blueColor =
    Element.rgb 0 0 0.9
