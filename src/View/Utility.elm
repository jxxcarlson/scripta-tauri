
module View.Utility exposing (..)

import Browser.Dom
import Task
import Model exposing(Msg(..))

setViewportForElement : String -> String -> Cmd Msg
setViewportForElement viewportId elementId =
    Browser.Dom.getViewportOf viewportId
        |> Task.andThen (\vp -> getElementWithViewPort vp elementId)
        |> Task.attempt SetViewPortForElement


getElementWithViewPort : Browser.Dom.Viewport -> String -> Task.Task Browser.Dom.Error (Browser.Dom.Element, Browser.Dom.Viewport )
getElementWithViewPort vp id =
    Browser.Dom.getElement id
        |> Task.map (\el -> ( el, vp ))
