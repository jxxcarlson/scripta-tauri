
module View.Utility exposing (jumpToTop, setViewportForElement, getElementWithViewPort, setViewPortForSelectedLine)

import Browser.Dom
import Task
import Model exposing(Msg(..))
import Config


jumpToTop : String -> Cmd Msg
jumpToTop viewportId =
    Browser.Dom.getViewportOf viewportId
        |> Task.andThen (\info -> Browser.Dom.setViewportOf viewportId 0 0)
        |> Task.attempt (\_ -> NoOp)



setViewportForElement : String -> String -> Cmd Msg
setViewportForElement viewportId elementId =
    Browser.Dom.getViewportOf viewportId
        |> Task.andThen (\vp -> getElementWithViewPort vp elementId) 
        |> Task.attempt SetViewPortForElement 

getElementWithViewPort : Browser.Dom.Viewport -> String -> Task.Task Browser.Dom.Error (Browser.Dom.Element, Browser.Dom.Viewport )
getElementWithViewPort vp id =
    Browser.Dom.getElement id
        |> Task.map (\el -> ( el, vp ))


setViewPortForSelectedLine :  Browser.Dom.Element -> Browser.Dom.Viewport -> Cmd Msg
setViewPortForSelectedLine element viewport =
    let
        y =
            -- viewport.viewport.y + element.element.y - element.element.height - 380
            viewport.viewport.y + element.element.y - element.element.height - 380
    in
      Task.attempt (\_ -> NoOp) (Browser.Dom.setViewportOf Config.renderedTextViewportID 0 y)

