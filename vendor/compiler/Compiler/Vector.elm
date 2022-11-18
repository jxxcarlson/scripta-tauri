module Compiler.Vector exposing (Vector, get, increment, init, level, resetFrom, set, toString)

import List.Extra


type alias Vector =
    { size : Int, content : List Int }


init : Int -> Vector
init k =
    { size = k, content = List.repeat k 0 }


toString : Vector -> String
toString v =
    v.content
        |> List.filter (\x -> x > 0)
        |> List.map String.fromInt
        |> String.join "."


get : Int -> Vector -> Int
get k v =
    List.Extra.getAt k v.content |> Maybe.withDefault 0


set : Int -> Int -> Vector -> Vector
set k a v =
    { v | content = List.Extra.setAt k a v.content }


resetFrom : Int -> Vector -> Vector
resetFrom k v =
    let
        prefix =
            List.take k v.content

        suffix =
            List.repeat (v.size - k) 0
    in
    { size = v.size, content = prefix ++ suffix }


increment : Int -> Vector -> Vector
increment k v =
    if k < 0 || k >= v.size then
        v

    else
        set k (get k v + 1) v
            |> resetFrom (k + 1)


level : Vector -> Int
level v =
    List.filter (\i -> i /= 0) v.content |> List.length
