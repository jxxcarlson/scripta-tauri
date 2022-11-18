module Compiler.DifferEq exposing (backwardClosure, diff, diffc, forwardClosure)

import Compiler.Differ exposing (DiffRecord)
import List.Extra


{-| Let u and v be two lists of q's (strings, primitive blocks,
whatever). Write them as u = axb, v = ayb, where a is the greatest
common prefix and b is the greatest common suffix.
Return DiffRecord a b x y
-}
diff : (q -> q -> Bool) -> (q -> Int) -> List q -> List q -> DiffRecord q
diff eq indentation u v =
    let
        a =
            commonInitialSegment eq u v

        b_ =
            commonTerminalSegmentAux eq a u v

        la =
            List.length a

        lb =
            List.length b_

        x =
            u |> List.drop la |> dropLast lb

        y =
            v |> List.drop la |> dropLast lb

        b =
            if la == List.length u then
                []

            else
                b_
    in
    DiffRecord a b x y |> backwardClosure indentation |> forwardClosure indentation


backwardClosure : (q -> Int) -> DiffRecord q -> DiffRecord q
backwardClosure level diffRecord =
    let
        n =
            List.length diffRecord.commonInitialSegment
    in
    case List.head diffRecord.middleSegmentInTarget of
        Nothing ->
            diffRecord

        Just item ->
            if level item > 0 then
                case List.Extra.unconsLast diffRecord.commonInitialSegment of
                    Nothing ->
                        diffRecord

                    Just ( last, remaining ) ->
                        backwardClosure level (retreat last remaining diffRecord)

            else
                diffRecord


retreat : q -> List q -> DiffRecord q -> DiffRecord q
retreat last remaining diffRecord =
    let
        n =
            List.length diffRecord.commonInitialSegment
    in
    { diffRecord
        | commonInitialSegment = remaining
        , middleSegmentInSource = last :: diffRecord.middleSegmentInSource
        , middleSegmentInTarget = last :: diffRecord.middleSegmentInTarget
    }


forwardClosure : (q -> Int) -> DiffRecord q -> DiffRecord q
forwardClosure level diffRecord =
    case List.Extra.uncons diffRecord.commonTerminalSegment of
        Nothing ->
            diffRecord

        Just ( first, remaining ) ->
            if level first == 0 then
                diffRecord

            else
                forwardClosure level (advance first remaining diffRecord)


advance : q -> List q -> DiffRecord q -> DiffRecord q
advance first remaining diffRecord =
    let
        n =
            List.length diffRecord.commonTerminalSegment + List.length diffRecord.middleSegmentInTarget
    in
    { diffRecord
        | commonTerminalSegment = remaining
        , middleSegmentInSource = diffRecord.middleSegmentInSource ++ [ first ]
        , middleSegmentInTarget = diffRecord.middleSegmentInTarget ++ [ first ]
    }


diffc : (q -> q -> Bool) -> (q -> Int) -> List q -> List q -> List Int
diffc eq ind u v =
    let
        r =
            diff eq ind u v
    in
    List.map List.length [ r.commonInitialSegment, r.commonTerminalSegment, r.middleSegmentInSource, r.middleSegmentInTarget ]


commonInitialSegment : (q -> q -> Bool) -> List q -> List q -> List q
commonInitialSegment eq x y =
    if x == [] then
        []

    else if y == [] then
        []

    else
        case ( List.head x, List.head y ) of
            ( Just a, Just b ) ->
                if eq a b then
                    a :: commonInitialSegment eq (List.drop 1 x) (List.drop 1 y)

                else
                    []

            _ ->
                []


commonTerminalSegmentAux : (q -> q -> Bool) -> List q -> List q -> List q -> List q
commonTerminalSegmentAux eq cis x y =
    let
        n =
            List.length cis

        xx =
            List.drop n x |> List.reverse

        yy =
            List.drop n y |> List.reverse
    in
    commonInitialSegment eq xx yy |> List.reverse


dropLast : Int -> List a -> List a
dropLast k x =
    x |> List.reverse |> List.drop k |> List.reverse


takeLast : Int -> List a -> List a
takeLast k x =
    x |> List.reverse |> List.take k |> List.reverse
