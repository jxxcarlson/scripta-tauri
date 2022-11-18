module Parser.Forest exposing (Forest, map)

import Tree exposing (Tree)


type alias Forest a =
    List (Tree a)


map =
    List.map << Tree.map
