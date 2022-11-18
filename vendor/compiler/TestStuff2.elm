module TestStuff2 exposing (..)

import Compiler.Differ as D exposing (DiffRecord)
import Compiler.DifferEq as DE
import Markup
import Parser.PrimitiveBlock exposing (PrimitiveBlock)
import Parser.Utility
import Scripta.Language exposing (Language(..))



--- STRINGS
-- diff : (q -> q -> Bool) -> (q -> Int) -> List q -> List q -> DiffRecord q


diffS =
    DE.diff (\a b -> a == b) (\a -> String.length (Parser.Utility.getLeadingBlanks a))


x6 =
    [ "000", "aaa", "  bbb", "  ccc", "ddd" ]


y6 =
    [ "000", "aaa", "  bbb", "  cxc", "ddd" ]


diffB : List PrimitiveBlock -> List PrimitiveBlock -> DiffRecord PrimitiveBlock
diffB =
    DE.diff (\a b -> Parser.PrimitiveBlock.eq a b) (\b -> b.indent)


toPrimitiveBlocks =
    Markup.toPrimitiveBlocks L0Lang


a1 =
    toPrimitiveBlocks """
| title
L0 Test

| item
Bread

| item
Cheese

| item
Wine
"""


b1 =
    toPrimitiveBlocks """
| title
L0 Test

| item
Bread

| item!
Cheese

| numbered
Wine
"""
