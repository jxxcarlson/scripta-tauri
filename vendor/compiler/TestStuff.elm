module TestStuff exposing (..)

import Compiler.DifferEq
import Markup
import Parser.PrimitiveBlock exposing (PrimitiveBlock)
import Scripta.Language exposing (Language(..))


toPrimitiveBlocks =
    Markup.toPrimitiveBlocks L0Lang


diff a b =
    Compiler.DifferEq.diff Parser.PrimitiveBlock.eq (\block -> block.indent + itemLevel block) (toPrimitiveBlocks a) (toPrimitiveBlocks b)


diffc a b =
    Compiler.DifferEq.diffc Parser.PrimitiveBlock.eq (\block -> block.indent + itemLevel block) (toPrimitiveBlocks a) (toPrimitiveBlocks b)


itemLevel : PrimitiveBlock -> Int
itemLevel block =
    if block.name == Just "item" || block.name == Just "numbered" then
        1

    else
        0


y1 =
    """
aaa

bbb

  ccc

  ddd

eee
"""


y2 =
    """
aaa

bbb

  ccc

  dxd

eee
"""


x1 =
    """
| title
L0 Test

| item
Bread

| item
Cheese

| item
Wine
"""


x2 =
    """
| title
L0 Test

| item
Bread

| numbered
Cheese

| item
Wine
"""
