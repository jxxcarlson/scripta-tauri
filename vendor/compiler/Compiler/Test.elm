module Compiler.Test exposing
    ( a2
    , a3
    , a4
    , bll
    , bllt
    , blm
    , blmt
    , blx
    , blxt
    , doc
    , dpl
    , dpm
    , e2
    , initl
    , pl
    , plt
    , pm
    , pmt
    , pxt
    , tol0
    )

import Compiler.Acc
import Compiler.DifferentialParser
import Compiler.Transform
import Dict
import L0.Parser.Classify
import Markup
import MicroLaTeX.Parser.TransformLaTeX
import Parser.Block exposing (ExpressionBlock)
import Parser.Forest exposing (Forest)
import Parser.PrimitiveBlock as PrimitiveBlock exposing (PrimitiveBlock)
import Scripta.API
import Scripta.Language exposing (Language(..))
import Tree exposing (Tree)


doc =
    "| title\n[Test Folder2]\n\n| collection\n\n| document id-0504ef8f-cd80-4276-a335-e1da76d115e9\nAnharmonic Oscillator\n\n| document id-413a3868-08bb-4972-8af1-18b62cd18087\nLaTeX Test 3\n"



-- 1. bll (OK) -- to List PrimitiveBlock
-- 2. bllt (OK) -- to List PrimitiveBlock followed  Compiler.Transform.transform (identity for L0)
-- 3. plt (OK) -- to Forest ExpressionBloc followed  Compiler.Transform.transform (identity for L0)
-- 4. pl (OK) -- to Forest ExpressionBlo
-- 5. dpl (OK) -- Differential parser (Dict is empty)
--
--
-- TEST formation of primitive blocks


tol0 : String -> List String
tol0 str =
    str |> String.lines |> MicroLaTeX.Parser.TransformLaTeX.toL0


bll : String -> List PrimitiveBlock
bll str =
    PrimitiveBlock.parse_ L0Lang L0.Parser.Classify.isVerbatimLine (String.lines str)


blm : String -> List PrimitiveBlock
blm str =
    PrimitiveBlock.parse MicroLaTeXLang Markup.isVerbatimLine (String.lines str)


blx : String -> List PrimitiveBlock
blx str =
    PrimitiveBlock.parse XMarkdownLang Markup.isVerbatimLine (String.lines str)



-- TEST formation of primitive blocks with transform


bllt : String -> List PrimitiveBlock
bllt str =
    bll str |> List.map (Compiler.Transform.transform L0Lang)


blmt : String -> List PrimitiveBlock
blmt str =
    blm str |> List.map (Compiler.Transform.transform MicroLaTeXLang)


blxt : String -> List PrimitiveBlock
blxt str =
    blx str |> List.map (Compiler.Transform.transform XMarkdownLang)



-- FOREST EXPRESSIONBLOCK


plt : String -> Forest ExpressionBlock
plt str =
    Markup.parse L0Lang str |> Compiler.Acc.transformST (initialAccumulatorData L0Lang)


pmt : String -> Forest ExpressionBlock
pmt str =
    Markup.parse MicroLaTeXLang str |> Compiler.Acc.transformST (initialAccumulatorData MicroLaTeXLang)


pxt : String -> Forest ExpressionBlock
pxt str =
    Markup.parse XMarkdownLang str |> Compiler.Acc.transformST (initialAccumulatorData XMarkdownLang)


initialAccumulatorData lang =
    { language = lang
    , mathMacros = ""
    , textMacros = ""
    , vectorSize = 4
    }



-- TEST Parser


pl : String -> Forest ExpressionBlock
pl str =
    Markup.parse L0Lang str


pm : String -> Forest ExpressionBlock
pm str =
    Markup.parse MicroLaTeXLang str



-- TEST differential parser


dpl : String -> List ExpressionBlock
dpl str =
    Compiler.DifferentialParser.init Dict.empty L0Lang str |> .parsed


dpm : String -> List ExpressionBlock
dpm str =
    Compiler.DifferentialParser.init Dict.empty MicroLaTeXLang str |> .parsed



--- TEST API


initl str =
    Scripta.API.init Dict.empty L0Lang str |> .parsed



-- EXAMPLES


e2 =
    """
| theorem

  This is a very good theorem

  $$
  x^2
  $$

  Isn't that nice?

"""


a2 =
    """
| indent
abc

  | indent
  def

    | indent
    ghi

"""


a3 =
    """
| theorem
This is a very good theorem

  $$
  x^2
  $$

  Isn't that nice?

"""


a4 =
    """
\\begin{theorem}
This is a very good theorem

  $$
  x^2
  $$

  Isn't that nice?

\\end{theorem}
"""
