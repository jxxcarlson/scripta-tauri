module Document exposing (Document, SourceTextRecord, default, updateContent, language)

import Scripta.Language exposing (Language(..))
import List.Extra

type alias Document = { content : String
                      , name : String
                      , path : String
                      }

type alias SourceTextRecord =
    { position : Int, source : String }

language : Document -> Language
language doc = 
  let
    ext = case List.Extra.unconsLast (String.split "." doc.name) of 
      Nothing -> "unknown"
      Just (ext_, _)-> ext_
  in
  case ext of 
    "L0" -> L0Lang
    "md" -> XMarkdownLang
    "tex" -> MicroLaTeXLang
    _ -> MicroLaTeXLang


updateContent : String -> Document -> Document
updateContent str doc = {doc | content = str}

default = { content = defaultText, name = "default.L0"}

defaultText = """

| title
Default

| banner
[link Scripta.io https://scripta.io]

| contents

| section 1
Images

|| hide
[image https://nas-national-prod.s3.amazonaws.com/styles/hero_image/s3/web_h_apa_2016-a1_2474_8_cedar-waxwing_peter_brannon_kk_female.jpg?itok=VdeVVmGA]

[image https://www.birdsandblooms.com/wp-content/uploads/2018/10/BNBbyc18_patricia-warren.jpg width:400]


| section 1
Math

Pythagoras says: $a^2 + b^2 = c^2$

From calculus:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

[bold Tip:] Click on a section title to go back to the table of contents.

"""