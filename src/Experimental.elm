module Experimental exposing (text)


text =
    """
\\title{MicroLaTeX Glossary}

| contents

|| docinfo
author: James Carlson
date: Auguest 15, 2022
subtitle: List of all macros and environments

| set-key contentsdepth 1


\\tags{jxxcarlson:microlatex-glossary}


\\section{Introduction}

What follows is a complete list of the macros and
environments available in Scripta for MicroLaTeX.
Most of the macros work as in LaTeX.  There are however,
a few exceptions, detailed in section \\ref{differences-from-standard-latex}, and  and additions, detailed in section
\\ref{addtions-to-standard-latex}.

If there are macros or environments that you need but
are missing, please contact me (James Carlson, jxxcarlson at
gmail), or used the "bug" icon in the header (upper right corner)


\\section{Addtions to standard LaTeX}

There are features of Scripta that are accessible in
MicroLaTeX using Scripta syntax.   As an example, the
Figure A below is is placed using

|| code
|| svg Figure A
<SVG CODE>



|| svg Figure A
<svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="286.13466" height="299.53867" viewBox="0 0 286.13466 299.53867">
<!-- Original BoundingBox: -141.8174 -142.55246 144.31726 156.9862 -->
  <defs>
    <g transform="scale(0.00995,0.00995)" id="GLYPHcmr10_65">
      <path style="fill-rule: evenodd;" d="M398 -696C393 -709,391 -716,375 -716C359 -716,356 -710,351 -696L144 -98C126 -47,86 -32,32 -31L32 0C55 -1,98 -3,134 -3C165 -3,217 -1,249 0L249 -31C199 -31,174 -56,174 -82C174 -85,175 -95,176 -97L222 -228L469 -228L522 -75C523 -71,525 -65,525 -61C525 -31,469 -31,442 -31L442 0C478 -3,548 -3,586 -3C629 -3,675 -2,717 0L717 -31L699 -31C639 -31,625 -38,614 -71M345 -584L458 -259L233 -259"></path>
    </g>
    <g transform="scale(0.00995,0.00995)" id="GLYPHcmr10_66">
      <path style="fill-rule: evenodd;" d="M222 -366L222 -612C222 -645,224 -652,271 -652L395 -652C492 -652,527 -567,527 -514C527 -450,478 -366,367 -366M458 -357C555 -376,624 -440,624 -514C624 -601,532 -683,402 -683L36 -683L36 -652L60 -652C137 -652,139 -641,139 -605L139 -78C139 -42,137 -31,60 -31L36 -31L36 0L428 0C561 0,651 -89,651 -183C651 -270,569 -345,458 -357M396 -31L271 -31C224 -31,222 -38,222 -71L222 -344L410 -344C509 -344,551 -251,551 -184C551 -113,499 -31,396 -31"></path>
    </g>
    <g transform="scale(0.00995,0.00995)" id="GLYPHcmr10_77">
      <path style="fill-rule: evenodd;" d="M241 -661C232 -683,229 -683,206 -683L37 -683L37 -652L61 -652C138 -652,140 -641,140 -605L140 -105C140 -78,140 -31,37 -31L37 0C72 -1,121 -3,154 -3C187 -3,236 -1,271 0L271 -31C168 -31,168 -78,168 -105L168 -644L169 -644L410 -22C415 -9,420 0,430 0C441 0,444 -8,448 -19L694 -652L695 -652L695 -78C695 -42,693 -31,616 -31L592 -31L592 0C629 -3,697 -3,736 -3C775 -3,842 -3,879 0L879 -31L855 -31C778 -31,776 -42,776 -78L776 -605C776 -641,778 -652,855 -652L879 -652L879 -683L710 -683C684 -683,684 -682,677 -664L458 -101"></path>
    </g>
    <g transform="scale(0.00995,0.00995)" id="GLYPHcmr10_79">
      <path style="fill-rule: evenodd;" d="M721 -339C721 -543,570 -705,388 -705C209 -705,56 -545,56 -339C56 -134,210 22,388 22C570 22,721 -137,721 -339M389 -4C293 -4,159 -92,159 -353C159 -612,305 -680,388 -680C475 -680,618 -609,618 -353C618 -88,481 -4,389 -4"></path>
    </g>
  </defs>
  <path d="M141.8174 15.25395C177.1848 14.0784,211.1995 27.9649,237.12119 52.0789C282.05598 93.8801,297.15334 159.23988,271.33234 214.555C225.1936 313.39583,91.44423 328.21483,28.26982 241.81015C-39.12485 149.63335,25.80164 19.11014,141.8174 15.25395Z" style="stroke:rgb(0%,0%,0%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <path d="M271.33234 214.555C225.1936 313.39583,91.44423 328.21483,28.26982 241.81015C-39.12485 149.63335,25.80164 19.11014,141.8174 15.25395" style="stroke:rgb(0%,0%,100%); stroke-width: 1.99252;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <path d="M141.8174 15.25395L237.12119 52.0789L271.33234 214.555" style="stroke:rgb(100%,0%,0%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <path d="M223.9006 46.97047C219.84766 57.45949,229.0379 68.26492,240.04143 65.94806" style="stroke:rgb(100%,0%,0%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <g transform="matrix(1.79999,0,0,1.79999,135.09264 12.25395)" style="fill: rgb(0%,0%,0%);">
    <use xlink:href="#GLYPHcmr10_65"></use>
  </g>
  <g transform="matrix(1.79999,0,0,1.79999,273.43233 228.90894)" style="fill: rgb(0%,0%,0%);">
    <use xlink:href="#GLYPHcmr10_66"></use>
  </g>
  <g transform="matrix(1.79999,0,0,1.79999,239.22118 49.97891)" style="fill: rgb(0%,0%,0%);">
    <use xlink:href="#GLYPHcmr10_77"></use>
  </g>
  <path d="M141.8174 15.25395L141.8174 156.9862L271.33234 214.555" style="stroke:rgb(0%,0%,100%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <path d="M141.8174 142.81297C130.1504 142.81297,123.48027 156.12177,130.46277 165.46861" style="stroke:rgb(0%,0%,100%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <path d="M130.46277 165.46861C136.94482 174.14554,150.3695 172.64014,154.76872 162.743" style="stroke:rgb(0%,0%,100%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <g transform="matrix(1.79999,0,0,1.79999,143.91739 154.88622)" style="fill: rgb(0%,0%,0%);">
    <use xlink:href="#GLYPHcmr10_79"></use>
  </g>
</svg>

\\vspace{30}

The text `|| svg ...` is a Scripta \\term{verbatim block}.
The general form is

|| code
HEADER
BODY

where the header has the form `|| BLOCK-NAME ARGS`,
where `ARGS` is a possible empty sequence of words
separated by spaces.  The body of a verbatim block
is any sequence of nonempty lines.  If you have
somethings with empty lines, just indent the entire
text, so that empty lines become lines consisting of
a certain number of spaces.

There is also the notion of a Scripta \\term{ordinary block},
e.g., the below, which places an active index of terms
that have been marked using the `\\term` or `\\term_` macro.
See section \\ref{index} of this document for an example.
Click on an item in the index to view the associated text.

|| code
| index


\\section{Differences from standard LaTeX}

For example, the `\\item` macro is used
as in the exmple below.  Notice that there is no
`\\begin{itemized} ... \\end{itemized}`
\\vspace{15}


  `\\item` \\vspace{}
  `Fertilizer for the garden`
   \\vspace{20}

  `\\item` \\vspace{}
  `Four 20 lb. bags of compost`
   \\vspace{20}

  `\\item` \\vspace{}
  `Shovel`


\\section{Frontmatter}

Frontmatter for a document is defined by
two things:

\\item
The \\term{title} macro, which is mandatory
and should appear at the top of your document.

\\item
The \\term{docinfo} block as in the example below.

|| code
|| docinfo
author: James Carlson
date: August 15, 2022
subtitle: Complete list of macros and environments

\\subsection{subtitle}

\\subsection{today}



\\section{Document}

\\strong{Topics} \\vspace{}
\\ref{contents} contents, set-key contents-depth \\vspace{}
\\ref{link-href} link, href \\vspace{}
\\ref{term-index} term, term_, index \\vspace{}
\\ref{ref} ref \\vspace{}
\\ref{abstract} abstract \\vspace{}
\\ref{footnote-endnotes} footnote, endnotes \\vspace{}
\\ref{abstract} abstract \\vspace{}
\\ref{bibitem-cite} bibitem, cite \\vspace{}

\\subsection{Contents}




Use the Scripta block \\term_{contents}

|| code
| axiom

to place a table of contents.  The depth of the table
of contents can be limited via an (optional) block like
\\term_{contents-depth}

|| code
set-key contents-depth 1

In this case only sections will appear in the table of
contents.  If you say

|| code
set-key contents-depth 2

the table of contents will display both sections
and subsections.


\\subsection{link, href}

The text `\\link{New York Times https://nytimes.com}` will
render as the link \\link{New York Times https://nytimes.com}.
\\term_{link}

The `text \\href{https://nytimes.com}{New York Times}` also renders as the active link \\href{https://nytimes.com}{New York Times}.
\\term_{href}


\\subsection{term, index}

Use \\bs{term}\\texarg{derivative} to mark the word "derivatve" for
inclusion in the index.  It will be rendered in italic in the
text.  Use \\bs{term_} to mark the argument of \\bs{term_} for
inclusion in the index without rendering it in the text.
To display the index, use this:

|| code
| index

\\term_{index} \\term_{term_} \\term{term}

\\subsection{ref}

`For information about labeling equations,
see section \\ref{label}.` $\\to$ For information about labeling equations,
see section \\ref{label}.
\\term_{ref}

The text `4.1.1` is an active link.  The argument of
\\bs{ref} is the normalized section title.  For example,
if the section title is `Label Stuff`, the normalized
title will be `label-stuff`.

\\subsection{abstract}\\term_{abstract}

|| code
\\abstract
We show that $x = y$ in all contexts.


renders as


\\abstract
We show that $x = y$ in all contexts.

\\term_{abstract}


\\subsection{footnote, endnotes}

The text

|| code
The the lawyer
asserted\\footnote{See Jason York, Memoirs, 1988}
\\term_{footnote}

renders as
\\vspace{10}

 The the lawyer asserted\\footnote{See Jason York, Memoirs, 1988}

The \\bs{footnote} macro will act as usual when your document
is exported to standard LaTeX.  In Scripta, use `| endnotes`
to display the footnotes.\\term_{endnotes}


\\subsection{bibitem, cite}

To make bibliographical entries, use this model:

|| code
\\bibitem VX
Victor Xaha, Introduction to Magic


\\bibitem VX
Victor Xaha, Introduction to Magic
\\term_{bibitem}

Here \\strong{VX} is the label of the bibliographical
entry.  Use the label to refer to that entry as in the
model below:

|| code
He said that \\cite{VX} is
the place to read about it.

which renders as

He said that \\cite{VX} is
the place to read about it.


\\subsection{Setcounter}

Use `setcounter` to set the section number.  This is
useful when a document is part of a multi-section document
such as \\link{these notes jxxcarlson:quantum-mechanics-notes}.
Thus, if we want the section number to be 4, we say `\\setcounter{4}`. \\term_{setcounter}

\\section{Formatting}

\\subsection{vspace}


Use `\\vspace` to insert vertical space, e.g. `\\vspace{10}`
for 10 pixels of vertical space. \\term_{vspace}













\\section{Font}

\\strong{Font} \\vspace{}
\\ref{large} large \\vspace{}
\\ref{strong-bold-b} strong, bold, b \\vspace{}
\\ref{italic-emph} italic, emph \\vspace{}
\\ref{bolditalic-bi} bolditalic, bi \\vspace{}
\\ref{stike} strike \\vspace{}
\\ref{underline} underline \\vspace{}

\\subsection{large}

`This is \\large{large text}` $\\to$ This is \\large{large text}.
 \\term_{large}

\\subsection{strong, bold, b}

`This is \\strong{strong stuff}.` $\\to$
``This is \\strong{strong stuff}.

Aliases: \\term{bold}, \\term{b} \\term_{strong}

\\subsection{italic, emph}

`This is \\italic{italic text}.` $\\to$ This is \\italic{italic text}.  \\term_{italic}

Aliases: \\term{i}, \\term{emph}


\\subsection{bolditalic, bi}

`This is \\bolditalic{bold italic text}.` $\\to$ This is \\bolditalic{bold italic text}.  \\term_{bolditalic}

Aliases: \\term{bi}


\\subsection{strike}

`This is \\strike{stricken from the record}.` $\\to$ This is \\strike{stricken from the record}.
\\term_{strike}

\\subsection{underline}

`This text is \\underline{underlined}.` $\\to$ This text is \\underline{underlined}. \\term_{underline}





\\section{Color}

There are a number of macros for colored text, e.g.,


`This text is \\red{red}` $\\to$ This text is \\red{red}.
\\term_{red}

They all work in the same way. The others are
\\term{blue}, \\term{pink}, \\term{magenta}, \\term{violet}, and
\\term{gray}.

There is also the \\term{highlight} macro:

`This text is \\highlight{highlighted}.` $\\to$ This text is \\highlight{highlighted}.






\\section{Special characters}

\\subsection{brackets}

`\\brackets{1,2,3}` $\\to$ \\brackets{1,2,3}  \\term_{brackets}

\\subsection{rb}

`\\rb` $\\to$ \\rb  \\term_{rb}

\\subsection{lb}

`\\lb` $\\to$ \\lb  \\term_{lb}

\\subsection{bt}

`\\bt abc\\bt` $\\to$ \\bt abc \\bt  \\term_{bt}

\\subsection{bs, texarg}

`\\bs{foo}` $\\to$ \\bs{foo}  \\term_{bs}

`\\bs{foo}\\texarg{bar}` $\\to$ \\bs{foo}\\texarg{bar}
 \\term_{texarg}






\\section{Lists}

Below, we describe bulleted, numbered and definition lists.

\\subsection{Item}

\\item
Groceries

\\term_{item, bullets}

\\item
Parent-teacher conference

\\subsection{Numbered item}

This is a test.

\\numbered
Mortgage payment

\\term_{item, numbered}

\\numbered
File taxes


\\subsection{Desc}

The `\\desc` macro is used for description or definition lists:
\\term_{item, desc}.

|| code
\\desc Sets
A set is a collection of things.

renders as

  \\desc Sets
  A set is a collection of things.


\\section{Environments: general}

\\subsection{Quotation}

The \\term{quotation} environemnt:

|| code
\\begin{quotation} W.F Snodgrass
He who asks for little shall receive much.
\\end{quotation}

\\begin{quotation} W.F Snodgrass
He who asks for little shall receive much.
\\end{quotation}


\\subsection{Indent}

The \\term{indent} environment:

|| code
\\begin{indent}
He who wishes little receives much.
\\end{indent}

\\begin{indent}
He who wishes little receives much.
\\end{indent}

\\section{Environments: mathematics}

\\subsection{Equation}

The \\term{equation} environment is automatically numbered

|| code
\\begin{equation}
\\int_0^1 x^n dx = \\frac{1}{n+1}
\\end{equation}

renders as

\\begin{equation}
\\int_0^1 x^n dx = \\frac{1}{n+1}
\\end{equation}


\\subsubsection{label}

Equations can be labeled: \\term_{label}

|| code
\\begin{equation}
\\label{newton}
\\int_0^1 x^n dx = \\frac{1}{n+1}
\\end{equation}

\\begin{equation}
\\label{newton}
\\int_0^1 x^n dx = \\frac{1}{n+1}
\\end{equation}


The label is used for cross-referencing by the \\term{cite}
macro:

`The equation \\eqref{newton} will be on the test.` $\\to$
The equation \\eqref{newton} will be on the test.  \\term_{label}

\\subsection{Aligned}

\\begin{aligned}
x &= a + b \\\\
y &= a - b \\\\
z &= xy = a^2 - b^2
\\end{aligned}

\\term_{aligned}


\\subsection{Mathmacros}

Use a `mathmacro` block to define math-mode macros:
\\term_{mathmacro}.

|| code
\\begin{mathmacros}
\\newcommand{\\cA}{\\mathcal{A}}
\\end{mathmacros}

\\begin{mathmacros}
\\newcommand{\\cA}{\\mathcal{A}}
\\end{mathmacros}

Then if we say `$\\cA$`, we obtain $\\cA$. See
\\ilink{this document jxxcarlson:manual-tex-macros-2}
for more information.



\\subsection{Textmacros}


Text-mode macros are defined using a
Scripta `textmacros` block: \\term_{texmacros}

|| textmacros
\\newcommand{\\hello}{\\blue{\\bold{Hello #1}}!}


|| code
|| texmacros
\\newcommand{\\hello}{\\blue{\\bold{Hello #1}}!}

Then, if you say `\\hello{John}`, you will get

| indent
\\hello{John}

Notice that we \\bold{do not} say


|| code
\\newcommand{\\hello}[1]{\\blue{\\bold{Hello #1}}!}

MicroLaTeX will figure out by itself how many arguments
the macro takes.  When a MicroLaTeX document is exported
to standard LaTeX, the exported macro definition will
contain the `[1]`.

\\strong{Note.}
Math-mode macros and Text-mode macros must be defined
in separate blocks — `mathmacros` and `textmacros`, respectively.



\\term_{textmacros}



\\section{Theorem-like environments}

The theorem-like environments all function alike. Here is
the theorem environment: \\term_{theorem}


|| code
\\begin{theorem} (Euclid)
There are infinitely primes.
\\end{theorem}


It renders like this:

\\begin{theorem} (Euclid)
There are infinitely primes.
\\end{theorem}

The other environtments are:

| indent
\\term{axiom}, \\term{proposition}, \\term{corollary},
\\term{definition}, \\term{lemma}, \\term{problem},
\\term{example}, \\term{note}, \\term{remark}




\\section{Data}


\\subsection{datatable}

The Scripta \\term{datable} block gives an easy way of displaying
raw data.  The text below

|| code
|| datatable
a b c
1 2 3
10 17 46


renders as

|| datatable
a b c
1 2 3
10 17 46

\\section{Graphics}

\\subsection{image}

Uwe the \\term{image} macro to display online images.
The text

|| code
\\image{https://i.ibb.co/m5P5jy2/tree-frog.jpg}

renders as

\\image{https://i.ibb.co/m5P5jy2/tree-frog.jpg}

The image macro accepts various options:
\\term_{caption (image)} \\term_{width (image)}

|| code
\\image{https://.. width:300 caption: Tree frog}


\\image{https://i.ibb.co/m5P5jy2/tree-frog.jpg width:300 caption: Tree frog}

The caption options should come last.

\\subsection{Chart}

Use the Scripta `chart` \\term_{chart} block to visualize csv data, as
in the example below.  For more information see the
\\ilink{Image Gallery jxxcarlson:jxxcarlson:gallery}.

|| code
|| chart timeseries reverse columns:1 lowest:3700
label:S&P  Index, 06/14/2021 to 06/10/2022
Date,Close/Last,Volume,Open,High,Low
06/10/2022,3900.86,--,3974.39,3974.39,3900.16
06/09/2022,4017.82,--,4101.65,4119.1,4017.17
...

|| chart timeseries reverse columns:1 lowest:3700 label:S&P  Index, 06/14/2021 to 06/10/2022
Date,Close/Last,Volume,Open,High,Low
06/10/2022,3900.86,--,3974.39,3974.39,3900.16
06/09/2022,4017.82,--,4101.65,4119.1,4017.17
06/08/2022,4115.77,--,4147.12,4160.14,4107.2
06/07/2022,4160.68,--,4096.47,4164.86,4080.19
06/06/2022,4121.43,--,4134.72,4168.78,4109.18
06/03/2022,4108.54,--,4137.57,4142.67,4098.67
06/02/2022,4176.82,--,4095.41,4177.51,4074.37
06/01/2022,4101.23,--,4149.78,4166.54,4073.85
05/31/2022,4132.15,--,4151.09,4168.34,4104.88
05/27/2022,4158.24,--,4077.43,4158.49,4077.43
05/26/2022,4057.84,--,3984.6,4075.14,3984.6
05/25/2022,3978.73,--,3929.59,3999.33,3925.03
05/24/2022,3941.48,--,3942.94,3955.68,3875.13
05/23/2022,3973.75,--,3927.02,3981.88,3909.04
05/20/2022,3901.36,--,3927.76,3943.42,3810.32
05/19/2022,3900.79,--,3899,3945.96,3876.58
05/18/2022,3923.68,--,4051.98,4051.98,3911.91
05/17/2022,4088.85,--,4052,4090.72,4033.93
05/16/2022,4008.01,--,4013.02,4046.46,3983.99
05/13/2022,4023.89,--,3963.9,4038.88,3963.9
05/12/2022,3930.08,--,3903.95,3964.8,3858.87
05/11/2022,3935.18,--,3990.08,4049.09,3928.82
05/10/2022,4001.05,--,4035.18,4068.82,3958.17
05/09/2022,3991.24,--,4081.27,4081.27,3975.48
05/06/2022,4123.34,--,4128.17,4157.69,4067.91
05/05/2022,4146.87,--,4270.43,4270.43,4106.01
05/04/2022,4300.17,--,4181.18,4307.66,4148.91
05/03/2022,4175.48,--,4159.78,4200.1,4147.08
05/02/2022,4155.38,--,4130.61,4169.81,4062.51
04/29/2022,4131.93,--,4253.75,4269.68,4124.28
04/28/2022,4287.5,--,4222.58,4308.45,4188.63
04/27/2022,4183.96,--,4186.52,4240.71,4162.9
04/26/2022,4175.2,--,4278.14,4278.14,4175.04
04/25/2022,4296.12,--,4255.34,4299.02,4200.82
04/22/2022,4271.78,--,4385.83,4385.83,4267.62
04/21/2022,4393.66,--,4489.17,4512.94,4384.47
04/20/2022,4459.45,--,4472.26,4488.29,4448.76
04/19/2022,4462.21,--,4390.63,4471.03,4390.63
04/18/2022,4391.69,--,4385.63,4410.31,4370.3
04/14/2022,4392.59,--,4449.12,4460.46,4390.77
04/13/2022,4446.59,--,4394.3,4453.92,4392.7
04/12/2022,4397.45,--,4437.59,4471,4381.34
04/11/2022,4412.53,--,4462.64,4464.35,4408.38
04/08/2022,4488.28,--,4494.15,4520.41,4474.6
04/07/2022,4500.21,--,4474.65,4521.16,4450.3
04/06/2022,4481.15,--,4494.17,4503.94,4450.04
04/05/2022,4525.12,--,4572.45,4593.45,4514.17
04/04/2022,4582.64,--,4547.97,4583.5,4539.21
04/01/2022,4545.86,--,4540.32,4548.7,4507.57
03/31/2022,4530.41,--,4599.02,4603.07,4530.41
03/30/2022,4602.45,--,4624.2,4627.77,4581.32
03/29/2022,4631.6,--,4602.86,4637.3,4589.66
03/28/2022,4575.52,--,4541.09,4575.65,4517.69
03/25/2022,4543.06,--,4522.91,4546.03,4501.07
03/24/2022,4520.16,--,4469.98,4520.58,4465.17
03/23/2022,4456.24,--,4493.1,4501.07,4455.81
03/22/2022,4511.61,--,4469.1,4522,4469.1
03/21/2022,4461.18,--,4462.4,4481.75,4424.3
03/18/2022,4463.12,--,4407.34,4465.4,4390.57
03/17/2022,4411.67,--,4345.11,4412.67,4335.65
03/16/2022,4357.86,--,4288.14,4358.9,4251.99
03/15/2022,4262.45,--,4188.82,4271.05,4187.9
03/14/2022,4173.11,--,4202.75,4247.57,4161.72
03/11/2022,4204.31,--,4279.5,4291.01,4200.49
03/10/2022,4259.52,--,4252.55,4268.28,4209.8
03/09/2022,4277.88,--,4223.1,4299.4,4223.1
03/08/2022,4170.7,--,4202.66,4276.94,4157.87
03/07/2022,4201.09,--,4327.01,4327.01,4199.85
03/04/2022,4328.87,--,4342.12,4342.12,4284.98
03/03/2022,4363.49,--,4401.31,4416.78,4345.56
03/02/2022,4386.54,--,4322.56,4401.48,4322.56
03/01/2022,4306.26,--,4363.14,4378.45,4279.54
02/28/2022,4373.94,--,4354.17,4388.84,4315.12
02/25/2022,4384.65,--,4298.38,4385.34,4286.83
02/24/2022,4288.7,--,4155.77,4294.73,4114.65
02/23/2022,4225.5,--,4324.93,4341.51,4221.51
02/22/2022,4304.76,--,4332.74,4362.12,4267.11
02/18/2022,4348.87,--,4384.57,4394.6,4327.22
02/17/2022,4380.26,--,4456.06,4456.06,4373.81
02/16/2022,4475.01,--,4455.75,4489.55,4429.68
02/15/2022,4471.07,--,4429.28,4472.77,4429.28
02/14/2022,4401.67,--,4412.61,4426.22,4364.84
02/11/2022,4418.64,--,4506.27,4526.33,4401.41
02/10/2022,4504.08,--,4553.24,4588.92,4484.31
02/09/2022,4587.18,--,4547,4590.03,4547
02/08/2022,4521.54,--,4480.02,4531.32,4465.4
02/07/2022,4483.87,--,4505.75,4521.86,4471.47
02/04/2022,4500.53,--,4482.79,4539.66,4451.5
02/03/2022,4477.44,--,4535.41,4542.88,4470.39
02/02/2022,4589.38,--,4566.39,4595.31,4544.32
02/01/2022,4546.54,--,4519.57,4550.49,4483.53
01/31/2022,4515.55,--,4431.79,4516.89,4414.02
01/28/2022,4431.85,--,4336.19,4432.72,4292.46
01/27/2022,4326.51,--,4380.58,4428.74,4309.5
01/26/2022,4349.93,--,4408.43,4453.23,4304.8
01/25/2022,4356.45,--,4366.64,4411.01,4287.11
01/24/2022,4410.13,--,4356.32,4417.35,4222.62
01/21/2022,4397.94,--,4471.38,4494.52,4395.34
01/20/2022,4482.73,--,4547.35,4602.11,4477.95
01/19/2022,4532.76,--,4588.03,4611.55,4530.2
01/18/2022,4577.11,--,4632.24,4632.24,4568.7
01/14/2022,4662.85,--,4637.99,4665.13,4614.75
01/13/2022,4659.03,--,4733.56,4744.13,4650.29
01/12/2022,4726.35,--,4728.59,4748.83,4706.71
01/11/2022,4713.07,--,4669.14,4714.13,4638.27
01/10/2022,4670.29,--,4655.34,4673.02,4582.24
01/07/2022,4677.03,--,4697.66,4707.95,4662.74
01/06/2022,4696.05,--,4693.39,4725.01,4671.26
01/05/2022,4700.58,--,4787.99,4797.7,4699.44
01/04/2022,4793.54,--,4804.51,4818.62,4774.27
01/03/2022,4796.56,--,4778.14,4796.64,4758.17
12/31/2021,4766.18,--,4775.21,4786.83,4765.75
12/30/2021,4778.73,--,4794.23,4808.93,4775.33
12/29/2021,4793.06,--,4788.64,4804.06,4778.08
12/28/2021,4786.35,--,4795.49,4807.02,4780.04
12/27/2021,4791.19,--,4733.99,4791.49,4733.99
12/23/2021,4725.79,--,4703.96,4740.74,4703.96
12/22/2021,4696.56,--,4650.36,4697.67,4645.53
12/21/2021,4649.23,--,4594.96,4651.14,4583.16
12/20/2021,4568.02,--,4587.9,4587.9,4531.1
12/17/2021,4620.64,--,4652.5,4666.7,4600.22
12/16/2021,4668.67,--,4719.13,4731.99,4651.89
12/15/2021,4709.85,--,4636.46,4712.6,4611.22
12/14/2021,4634.09,--,4642.99,4660.47,4606.52
12/13/2021,4668.97,--,4710.3,4710.3,4667.6
12/10/2021,4712.02,--,4687.64,4713.57,4670.24
12/09/2021,4667.45,--,4691,4695.26,4665.98
12/08/2021,4701.21,--,4690.86,4705.06,4674.52
12/07/2021,4686.75,--,4631.97,4694.04,4631.97
12/06/2021,4591.67,--,4548.37,4612.6,4540.51
12/03/2021,4538.43,--,4589.49,4608.03,4495.12
12/02/2021,4577.1,--,4504.73,4595.46,4504.73
12/01/2021,4513.04,--,4602.82,4652.94,4510.27
11/30/2021,4567,--,4640.25,4646.02,4560
11/29/2021,4655.27,--,4628.75,4672.95,4625.26
11/26/2021,4594.62,--,4664.63,4664.63,4585.43
11/24/2021,4701.46,--,4675.78,4702.87,4659.89
11/23/2021,4690.7,--,4678.48,4699.39,4652.66
11/22/2021,4682.94,--,4712,4743.83,4682.17
11/19/2021,4697.96,--,4708.44,4717.75,4694.22
11/18/2021,4704.54,--,4700.72,4708.8,4672.78
11/17/2021,4688.67,--,4701.5,4701.5,4684.41
11/16/2021,4700.9,--,4679.42,4714.95,4679.42
11/15/2021,4682.8,--,4689.3,4697.42,4672.86
11/12/2021,4682.85,--,4655.24,4688.47,4650.77
11/11/2021,4649.27,--,4659.39,4664.55,4648.31
11/10/2021,4646.71,--,4670.26,4684.85,4630.86
11/09/2021,4685.25,--,4707.25,4708.53,4670.87
11/08/2021,4701.7,--,4701.48,4714.92,4694.39
11/05/2021,4697.53,--,4699.26,4718.5,4681.32
11/04/2021,4680.06,--,4662.93,4683,4662.59
11/03/2021,4660.57,--,4630.65,4663.46,4621.19
11/02/2021,4630.65,--,4613.34,4635.15,4613.34
11/01/2021,4613.67,--,4610.62,4620.34,4595.06
10/29/2021,4605.38,--,4572.87,4608.08,4567.59
10/28/2021,4596.42,--,4562.84,4597.55,4562.84
10/27/2021,4551.68,--,4580.22,4584.57,4551.66
10/26/2021,4574.79,--,4578.69,4598.53,4569.17
10/25/2021,4566.48,--,4553.69,4572.62,4537.36
10/22/2021,4544.9,--,4546.12,4559.67,4524
10/21/2021,4549.78,--,4532.24,4551.44,4526.89
10/20/2021,4536.19,--,4524.42,4540.87,4524.4
10/19/2021,4519.63,--,4497.34,4520.4,4496.41
10/18/2021,4486.46,--,4463.72,4488.75,4447.47
10/15/2021,4471.37,--,4447.69,4475.82,4447.69
10/14/2021,4438.26,--,4386.75,4439.73,4386.75
10/13/2021,4363.8,--,4358.01,4372.87,4329.92
10/12/2021,4350.65,--,4368.31,4374.89,4342.09
10/11/2021,4361.19,--,4385.44,4415.88,4360.59
10/08/2021,4391.34,--,4406.51,4412.02,4386.22
10/07/2021,4399.76,--,4383.73,4429.97,4383.73
10/06/2021,4363.55,--,4319.57,4365.57,4290.49
10/05/2021,4345.72,--,4309.87,4369.23,4309.87
10/04/2021,4300.46,--,4348.84,4355.51,4278.94
10/01/2021,4357.04,--,4317.16,4375.19,4288.52
09/30/2021,4307.54,--,4370.67,4382.55,4306.24
09/29/2021,4359.46,--,4362.41,4385.57,4355.08
09/28/2021,4352.63,--,4419.54,4419.54,4346.33
09/27/2021,4443.11,--,4442.12,4457.3,4436.19
09/24/2021,4455.48,--,4438.04,4463.12,4430.27
09/23/2021,4448.98,--,4406.75,4465.4,4406.75
09/22/2021,4395.64,--,4367.43,4416.75,4367.43
09/21/2021,4354.19,--,4374.45,4394.87,4347.96
09/20/2021,4357.73,--,4402.95,4402.95,4305.91
09/17/2021,4432.99,--,4469.74,4471.52,4427.76
09/16/2021,4473.75,--,4477.09,4485.87,4443.8
09/15/2021,4480.7,--,4447.49,4486.87,4438.37
09/14/2021,4443.05,--,4479.33,4485.68,4435.46
09/13/2021,4468.73,--,4474.81,4492.99,4445.7
09/10/2021,4458.58,--,4506.92,4520.47,4457.66
09/09/2021,4493.28,--,4513.02,4529.9,4492.07
09/08/2021,4514.07,--,4518.09,4521.79,4493.95
09/07/2021,4520.03,--,4535.38,4535.38,4513
09/03/2021,4535.43,--,4532.42,4541.45,4521.3
09/02/2021,4536.95,--,4534.48,4545.85,4524.66
09/01/2021,4524.09,--,4528.8,4537.11,4522.02
08/31/2021,4522.68,--,4529.75,4531.39,4515.8
08/30/2021,4528.79,--,4513.76,4537.36,4513.76
08/27/2021,4509.37,--,4474.1,4513.33,4474.1
08/26/2021,4470,--,4493.75,4495.9,4468.99
08/25/2021,4496.19,--,4490.45,4501.71,4485.66
08/24/2021,4486.23,--,4484.4,4492.81,4482.28
08/23/2021,4479.53,--,4450.29,4489.88,4450.29
08/20/2021,4441.67,--,4410.56,4444.35,4406.8
08/19/2021,4405.8,--,4382.44,4418.61,4367.73
08/18/2021,4400.27,--,4440.94,4454.32,4397.59
08/17/2021,4448.08,--,4462.12,4462.12,4417.83
08/16/2021,4479.71,--,4461.65,4480.26,4437.66
08/13/2021,4468,--,4464.84,4468.37,4460.82
08/12/2021,4460.83,--,4446.08,4461.77,4435.96
08/11/2021,4447.7,--,4442.18,4449.44,4436.42
08/10/2021,4436.75,--,4435.79,4445.21,4430.03
08/09/2021,4432.35,--,4437.77,4439.39,4424.74
08/06/2021,4436.52,--,4429.07,4440.82,4429.07
08/05/2021,4429.1,--,4408.86,4429.76,4408.86
08/04/2021,4402.66,--,4415.95,4416.17,4400.23
08/03/2021,4423.15,--,4392.74,4423.79,4373
08/02/2021,4387.16,--,4406.86,4422.18,4384.81
07/30/2021,4395.26,--,4395.12,4412.25,4389.65
07/29/2021,4419.15,--,4403.59,4429.97,4403.59
07/28/2021,4400.64,--,4402.95,4415.47,4387.01
07/27/2021,4401.46,--,4416.38,4416.38,4372.51
07/26/2021,4422.3,--,4409.58,4422.73,4405.45
07/23/2021,4411.79,--,4381.2,4415.18,4381.2
07/22/2021,4367.48,--,4361.27,4369.87,4350.06
07/21/2021,4358.69,--,4331.13,4359.7,4331.13
07/20/2021,4323.06,--,4265.11,4336.84,4262.05
07/19/2021,4258.49,--,4296.4,4296.4,4233.13
07/16/2021,4327.16,--,4367.43,4375.09,4322.53
07/15/2021,4360.03,--,4369.02,4369.02,4340.7
07/14/2021,4374.3,--,4380.11,4393.68,4362.36
07/13/2021,4369.21,--,4381.07,4392.37,4366.92
07/12/2021,4384.63,--,4372.41,4386.68,4364.03
07/09/2021,4369.55,--,4329.38,4371.6,4329.38
07/08/2021,4320.82,--,4321.07,4330.88,4289.37
07/07/2021,4358.13,--,4351.01,4361.88,4329.79
07/06/2021,4343.54,--,4356.46,4356.46,4314.37
07/02/2021,4352.34,--,4326.6,4355.43,4326.6
07/01/2021,4319.94,--,4300.73,4320.66,4300.73
06/30/2021,4297.5,--,4290.65,4302.43,4287.96
06/29/2021,4291.8,--,4293.21,4300.52,4287.04
06/28/2021,4290.61,--,4284.9,4292.14,4274.67
06/25/2021,4280.7,--,4274.45,4286.12,4271.16
06/24/2021,4266.49,--,4256.97,4271.28,4256.97
06/23/2021,4241.84,--,4249.27,4256.6,4241.43
06/22/2021,4246.44,--,4224.61,4255.84,4217.27
06/21/2021,4224.79,--,4173.4,4226.24,4173.4
06/18/2021,4166.45,--,4204.78,4204.78,4164.4
06/17/2021,4221.86,--,4220.37,4232.29,4196.05
06/16/2021,4223.7,--,4248.87,4251.89,4202.45
06/15/2021,4246.59,--,4255.28,4257.16,4238.35
06/14/2021,4255.15,--,4248.31,4255.59,4234.07

\\subsection{Svg}

Use  the Scripta verbatim block to render svg graphics: \\term_{svg}

|| code
|| svg
<svg ... >
SVG CODE
</svg>

As an example, we have this:


|| svg
<svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="286.13466" height="299.53867" viewBox="0 0 286.13466 299.53867">
<!-- Original BoundingBox: -141.8174 -142.55246 144.31726 156.9862 -->
  <defs>
    <g transform="scale(0.00995,0.00995)" id="GLYPHcmr10_65">
      <path style="fill-rule: evenodd;" d="M398 -696C393 -709,391 -716,375 -716C359 -716,356 -710,351 -696L144 -98C126 -47,86 -32,32 -31L32 0C55 -1,98 -3,134 -3C165 -3,217 -1,249 0L249 -31C199 -31,174 -56,174 -82C174 -85,175 -95,176 -97L222 -228L469 -228L522 -75C523 -71,525 -65,525 -61C525 -31,469 -31,442 -31L442 0C478 -3,548 -3,586 -3C629 -3,675 -2,717 0L717 -31L699 -31C639 -31,625 -38,614 -71M345 -584L458 -259L233 -259"></path>
    </g>
    <g transform="scale(0.00995,0.00995)" id="GLYPHcmr10_66">
      <path style="fill-rule: evenodd;" d="M222 -366L222 -612C222 -645,224 -652,271 -652L395 -652C492 -652,527 -567,527 -514C527 -450,478 -366,367 -366M458 -357C555 -376,624 -440,624 -514C624 -601,532 -683,402 -683L36 -683L36 -652L60 -652C137 -652,139 -641,139 -605L139 -78C139 -42,137 -31,60 -31L36 -31L36 0L428 0C561 0,651 -89,651 -183C651 -270,569 -345,458 -357M396 -31L271 -31C224 -31,222 -38,222 -71L222 -344L410 -344C509 -344,551 -251,551 -184C551 -113,499 -31,396 -31"></path>
    </g>
    <g transform="scale(0.00995,0.00995)" id="GLYPHcmr10_77">
      <path style="fill-rule: evenodd;" d="M241 -661C232 -683,229 -683,206 -683L37 -683L37 -652L61 -652C138 -652,140 -641,140 -605L140 -105C140 -78,140 -31,37 -31L37 0C72 -1,121 -3,154 -3C187 -3,236 -1,271 0L271 -31C168 -31,168 -78,168 -105L168 -644L169 -644L410 -22C415 -9,420 0,430 0C441 0,444 -8,448 -19L694 -652L695 -652L695 -78C695 -42,693 -31,616 -31L592 -31L592 0C629 -3,697 -3,736 -3C775 -3,842 -3,879 0L879 -31L855 -31C778 -31,776 -42,776 -78L776 -605C776 -641,778 -652,855 -652L879 -652L879 -683L710 -683C684 -683,684 -682,677 -664L458 -101"></path>
    </g>
    <g transform="scale(0.00995,0.00995)" id="GLYPHcmr10_79">
      <path style="fill-rule: evenodd;" d="M721 -339C721 -543,570 -705,388 -705C209 -705,56 -545,56 -339C56 -134,210 22,388 22C570 22,721 -137,721 -339M389 -4C293 -4,159 -92,159 -353C159 -612,305 -680,388 -680C475 -680,618 -609,618 -353C618 -88,481 -4,389 -4"></path>
    </g>
  </defs>
  <path d="M141.8174 15.25395C177.1848 14.0784,211.1995 27.9649,237.12119 52.0789C282.05598 93.8801,297.15334 159.23988,271.33234 214.555C225.1936 313.39583,91.44423 328.21483,28.26982 241.81015C-39.12485 149.63335,25.80164 19.11014,141.8174 15.25395Z" style="stroke:rgb(0%,0%,0%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <path d="M271.33234 214.555C225.1936 313.39583,91.44423 328.21483,28.26982 241.81015C-39.12485 149.63335,25.80164 19.11014,141.8174 15.25395" style="stroke:rgb(0%,0%,100%); stroke-width: 1.99252;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <path d="M141.8174 15.25395L237.12119 52.0789L271.33234 214.555" style="stroke:rgb(100%,0%,0%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <path d="M223.9006 46.97047C219.84766 57.45949,229.0379 68.26492,240.04143 65.94806" style="stroke:rgb(100%,0%,0%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <g transform="matrix(1.79999,0,0,1.79999,135.09264 12.25395)" style="fill: rgb(0%,0%,0%);">
    <use xlink:href="#GLYPHcmr10_65"></use>
  </g>
  <g transform="matrix(1.79999,0,0,1.79999,273.43233 228.90894)" style="fill: rgb(0%,0%,0%);">
    <use xlink:href="#GLYPHcmr10_66"></use>
  </g>
  <g transform="matrix(1.79999,0,0,1.79999,239.22118 49.97891)" style="fill: rgb(0%,0%,0%);">
    <use xlink:href="#GLYPHcmr10_77"></use>
  </g>
  <path d="M141.8174 15.25395L141.8174 156.9862L271.33234 214.555" style="stroke:rgb(0%,0%,100%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <path d="M141.8174 142.81297C130.1504 142.81297,123.48027 156.12177,130.46277 165.46861" style="stroke:rgb(0%,0%,100%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <path d="M130.46277 165.46861C136.94482 174.14554,150.3695 172.64014,154.76872 162.743" style="stroke:rgb(0%,0%,100%); stroke-width: 0.99626;stroke-linecap: round;stroke-linejoin: round;stroke-miterlimit: 10;fill: none;"></path>
  <g transform="matrix(1.79999,0,0,1.79999,143.91739 154.88622)" style="fill: rgb(0%,0%,0%);">
    <use xlink:href="#GLYPHcmr10_79"></use>
  </g>
</svg>



\\ilink{more info ... jxxcarlson:manual-svg}

\\subsection{Commutative diagrams: q.uiver.app}

There is an excellent online editor, \\link{q.uiver.app https://q.uiver.app}, for constructing  commutative
diagrams. \\term_{quiver} \\term_{q.uiver.app}

You can insert q.uiver.app images and LaTeX code
in Scripta like this:

|| code
|| quiver
IMAGE URL
---
QUIVER LATEX CODE


The image will be useed to render the diagram in Scripta.
If the document is exported to LaTeX, the q.uiver.app
LaTeX code will be used.

|| quiver
https://d.img.vision/scripta/3471908172-image.png Figure 1
---
% https://q.uiver.app/?q=WzAsNixbMiwzLCJBIl0sWzQsMywiQiJdLFszLDIsIlUiXSxbMywwLCJYIl0sWzAsMywiUyJdLFs2LDMsIlQiXSxbMiwwLCJwIiwxXSxbMiwxLCJxIiwxXSxbMywwLCJmIiwxLHsiY3VydmUiOjJ9XSxbMywxLCJnIiwxLHsiY3VydmUiOi0yfV0sWzMsMiwibSIsMV0sWzAsNF0sWzEsNV0sWzMsNCwiZSIsMSx7ImN1cnZlIjozfV0sWzMsNSwiaCIsMSx7ImN1cnZlIjotM31dXQ==
\\[\\begin{tikzcd}
\t&&& X \\\\
\t\\\\
\t&&& U \\\\
\tS && A && B && T
\t\\arrow["p"{description}, from=3-4, to=4-3]
\t\\arrow["q"{description}, from=3-4, to=4-5]
\t\\arrow["f"{description}, curve={height=12pt}, from=1-4, to=4-3]
\t\\arrow["g"{description}, curve={height=-12pt}, from=1-4, to=4-5]
\t\\arrow["m"{description}, from=1-4, to=3-4]
\t\\arrow[from=4-3, to=4-1]
\t\\arrow[from=4-5, to=4-7]
\t\\arrow["e"{description}, curve={height=18pt}, from=1-4, to=4-1]
\t\\arrow["h"{description}, curve={height=-18pt}, from=1-4, to=4-7]
\\end{tikzcd}\\]


\\ilink{More info ... jxxcarlson:manual-commutative-diagrams}



\\subsection{Tikz}


In LaTeX, Tikz provides a way to do complex graphics, often
through a tool that generates the LaTeX, e.g., \\link{mathcha.io https://mathcha.io}.  \\term_{tikz}  \\term_{mathcha.io}


We can insert tikz
in Scripta like this:

|| code
|| tikz
IMAGE URL
---
TIKZ CODE

Here is an example



|| tikz width:300 caption:Triangle
https://i.postimg.cc/jj2d9YNs/image.png
---
\\tikzset{every picture/.style={line width=0.75pt}} %set default line width to 0.75pt
%
\\begin{tikzpicture}[x=0.75pt,y=0.75pt,yscale=-1,xscale=1]
%uncomment if require: \\path (0,300); %set diagram left start at 0, and has height of 300
%
%Straight Lines [id:da5728995010079783]
\\draw [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ]   (294,51) -- (294,124.29) ;
\\draw [shift={(294,124.29)}, rotate = 90] [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ][fill={rgb, 255:red, 74; green, 144; blue, 226 }  ,fill opacity=1 ][line width=0.75]      (0, 0) circle [x radius= 3.35, y radius= 3.35]   ;
\\draw [shift={(294,51)}, rotate = 90] [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ][fill={rgb, 255:red, 74; green, 144; blue, 226 }  ,fill opacity=1 ][line width=0.75]      (0, 0) circle [x radius= 3.35, y radius= 3.35]   ;
%Straight Lines [id:da45604615155427286]
\\draw [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ]   (447.58,121) -- (289.5,121) ;
\\draw [shift={(289.5,121)}, rotate = 180] [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ][fill={rgb, 255:red, 74; green, 144; blue, 226 }  ,fill opacity=1 ][line width=0.75]      (0, 0) circle [x radius= 3.35, y radius= 3.35]   ;
\\draw [shift={(447.58,121)}, rotate = 180] [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ][fill={rgb, 255:red, 74; green, 144; blue, 226 }  ,fill opacity=1 ][line width=0.75]      (0, 0) circle [x radius= 3.35, y radius= 3.35]   ;
%Straight Lines [id:da36278375740570823]
\\draw [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ]   (290.5,53) -- (447.5,121) ;
\\draw [shift={(447.5,121)}, rotate = 23.42] [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ][fill={rgb, 255:red, 74; green, 144; blue, 226 }  ,fill opacity=1 ][line width=0.75]      (0, 0) circle [x radius= 3.35, y radius= 3.35]   ;
\\draw [shift={(290.5,53)}, rotate = 23.42] [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ][fill={rgb, 255:red, 74; green, 144; blue, 226 }  ,fill opacity=1 ][line width=0.75]      (0, 0) circle [x radius= 3.35, y radius= 3.35]   ;
%Straight Lines [id:da47493855001371243]
\\draw [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ]   (291.5,52) -- (341.5,124) ;
\\draw [shift={(341.5,124)}, rotate = 55.22] [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ][fill={rgb, 255:red, 74; green, 144; blue, 226 }  ,fill opacity=1 ][line width=0.75]      (0, 0) circle [x radius= 3.35, y radius= 3.35]   ;
\\draw [shift={(291.5,52)}, rotate = 55.22] [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ][fill={rgb, 255:red, 74; green, 144; blue, 226 }  ,fill opacity=1 ][line width=0.75]      (0, 0) circle [x radius= 3.35, y radius= 3.35]   ;
%Shape: Arc [id:dp5434791661893299]
\\draw  [draw opacity=0][fill={rgb, 255:red, 74; green, 144; blue, 226 }  ,fill opacity=0.22 ] (331.81,108.37) .. controls (333.42,108.12) and (335.12,107.99) .. (336.87,108) .. controls (347.75,108.07) and (356.54,113.5) .. (356.5,120.12) .. controls (356.5,120.34) and (356.49,120.56) .. (356.47,120.78) -- (336.8,120) -- cycle ; \\draw  [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ] (331.81,108.37) .. controls (333.42,108.12) and (335.12,107.99) .. (336.87,108) .. controls (347.75,108.07) and (356.54,113.5) .. (356.5,120.12) .. controls (356.5,120.34) and (356.49,120.56) .. (356.47,120.78) ;
%Straight Lines [id:da9645777961014412]
\\draw [color={rgb, 255:red, 74; green, 144; blue, 226 }  ,draw opacity=1 ]   (294,111) -- (307.04,111) -- (307.04,121.78) ;
% Text Node
\\draw (359,105) node  [font=\\footnotesize,color={rgb, 255:red, 74; green, 144; blue, 226 }  ,opacity=1 ,rotate=-333.43]  {$\\alpha $};
% Text Node
\\draw (284,40) node  [font=\\footnotesize,color={rgb, 255:red, 74; green, 144; blue, 226 }  ,opacity=1 ]  {$C$};
% Text Node
\\draw (293,135) node  [font=\\footnotesize,color={rgb, 255:red, 74; green, 144; blue, 226 }  ,opacity=1 ]  {$D$};
% Text Node
\\draw (340,135) node  [font=\\footnotesize,color={rgb, 255:red, 74; green, 144; blue, 226 }  ,opacity=1 ]  {$A$};
% Text Node
\\draw (449,133) node  [font=\\footnotesize,color={rgb, 255:red, 74; green, 144; blue, 226 }  ,opacity=1 ]  {$B$};
\\end{tikzpicture}




\\strong{Directions:} Copy the image created by `mathcha`, insert it
in an image hosting program, e.g. \\link{imgbb.com https://imgbb.com}, copy the resulting image URL.  Paste
it in place of `IMAGE URL`. Then copy the LaTeXand past in place of `TIKZ CODE`.

\\section{Scripta}

syspar,  var,
reflink, hide, comment,

\\subsection{Not for user}

group, lambda, ulink,  cslink, errorHighlight

\\subsection{load-files}

\\subsection{include}


\\subsection{collection}

\\subsection{runninghead}

\\subsection{banner}

\\subsection{ilink}

Use `\\ilink}{joe1234:intro-chem101}` \\term_{ilink} to make a
link to the scripta document tagged by user `joe1234` as
`joe1234:intro-chem101`

\\subsection{tags}

Uses `\\tags{foo, bar}` \\term_{tag} to tag your document with the
workd "foo" and "bar".

\\subsection{type}

\\subsection{env}



\\section{Verbatim}

\\subsection{Code}

\\begin{code}
a := 1
   b := 2
\\end{code}



\\subsection{Verbatim}

\\begin{verbatim}
a := 1
   b := 2
\\end{verbatim}

|| verbatim
a := 1
   b := 2

\\subsection{Verse}

\\begin{verse}
Roses are red
Violets are blue
\\end{verse}

|| verse
  Roses are red
    Violets are blue
Etc., etc.


\\subsection{Hide}

\\begin{hide}
Whatever
\\end{hide}

|| hide
Whatever

| endnotes

| section 1 -
Index

| index







"""
