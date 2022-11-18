module Render.Export.Preamble exposing (make, supportingCode)

-- PREAMBLE

import List.Extra


make : List String -> List String -> String
make blockNames_ expressionNames_ =
    let
        names =
            blockNames_ ++ expressionNames_

        packagesUsed =
            packagesNeeded names
    in
    [ "\\documentclass[11pt, oneside]{article}"
    , newPackageText packagesUsed
    , supportingCode packagesUsed
    , standardPackages
    , commands
    ]
        |> String.join "\n"



-- PACKAGELIST


packageList =
    [ ( "quiver", [ "quiver" ] )
    , ( "tikz", [ "tikz" ] )
    , ( "link", [ "hyperref" ] )
    , ( "ilink", [ "hyperref" ] )
    , ( "href", [ "hyperref" ] )
    , ( "textcolor", [ "xcolor" ] )
    , ( "blue", [ "xcolor" ] )
    , ( "red", [ "xcolor" ] )
    , ( "green", [ "xcolor" ] )
    , ( "gray", [ "xcolor" ] )
    , ( "magenta", [ "xcolor" ] )
    , ( "violet", [ "xcolor" ] )
    , ( "pink", [ "xcolor" ] )
    , ( "highlight", [ "xcolor" ] )
    , ( "highlight", [ "soul" ] )
    , ( "strike", [ "soul" ] )
    , ( "errorHighlight", [ "xcolor" ] )
    , ( "image", [ "graphicx", "wrapfig", "float" ] )
    ]


setupCode =
    [ ( "graphicx", "\\graphicspath{ {image/} }" )
    , ( "hyperref", hypersetup )
    ]


hypersetup =
    """
\\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    filecolor=magenta,
    urlcolor=blue,
}
"""


packagesNeeded : List String -> List String
packagesNeeded names =
    names
        |> newPackageList
        |> List.Extra.unique
        |> List.sort


{-|

    'blockNames' is a list of blocks that occur in a document
    to be exported.  'newPackageList blockNames' is a list
    of the corresponding packages that the document will need
    to handle the given blocks.

-}
newPackageText : List String -> String
newPackageText packagesNeeded_ =
    packagesNeeded_
        |> List.map (\name -> "\\usepackage{" ++ name ++ "}")
        |> String.join "\n"


newPackageList : List String -> List String
newPackageList names =
    List.foldl (\( entityName, packageNames ) acc -> addPackage names entityName packageNames acc) [] packageList


supportingCode : List String -> String
supportingCode packagesInDocument =
    List.foldl (\( entityName, packageNames ) acc -> addCode packagesInDocument entityName packageNames acc) "" setupCode


{-| If
-}
addPackage : List String -> String -> List String -> List String -> List String
addPackage namesInDocument entityName packageNames packages_ =
    if List.member entityName namesInDocument then
        packageNames ++ packages_

    else
        packages_


addCode : List String -> String -> String -> String -> String
addCode packagesInDocument package codeText accumulatedCodeText =
    if List.member package packagesInDocument then
        codeText ++ "\n\n" ++ accumulatedCodeText

    else
        accumulatedCodeText



--
--packages entityNames =
--    [ newPackages entityNames, standardPackages ] |> String.join "\n"


standardPackages =
    """
%% Packages

%% Standard packages
\\usepackage{geometry}
\\geometry{letterpaper}
\\usepackage{changepage}   % for the adjustwidth environment

%% AMS
\\usepackage{amssymb}
\\usepackage{amsmath}

\\usepackage{amscd}

\\usepackage{fancyvrb} %% for inline verbatim
"""


commands =
    """
%% Commands

\\newcommand{\\code}[1]{{\\tt #1}}
\\newcommand{\\ellie}[1]{\\href{#1}{Link to Ellie}}
% \\newcommand{\\image}[3]{\\includegraphics[width=3cm]{#1}}

%% width=4truein,keepaspectratio]


\\newcommand{\\imagecentercaptioned}[3]{
   \\medskip
   \\begin{figure}[htp]
   \\centering
    \\includegraphics[width=#2]{#1}
    \\vglue0pt
    \\caption{#3}
    \\end{figure}
    \\medskip
}

\\newcommand{\\imagecenter}[2]{
   \\medskip
   \\begin{figure}[htp]
   \\centering
    \\includegraphics[width=#2]{#1}
    \\vglue0pt
    \\end{figure}
    \\medskip
}

\\newcommand{\\imagefloat}[4]{
    \\begin{wrapfigure}{#4}{#2}
    \\includegraphics[width=#2]{#1}
    \\caption{#3}
    \\end{wrapfigure}
}


\\newcommand{\\imagefloatright}[3]{
    \\begin{wrapfigure}{R}{0.30\\textwidth}
    \\includegraphics[width=0.30\\textwidth]{#1}
    \\caption{#2}
    \\end{wrapfigure}
}

\\newcommand{\\hide}[1]{}


\\newcommand{\\imagefloatleft}[3]{
    \\begin{wrapfigure}{L}{0.3-\\textwidth}
    \\includegraphics[width=0.30\\textwidth]{#1}
    \\caption{#2}
    \\end{wrapfigure}
}
% Font style
\\newcommand{\\italic}[1]{{\\sl #1}}
\\newcommand{\\strong}[1]{{\\bf #1}}
\\newcommand{\\strike}[1]{\\st{#1}}

% Scripta
\\newcommand{\\ilink}[2]{\\href{{https://scripta.io/s/#1}}{#2}}

% Color
\\newcommand{\\red}[1]{\\textcolor{red}{#1}}
\\newcommand{\\blue}[1]{\\textcolor{blue}{#1}}
\\newcommand{\\violet}[1]{\\textcolor{violet}{#1}}
\\newcommand{\\highlight}[1]{\\hl{#1}}
\\newcommand{\\note}[2]{\\textcolor{blue}{#1}{\\hl{#1}}}

% WTF?
\\newcommand{\\remote}[1]{\\textcolor{red}{#1}}
\\newcommand{\\local}[1]{\\textcolor{blue}{#1}}

% Unclassified
\\newcommand{\\subheading}[1]{{\\bf #1}\\par}
\\newcommand{\\term}[1]{{\\sl #1}}
\\newcommand{\\termx}[1]{}
\\newcommand{\\comment}[1]{}
\\newcommand{\\innertableofcontents}{}


% Special character
\\newcommand{\\dollarSign}[0]{{\\$}}
\\newcommand{\\backTick}[0]{\\`{}}

%% Theorems
\\newtheorem{remark}{Remark}
\\newtheorem{theorem}{Theorem}
\\newtheorem{axiom}{Axiom}
\\newtheorem{lemma}{Lemma}
\\newtheorem{proposition}{Proposition}
\\newtheorem{corollary}{Corollary}
\\newtheorem{definition}{Definition}
\\newtheorem{example}{Example}
\\newtheorem{exercise}{Exercise}
\\newtheorem{problem}{Problem}
\\newtheorem{exercises}{Exercises}
\\newcommand{\\bs}[1]{$\\backslash$#1}
\\newcommand{\\texarg}[1]{\\{#1\\}}


%% Environments
\\renewenvironment{quotation}
  {\\begin{adjustwidth}{2cm}{} \\footnotesize}
  {\\end{adjustwidth}}

\\def\\changemargin#1#2{\\list{}{\\rightmargin#2\\leftmargin#1}\\item[]}
\\let\\endchangemargin=\\endlist

\\renewenvironment{indent}
  {\\begin{adjustwidth}{0.75cm}{}}
  {\\end{adjustwidth}}


%% NEWCOMMAND

% \\definecolor{mypink1}{rgb}{0.858, 0.188, 0.478}
% \\definecolor{mypink2}{RGB}{219, 48, 122}
\\newcommand{\\fontRGB}[4]{
    \\definecolor{mycolor}{RGB}{#1, #2, #3}
    \\textcolor{mycolor}{#4}
    }

\\newcommand{\\highlightRGB}[4]{
    \\definecolor{mycolor}{RGB}{#1, #2, #3}
    \\sethlcolor{mycolor}
    \\hl{#4}
     \\sethlcolor{yellow}
    }

\\newcommand{\\gray}[2]{
\\definecolor{mygray}{gray}{#1}
\\textcolor{mygray}{#2}
}

\\newcommand{\\white}[1]{\\gray{1}[#1]}
\\newcommand{\\medgray}[1]{\\gray{0.5}[#1]}
\\newcommand{\\black}[1]{\\gray{0}[#1]}

% Spacing
\\parindent0pt
\\parskip5pt

"""
