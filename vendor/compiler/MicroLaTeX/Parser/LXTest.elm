module MicroLaTeX.Parser.LXTest exposing (str)


str =
    """
\\begin{equation}
W =
\\begin{pmatrix}
-2 & \\phantom{-}1 & \\phantom{-}0 & \\phantom{-}0 & \\phantom{-}0 \\\\
\\phantom{-}1 & -2 & \\phantom{-}1 & \\phantom{-}0 & \\phantom{-}0 \\\\
\\phantom{-}0 & \\phantom{-}1 & -2 & \\phantom{-}1 & \\phantom{-}0 \\\\
\\phantom{-}0 & \\phantom{-}0 & \\phantom{-}1 & -2 & \\phantom{-}1 \\\\
\\phantom{-}0 & \\phantom{-}0 & \\phantom{-}0 & \\phantom{-}1 & -2
\\end{pmatrix}
\\end{equation}
"""
