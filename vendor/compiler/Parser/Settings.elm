module Parser.Settings exposing (Arity, blockData, numberedBlockNames)


numberedBlockNames =
    [ "q"
    , "axiom"
    , "theorem"
    , "definition"
    , "lemma"
    , "proposition"
    , "corollary"
    , "note"
    , "remark"
    , "question"
    , "problem"
    , "example"
    , "equation"
    , "aligned"
    , "quiver"
    , "image"
    , "iframe"
    , "chart"
    ]


type Arity
    = Arity Int


blockData =
    [ ( "item", { prefix = "|", arity = Arity 0 } )
    , ( "mathmacros", { prefix = "||", arity = Arity 0 } )
    , ( "a", { prefix = "|", arity = Arity 0 } )
    , ( "q", { prefix = "|", arity = Arity 0 } )
    , ( "axiom", { prefix = "|", arity = Arity 0 } )
    , ( "corollary", { prefix = "|", arity = Arity 0 } )
    , ( "definition", { prefix = "|", arity = Arity 0 } )
    , ( "equation", { prefix = "||", arity = Arity 0 } )
    , ( "aligned", { prefix = "||", arity = Arity 0 } )
    , ( "iframe", { prefix = "||", arity = Arity 0 } )
    , ( "example", { prefix = "|", arity = Arity 0 } )
    , ( "lemma", { prefix = "|", arity = Arity 0 } )
    , ( "note", { prefix = "|", arity = Arity 0 } )
    , ( "problem", { prefix = "|", arity = Arity 0 } )
    , ( "proposition", { prefix = "|", arity = Arity 0 } )
    , ( "remark", { prefix = "|", arity = Arity 0 } )
    , ( "theorem", { prefix = "|", arity = Arity 0 } )
    , ( "indent", { prefix = "|", arity = Arity 0 } )
    , ( "quotation", { prefix = "|", arity = Arity 0 } )
    , ( "numbered", { prefix = "|", arity = Arity 0 } )
    , ( "abstract", { prefix = "|", arity = Arity 0 } )
    , ( "bibitem", { prefix = "|", arity = Arity 1 } )
    , ( "desc", { prefix = "|", arity = Arity 1 } )
    , ( "setcounter", { prefix = "|", arity = Arity 1 } )
    , ( "contents", { prefix = "|", arity = Arity 0 } )
    ]
