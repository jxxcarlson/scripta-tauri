module XMarkdown.Line exposing (getNameAndArgs)


getNameAndArgs line =
    if String.left 3 line.content == "```" then
        ( Just "code", [] )

    else if String.left 3 line.content == "|| " then
        ( Just (String.dropLeft 3 line.content |> String.trimRight), [] )

    else if String.left 2 line.content == "$$" then
        ( Just "math", [] )

    else if String.left 2 line.content == "| " then
        ( Just (String.dropLeft 2 line.content |> String.trimRight), [] )

    else
        ( Nothing, [] )
