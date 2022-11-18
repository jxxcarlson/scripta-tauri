module L0.Parser.Line exposing (getNameAndArgs)


getNameAndArgs : { a | content : String } -> ( Maybe String, List String )
getNameAndArgs line =
    let
        normalizedLine =
            String.trim line.content

        -- account for possible indentation
    in
    if String.left 2 normalizedLine == "||" then
        let
            words =
                String.words (String.dropLeft 3 normalizedLine)

            name =
                List.head words |> Maybe.withDefault "anon"

            args =
                List.drop 1 words
        in
        ( Just name, args )

    else if String.left 1 normalizedLine == "|" then
        let
            words =
                String.words (String.dropLeft 2 normalizedLine)

            name =
                List.head words |> Maybe.withDefault "anon"

            args =
                List.drop 1 words
        in
        ( Just name, args )

    else if String.left 2 line.content == "$$" then
        ( Just "math", [] )

    else
        ( Nothing, [] )
