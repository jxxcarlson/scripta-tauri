module L0.Parser.Error exposing (ordinaryBlock)


ordinaryBlock : List String -> String -> ( String, List String )
ordinaryBlock currentMessages revisedContent =
    let
        lines =
            String.lines (String.trim revisedContent)

        n =
            List.length lines

        messages =
            currentMessages

        content =
            if n <= 1 then
                -- TODO: rethink this.
                " "

            else
                List.drop 1 lines |> String.join "\n"
    in
    ( content, messages )
