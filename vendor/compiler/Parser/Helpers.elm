module Parser.Helpers exposing
    ( Step(..)
    , getFirstOccurrence
    , loop
    , prependMessage
    )


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b


getFirstOccurrence : (a -> Bool) -> List a -> Maybe a
getFirstOccurrence predicate list =
    loop list (nextStep predicate)


nextStep : (a -> Bool) -> List a -> Step (List a) (Maybe a)
nextStep predicate list =
    case List.head list of
        Nothing ->
            Done Nothing

        Just item ->
            if predicate item then
                Done (Just item)

            else
                Loop (List.drop 1 list)


prependMessage : Int -> String -> List String -> List String
prependMessage lineNumber message messages =
    (message ++ " (line " ++ String.fromInt lineNumber ++ ")") :: List.take 2 messages
