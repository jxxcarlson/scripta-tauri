module DateUtility exposing (foo)


foo =
    1



--
--import Effect.Time exposing (Month(..))
--
--
--toString : Effect.Time.Zone -> Effect.Time.Posix -> String
--toString zone time =
--    monthString (Effect.Time.toMonth zone time)
--        ++ "/"
--        ++ (String.fromInt (Effect.Time.toDay zone time) |> String.padLeft 2 '0')
--        --++ "/"
--        --++ String.fromInt (Effect.Time.toYear zone time)
--        ++ ", "
--        ++ (String.fromInt (Effect.Time.toHour zone time) |> String.padLeft 2 '0')
--        ++ ":"
--        ++ (String.fromInt (Effect.Time.toMinute zone time) |> String.padLeft 2 '0')
--
--
--toStringWithYear : Effect.Time.Zone -> Effect.Time.Posix -> String
--toStringWithYear zone time =
--    monthString (Effect.Time.toMonth zone time)
--        ++ "/"
--        ++ (String.fromInt (Effect.Time.toDay zone time) |> String.padLeft 2 '0')
--        ++ "/"
--        ++ String.fromInt (Effect.Time.toYear zone time)
--
--
--monthString : Effect.Time.Month -> String
--monthString month =
--    case month of
--        Effect.Time.Jan ->
--            "1"
--
--        Effect.Time.Feb ->
--            "2"
--
--        Effect.Time.Mar ->
--            "3"
--
--        Effect.Time.Apr ->
--            "4"
--
--        Effect.Time.May ->
--            "5"
--
--        Effect.Time.Jun ->
--            "6"
--
--        Effect.Time.Jul ->
--            "7"
--
--        Effect.Time.Aug ->
--            "8"
--
--        Effect.Time.Sep ->
--            "9"
--
--        Effect.Time.Oct ->
--            "10"
--
--        Effect.Time.Nov ->
--            "11"
--
--        Effect.Time.Dec ->
--            "12"
