module Tools exposing
    ( blue
    , cyan
    , debugBlue
    , debugCyan
    , forklogBlue
    , forklogCyan
    , forklogRed
    , forklogYellow
    )

import Console



--
--debugCyan label width a = Debug.log (coloredLabel Console.black Console.bgCyan label width) a
--debugBlue label width a = Debug.log (coloredLabel Console.white Console.bgBlue label width) a


debugCyan label width a =
    a


debugBlue label width a =
    a


forklogRed label width f a =
    forklog_ Console.white Console.bgRed label width f a


forklogYellow label width f a =
    forklog_ Console.black Console.bgYellow label width f a


forklogCyan : String -> Int -> (d -> a) -> d -> d
forklogCyan label width f a =
    forklog_ Console.black Console.bgCyan label width f a


forklogBlue label width f a =
    forklog_ Console.white Console.bgBlue label width f a



--forklog_ fg bg label width f a =
--    case Env.mode of
--        Env.Development ->
--            let
--                _ =
--                    Debug.log (coloredLabel fg bg label width) (f a)
--            in
--            a
--
--        Env.Production ->
--            a


forklog_ fg bg label width f a =
    a


coloredLabel fg bg label width =
    let
        n =
            String.length label

        padding =
            String.repeat (width - n) " "
    in
    label |> (\x -> " " ++ x ++ padding) |> bg |> fg


blue : String -> Int -> String
blue label width =
    let
        n =
            String.length label

        padding =
            String.repeat (width - n) " "
    in
    label |> (\x -> " " ++ x ++ padding) |> Console.bgBlue |> Console.white


cyan label width =
    let
        n =
            String.length label

        padding =
            String.repeat (width - n) " "
    in
    label |> (\x -> " " ++ x ++ padding) |> Console.bgCyan |> Console.black
