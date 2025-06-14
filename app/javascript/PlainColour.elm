module PlainColour exposing (..)


type PlainColour
    = Black
    | Blue
    | Red
    | Magenta
    | Green
    | Cyan
    | Yellow
    | White


fromInt : Int -> PlainColour
fromInt int =
    case int of
        0 ->
            Black

        1 ->
            Blue

        2 ->
            Red

        3 ->
            Magenta

        4 ->
            Green

        5 ->
            Cyan

        6 ->
            Yellow

        _ ->
            White


toInt : PlainColour -> Int
toInt plainColour =
    case plainColour of
        Black ->
            0

        Blue ->
            1

        Red ->
            2

        Magenta ->
            3

        Green ->
            4

        Cyan ->
            5

        Yellow ->
            6

        White ->
            7
