module ScreenAttr exposing (..)

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import PlainColour exposing (PlainColour)


type alias ScreenAttr =
    { flash : Bool
    , bright : Bool
    , paper : PlainColour
    , ink : PlainColour
    }


fromInt : Int -> ScreenAttr
fromInt int =
    { flash = (int |> Bitwise.and 0x80) /= 0
    , bright = (int |> Bitwise.and 0x40) /= 0
    , paper = int |> Bitwise.and 0x38 |> shiftRightBy 3 |> PlainColour.fromInt
    , ink = int |> Bitwise.and 0x07 |> PlainColour.fromInt
    }


toInt : ScreenAttr -> Int
toInt attr =
    let
        flash =
            if attr.flash then
                0x80

            else
                0x00

        bright =
            if attr.bright then
                0x40

            else
                0x00

        paper =
            attr.paper |> PlainColour.toInt |> shiftLeftBy 3

        ink =
            attr.ink |> PlainColour.toInt
    in
    flash |> Bitwise.or bright |> Bitwise.or paper |> Bitwise.or ink
