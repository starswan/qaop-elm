module MemoryAddress exposing (..)

import Bitwise exposing (shiftRightBy)


type ScreenType
    = Screen
    | ULA


type HimemType
    = HimemLow
    | HimemHigh


type MemoryAddress
    = ROM Int
    | ULAMem ScreenType Int
    | Himem HimemType Int


fromInt : Int -> MemoryAddress
fromInt inAddr =
    let
        addr =
            inAddr |> Bitwise.and 0x3FFF
    in
    case inAddr |> shiftRightBy 14 of
        3 ->
            Himem HimemHigh addr

        2 ->
            Himem HimemLow addr

        1 ->
            if addr >= 6912 then
                ULAMem ULA (addr - 6912)

            else
                ULAMem Screen addr

        _ ->
            ROM addr
