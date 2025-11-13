module MemoryAddress exposing (..)

import Bitwise exposing (shiftRightBy)


type ScreenType
    = Screen
    | ULA


type HimemType
    = HimemLow
    | HimemHigh


type RamAddress
    = ULAMem ScreenType Int
    | Himem HimemType Int


type MemoryAddress
    = ROM Int
    | RAM RamAddress


fromInt : Int -> MemoryAddress
fromInt inAddr =
    let
        addr =
            inAddr |> Bitwise.and 0x3FFF
    in
    case inAddr |> shiftRightBy 14 of
        3 ->
            RAM (Himem HimemHigh addr)

        2 ->
            RAM (Himem HimemLow addr)

        1 ->
            if addr >= 6912 then
                RAM (ULAMem ULA (addr - 6912))

            else
                RAM (ULAMem Screen addr)

        _ ->
            ROM addr
