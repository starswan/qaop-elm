module MemoryAddress exposing (..)

import Bitwise


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
    case inAddr |> Bitwise.and 0xC000 of
        0xC000 ->
            RAM (Himem HimemHigh addr)

        0x8000 ->
            RAM (Himem HimemLow addr)

        0x4000 ->
            if addr >= 6912 then
                RAM (ULAMem ULA (addr - 6912))

            else
                RAM (ULAMem Screen addr)

        _ ->
            ROM addr
