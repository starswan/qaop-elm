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


fromInt16 : Int -> ( MemoryAddress, MemoryAddress )
fromInt16 inAddr =
    let
        addr =
            inAddr |> Bitwise.and 0x3FFF

        nonOnBoundary =
            addr /= 0x3FFF
    in
    case inAddr |> Bitwise.and 0xC000 of
        0xC000 ->
            if nonOnBoundary then
                ( RAM (Himem HimemHigh addr), RAM (Himem HimemHigh (addr + 1)) )

            else
                ( RAM (Himem HimemHigh addr), ROM 0 )

        0x8000 ->
            if nonOnBoundary then
                ( RAM (Himem HimemLow addr), RAM (Himem HimemLow (addr + 1)) )

            else
                ( RAM (Himem HimemLow addr), RAM (Himem HimemHigh 0) )

        0x4000 ->
            if nonOnBoundary then
                if addr /= 6911 then
                    if addr >= 6912 then
                        ( RAM (ULAMem ULA (addr - 6912)), RAM (ULAMem ULA (addr - 6911)) )

                    else
                        ( RAM (ULAMem Screen addr), RAM (ULAMem Screen (addr + 1)) )

                else
                    ( RAM (ULAMem Screen addr), RAM (ULAMem ULA 0) )

            else
                ( RAM (ULAMem ULA (addr - 6912)), RAM (Himem HimemLow 0) )

        _ ->
            if nonOnBoundary then
                ( ROM addr, ROM (addr + 1) )

            else
                ( ROM addr, RAM (ULAMem Screen 0) )
