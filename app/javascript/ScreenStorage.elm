module ScreenStorage exposing (..)

-- Convert row index into start row data location

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import SpectrumColour exposing (BorderColour(..))
import Vector24 exposing (Vector24)
import Vector32 exposing (Vector32)
import Vector8 exposing (Vector8)
import Z80Memory exposing (Z80Memory, getMemValue)



--range07 =
--    --    List.range 0 7
--    --    Vector8.from8 Vector24.Index0 Vector24.Index1 Vector24.Index2 Vector24.Index3 Vector24.Index4 Vector24.Index5 Vector24.Index6 Vector24.Index7
--    [ Vector24.Index0, Vector24.Index1, Vector24.Index2, Vector24.Index3, Vector24.Index4, Vector24.Index5, Vector24.Index6, Vector24.Index7 ]
--
--
--range8_15 =
--    --List.range 8 15
--    --Vector8.from8 Vector24.Index8 Vector24.Index9 Vector24.Index10 Vector24.Index11 Vector24.Index12 Vector24.Index13 Vector24.Index14 Vector24.Index15
--    [ Vector24.Index8, Vector24.Index9, Vector24.Index10, Vector24.Index11, Vector24.Index12, Vector24.Index13, Vector24.Index14, Vector24.Index15 ]
--
--
--range16_23 =
--    --List.range 16 23
--    --Vector8.from8 Vector24.Index16 Vector24.Index17 Vector24.Index18 Vector24.Index19 Vector24.Index20 Vector24.Index21 Vector24.Index22 Vector24.Index23
--    [ Vector24.Index16, Vector24.Index17, Vector24.Index18, Vector24.Index19, Vector24.Index20, Vector24.Index21, Vector24.Index22, Vector24.Index23 ]
--bank0_attr_indexes =
--    --range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07
--    List.repeat 8 range07 |> List.concat
--
--
--bank1_attr_indexes =
--    --range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15
--    List.repeat 8 range8_15 |> List.concat
--
--
--bank2_attr_indexes =
--    --range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23
--    List.repeat 8 range16_23 |> List.concat
-- 192 mappings of screen index to attribute data index
--attr_indexes : List Vector24.Index
--attr_indexes =
--    Vector24.indices |> Vector24.map (\ind24 -> List.repeat 8 ind24) |> Vector24.toList |> List.concat


attr_indexes : Vector24 (Vector8 Vector24.Index)
attr_indexes =
    Vector24.indices |> Vector24.map (\ind24 -> Vector8.repeat ind24)


type alias Z80Screen =
    { data : Z80Memory
    , attrs : Vector24 (Vector32 Int)
    , border : BorderColour
    , flash : Bool

    --refrs_a: Int,
    --refrs_b: Int,
    --refrs_t: Int,
    --refrs_s: Int
    }


type alias RawScreenData =
    { -- colour data is bit 7 flash, bit 6 bright, bits 5-3 paper, bits 2-0 ink
      colour : Int
    , data : Int
    }


constructor : Z80Screen
constructor =
    let
        screen_data =
            List.repeat 6144 0 |> Z80Memory.constructor

        --for(int i=6144;i<6912;i++) ram[i] = 070; // white
        --attributes =
        --    List.repeat 768 0x38 |> Z80Memory.constructor
        attr_line =
            Vector32.repeat 0x38

        attributes =
            Vector24.repeat attr_line
    in
    Z80Screen screen_data attributes BorderWhite False


calcDataOffset : Int -> Int
calcDataOffset start =
    let
        bankStart =
            start |> Bitwise.and 0xC0

        bankOffset =
            start |> Bitwise.and 0x3F |> shiftRightBy 3

        data_offset =
            start |> Bitwise.and 0x07 |> shiftLeftBy 3
    in
    bankStart + bankOffset + data_offset



--screenOffsets : List (Int, Vector24.Index)
--screenOffsets =
--    attr_indexes |> List.indexedMap (\index attr_index -> ( calcDataOffset index, attr_index ))
--attr_indexes : Vector24 (Vector8 Vector24.Index)
--screenOffsets : List (Int, Vector24.Index)
--screenOffsets =
--    attr_indexes |> List.indexedMap (\index attr_index -> ( calcDataOffset index, attr_index ))
--screenOffsets : Vector24 (Vector8 ( Int, Vector24.Index ))


screenOffsets : List ( Int, Vector24.Index )
screenOffsets =
    let
        offsets : Vector24 (Vector8 ( Int, Vector24.Index ))
        offsets =
            attr_indexes
                |> Vector24.indexedMap
                    (\vec8_index vec8 ->
                        vec8
                            |> Vector8.indexedMap
                                (\index8 item ->
                                    let
                                        index =
                                            (vec8_index |> Vector24.indexToInt) * 8 + (index8 |> Vector8.indexToInt)
                                    in
                                    ( calcDataOffset index, item )
                                )
                    )
    in
    offsets |> Vector24.map (\vec8 -> vec8 |> Vector8.toList) |> Vector24.toList |> List.concat



--rowIndexes =
--    Vector24.indices
--vec32Indexes =
--    Vector32.indices
--vec8Indexes =
--    Vector8.indices
--rowStartIndexes =
--    rowIndexes
--        |> Vector24.map
--            (\ind24 ->
--                let
--                    offsets =
--                        vec8Indexes |> Vector8.map (\index -> (index |> Vector8.indexToInt) * 8)
--                in
--                ( ind24, offsets )
--            )
--mapScreen : ( Int, Int ) -> Z80Screen -> Int -> RawScreenData
--mapScreen ( row_index, attr_index ) z80_screen index =
--    let
--        --row_offset =
--        --    row_index * 32
--        --
--        --attr_offset =
--        --    attr_index * 32
--        data =
--            z80_screen.data |> getMemValue (row_index * 32 + index)
--
--        colour =
--            z80_screen.attrs |> getMemValue (attr_index * 32 + index)
--    in
--    { colour = colour, data = data }


setScreenValue : Int -> Int -> Z80Screen -> Z80Screen
setScreenValue addr value z80screen =
    --let
    --    z80screen =
    --        z80s |> refresh_screen
    --in
    if addr < 0x1800 then
        { z80screen | data = z80screen.data |> Z80Memory.setMemValue addr value }

    else
        let
            offset =
                addr - 0x1800

            row =
                offset // 32 |> Vector24.intToIndex |> Maybe.withDefault Vector24.Index0

            oldrow =
                z80screen.attrs |> Vector24.get row

            col =
                offset |> remainderBy 32 |> Vector32.intToIndex |> Maybe.withDefault Vector32.Index0

            new_row =
                oldrow |> Vector32.set col value
        in
        --{ z80screen | attrs = z80screen.attrs |> Z80Memory.setMemValue (addr - 0x1800) value }
        { z80screen | attrs = z80screen.attrs |> Vector24.set row new_row }


getScreenValue : Int -> Z80Screen -> Int
getScreenValue addr screen =
    if addr < 0x1800 then
        screen.data |> getMemValue addr

    else
        --screen.attrs |> getMemValue (addr - 0x1800)
        let
            offset =
                addr - 0x1800

            row =
                offset // 32 |> Vector24.intToIndex |> Maybe.withDefault Vector24.Index0

            oldrow =
                screen.attrs |> Vector24.get row

            col =
                offset |> remainderBy 32 |> Vector32.intToIndex |> Maybe.withDefault Vector32.Index0
        in
        oldrow |> Vector32.get col


rawScreenData : Z80Screen -> List (Vector32 RawScreenData)
rawScreenData z80_screen =
    screenOffsets
        |> List.map
            (\( row_index, attr_index ) ->
                let
                    data_row =
                        memoryRow z80_screen row_index

                    attr_row =
                        z80_screen.attrs |> Vector24.get attr_index
                in
                Vector32.map2 (\data attr -> { colour = attr, data = data }) data_row attr_row
            )


memoryRow : Z80Screen -> Int -> Vector32 Int
memoryRow z80_screen row_index =
    let
        offset =
            row_index * 32

        d0 =
            z80_screen.data |> getMemValue (offset + 0)

        d1 =
            z80_screen.data |> getMemValue (offset + 1)

        d2 =
            z80_screen.data |> getMemValue (offset + 2)

        d3 =
            z80_screen.data |> getMemValue (offset + 3)

        d4 =
            z80_screen.data |> getMemValue (offset + 4)

        d5 =
            z80_screen.data |> getMemValue (offset + 5)

        d6 =
            z80_screen.data |> getMemValue (offset + 6)

        d7 =
            z80_screen.data |> getMemValue (offset + 7)

        d8 =
            z80_screen.data |> getMemValue (offset + 8)

        d9 =
            z80_screen.data |> getMemValue (offset + 9)

        d10 =
            z80_screen.data |> getMemValue (offset + 10)

        d11 =
            z80_screen.data |> getMemValue (offset + 11)

        d12 =
            z80_screen.data |> getMemValue (offset + 12)

        d13 =
            z80_screen.data |> getMemValue (offset + 13)

        d14 =
            z80_screen.data |> getMemValue (offset + 14)

        d15 =
            z80_screen.data |> getMemValue (offset + 15)

        d16 =
            z80_screen.data |> getMemValue (offset + 16)

        d17 =
            z80_screen.data |> getMemValue (offset + 17)

        d18 =
            z80_screen.data |> getMemValue (offset + 18)

        d19 =
            z80_screen.data |> getMemValue (offset + 19)

        d20 =
            z80_screen.data |> getMemValue (offset + 20)

        d21 =
            z80_screen.data |> getMemValue (offset + 21)

        d22 =
            z80_screen.data |> getMemValue (offset + 22)

        d23 =
            z80_screen.data |> getMemValue (offset + 23)

        d24 =
            z80_screen.data |> getMemValue (offset + 24)

        d25 =
            z80_screen.data |> getMemValue (offset + 25)

        d26 =
            z80_screen.data |> getMemValue (offset + 26)

        d27 =
            z80_screen.data |> getMemValue (offset + 27)

        d28 =
            z80_screen.data |> getMemValue (offset + 28)

        d29 =
            z80_screen.data |> getMemValue (offset + 29)

        d30 =
            z80_screen.data |> getMemValue (offset + 30)

        d31 =
            z80_screen.data |> getMemValue (offset + 31)

        data_row =
            Vector32.from32 d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31
    in
    data_row



--refresh_screen : Z80Screen -> Z80Screen
--refresh_screen z80env =
--    z80env
