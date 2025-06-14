module ScreenStorage exposing (..)

-- Convert row index into start row data location

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import PlainColour exposing (PlainColour(..))
import ScreenAttr exposing (ScreenAttr)
import Vector24 exposing (Vector24)
import Vector32 exposing (Vector32)
import Z80Memory exposing (Z80Memory, getMemValue)


range07 =
    --    List.range 0 7
    --    Vector8.from8 Vector24.Index0 Vector24.Index1 Vector24.Index2 Vector24.Index3 Vector24.Index4 Vector24.Index5 Vector24.Index6 Vector24.Index7
    [ Vector24.Index0, Vector24.Index1, Vector24.Index2, Vector24.Index3, Vector24.Index4, Vector24.Index5, Vector24.Index6, Vector24.Index7 ]


range8_15 =
    --List.range 8 15
    --Vector8.from8 Vector24.Index8 Vector24.Index9 Vector24.Index10 Vector24.Index11 Vector24.Index12 Vector24.Index13 Vector24.Index14 Vector24.Index15
    [ Vector24.Index8, Vector24.Index9, Vector24.Index10, Vector24.Index11, Vector24.Index12, Vector24.Index13, Vector24.Index14, Vector24.Index15 ]


range16_23 =
    --List.range 16 23
    --Vector8.from8 Vector24.Index16 Vector24.Index17 Vector24.Index18 Vector24.Index19 Vector24.Index20 Vector24.Index21 Vector24.Index22 Vector24.Index23
    [ Vector24.Index16, Vector24.Index17, Vector24.Index18, Vector24.Index19, Vector24.Index20, Vector24.Index21, Vector24.Index22, Vector24.Index23 ]


bank0_attr_indexes =
    --range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07
    List.repeat 8 range07 |> List.concat


bank1_attr_indexes =
    --range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15
    List.repeat 8 range8_15 |> List.concat


bank2_attr_indexes =
    --range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23
    List.repeat 8 range16_23 |> List.concat



-- 192 mappings of screen index(0-191) to attribute data index(Vector24.Index, 0-23)


attr_indexes =
    bank0_attr_indexes ++ bank1_attr_indexes ++ bank2_attr_indexes


type alias Z80Screen =
    { data : Z80Memory
    , attrs : Vector24 (Vector32 ScreenAttr)
    , border : PlainColour
    , flash : Bool

    --refrs_a: Int,
    --refrs_b: Int,
    --refrs_t: Int,
    --refrs_s: Int
    }


type alias RawScreenData =
    { colour : ScreenAttr
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
            Vector32.repeat (0x38 |> ScreenAttr.fromInt)

        attributes =
            Vector24.repeat attr_line
    in
    Z80Screen screen_data attributes White False


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


screenOffsets =
    attr_indexes |> List.indexedMap (\index attr_index -> ( calcDataOffset index, attr_index ))


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
                oldrow |> Vector32.set col (value |> ScreenAttr.fromInt)
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
        oldrow |> Vector32.get col |> ScreenAttr.toInt


rawScreenData : Z80Screen -> List (Vector32 RawScreenData)
rawScreenData z80_screen =
    screenOffsets
        |> List.map
            (\( row_index, attr_index ) ->
                let
                    data_row =
                        getScreenVector z80_screen.data row_index

                    attr_row =
                        z80_screen.attrs |> Vector24.get attr_index
                in
                Vector32.map2 (\data attr -> { colour = attr, data = data }) data_row attr_row
            )


getScreenVector : Z80Memory -> Int -> Vector32 Int
getScreenVector z80_screen_data row_index =
    let
        offset =
            row_index * 32

        d0 =
            z80_screen_data |> getMemValue (offset + 0)

        d1 =
            z80_screen_data |> getMemValue (offset + 1)

        d2 =
            z80_screen_data |> getMemValue (offset + 2)

        d3 =
            z80_screen_data |> getMemValue (offset + 3)

        d4 =
            z80_screen_data |> getMemValue (offset + 4)

        d5 =
            z80_screen_data |> getMemValue (offset + 5)

        d6 =
            z80_screen_data |> getMemValue (offset + 6)

        d7 =
            z80_screen_data |> getMemValue (offset + 7)

        d8 =
            z80_screen_data |> getMemValue (offset + 8)

        d9 =
            z80_screen_data |> getMemValue (offset + 9)

        d10 =
            z80_screen_data |> getMemValue (offset + 10)

        d11 =
            z80_screen_data |> getMemValue (offset + 11)

        d12 =
            z80_screen_data |> getMemValue (offset + 12)

        d13 =
            z80_screen_data |> getMemValue (offset + 13)

        d14 =
            z80_screen_data |> getMemValue (offset + 14)

        d15 =
            z80_screen_data |> getMemValue (offset + 15)

        d16 =
            z80_screen_data |> getMemValue (offset + 16)

        d17 =
            z80_screen_data |> getMemValue (offset + 17)

        d18 =
            z80_screen_data |> getMemValue (offset + 18)

        d19 =
            z80_screen_data |> getMemValue (offset + 19)

        d20 =
            z80_screen_data |> getMemValue (offset + 20)

        d21 =
            z80_screen_data |> getMemValue (offset + 21)

        d22 =
            z80_screen_data |> getMemValue (offset + 22)

        d23 =
            z80_screen_data |> getMemValue (offset + 23)

        d24 =
            z80_screen_data |> getMemValue (offset + 24)

        d25 =
            z80_screen_data |> getMemValue (offset + 25)

        d26 =
            z80_screen_data |> getMemValue (offset + 26)

        d27 =
            z80_screen_data |> getMemValue (offset + 27)

        d28 =
            z80_screen_data |> getMemValue (offset + 28)

        d29 =
            z80_screen_data |> getMemValue (offset + 29)

        d30 =
            z80_screen_data |> getMemValue (offset + 30)

        d31 =
            z80_screen_data |> getMemValue (offset + 31)
    in
    Vector32.from32 d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31
