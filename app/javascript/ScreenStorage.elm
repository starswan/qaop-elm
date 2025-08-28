module ScreenStorage exposing (..)

-- Convert row index into start row data location

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Dict exposing (Dict)
import SpectrumColour exposing (BorderColour(..))
import Vector24 exposing (Vector24)
import Vector32 exposing (Vector32)
import Vector8



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


attr_indexes : List Vector24.Index
attr_indexes =
    --bank0_attr_indexes ++ bank1_attr_indexes ++ bank2_attr_indexes
    Vector24.indices |> Vector24.map (\ind24 -> List.repeat 8 ind24) |> Vector24.toList |> List.concat


type alias Z80Screen =
    { data : Dict Int (Vector32 Int)
    , attrs : Vector24 (Vector32 Int)
    , border : BorderColour
    , flash : Bool

    --refrs_a: Int,
    --refrs_b: Int,
    --refrs_t: Int,
    --refrs_s: Int
    }



-- colour data is bit 7 flash, bit 6 bright, bits 5-3 paper, bits 2-0 ink


type alias RawScreenData =
    { colour : Int
    , data : Int
    }


constructor : Z80Screen
constructor =
    let
        dataLine =
            Vector32.repeat 0

        screen_data =
            List.repeat 192 dataLine |> List.indexedMap Tuple.pair |> Dict.fromList

        --for(int i=6144;i<6912;i++) ram[i] = 070; // white
        attr_line =
            Vector32.repeat 0x38

        attributes =
            Vector24.repeat attr_line
    in
    Z80Screen screen_data attributes BorderWhite False


calcDataOffset : Int -> Int
calcDataOffset start =
    -- given an offset 0-191, return the start screen address
    -- for the data row 0-191 (which needs multiplying by 32 to get an actual address)
    let
        bankStart =
            start |> Bitwise.and 0xC0

        bankOffset =
            start |> Bitwise.and 0x3F |> shiftRightBy 3

        data_offset =
            start |> Bitwise.and 0x07 |> shiftLeftBy 3
    in
    bankStart + bankOffset + data_offset


type alias VectorDataOffset =
    { index24 : Vector24.Index
    , index8 : Vector8.Index
    }


calcVectorDataOffset : Int -> VectorDataOffset
calcVectorDataOffset int =
    let
        dataOffset =
            calcDataOffset int

        rowIndex =
            dataOffset // 8 |> Vector24.intToIndex |> Maybe.withDefault Vector24.Index0

        dataIndex =
            (dataOffset |> remainderBy 8) |> Vector8.intToIndex |> Maybe.withDefault Vector8.Index0
    in
    VectorDataOffset rowIndex dataIndex


screenOffsets : List ( VectorDataOffset, Vector24.Index )
screenOffsets =
    attr_indexes |> List.indexedMap (\index attr_index -> ( calcVectorDataOffset index, attr_index ))



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
setScreenValue addr value z80_screen =
    if addr < 0x1800 then
        let
            base =
                addr // 32

            offset =
                addr |> remainderBy 32 |> Vector32.intToIndex |> Maybe.withDefault Vector32.Index0

            newData =
                z80_screen.data |> Dict.get base |> Maybe.withDefault (Vector32.repeat 0) |> Vector32.set offset value
        in
        { z80_screen | data = z80_screen.data |> Dict.insert base newData }

    else
        let
            offset =
                addr - 0x1800

            rowIndex =
                offset // 32 |> Vector24.intToIndex |> Maybe.withDefault Vector24.Index0

            old_row =
                z80_screen.attrs |> Vector24.get rowIndex

            col =
                offset |> remainderBy 32 |> Vector32.intToIndex |> Maybe.withDefault Vector32.Index0

            new_row =
                old_row |> Vector32.set col value
        in
        { z80_screen | attrs = z80_screen.attrs |> Vector24.set rowIndex new_row }


getScreenValue : Int -> Z80Screen -> Int
getScreenValue addr z80_screen =
    if addr < 0x1800 then
        let
            base =
                addr // 32

            offset =
                addr |> remainderBy 32 |> Vector32.intToIndex |> Maybe.withDefault Vector32.Index0
        in
        z80_screen.data |> Dict.get base |> Maybe.withDefault (Vector32.repeat 0) |> Vector32.get offset

    else
        let
            offset =
                addr - 0x1800

            rowIndex =
                offset // 32 |> Vector24.intToIndex |> Maybe.withDefault Vector24.Index0

            oldrow =
                z80_screen.attrs |> Vector24.get rowIndex

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


memoryRow : Z80Screen -> VectorDataOffset -> Vector32 Int
memoryRow z80_screen screenOffset =
    let
        row_index =
            (screenOffset.index8 |> Vector8.indexToInt) + 8 * (screenOffset.index24 |> Vector24.indexToInt)

        data_row =
            z80_screen.data |> Dict.get row_index |> Maybe.withDefault (Vector32.repeat 0)
    in
    data_row
