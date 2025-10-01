module ScreenStorage exposing (..)

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import SpectrumColour exposing (BorderColour(..))
import Vector24 exposing (Vector24)
import Vector32 exposing (Vector32)
import Vector8 exposing (Vector8)
import Z80Memory exposing (Z80Memory, getMemValue)


type alias ScreenLine =
    { data : Z80Memory
    , attrs : Vector32 Int
    }


type alias Z80Screen =
    { lines : Vector24 ScreenLine
    , border : BorderColour

    --, flash : Bool
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
        screen_line =
            List.repeat 256 0 |> Z80Memory.constructor

        --for(int i=6144;i<6912;i++) ram[i] = 070; // white
        attr_line =
            Vector32.repeat 0x38

        line =
            { attrs = attr_line, data = screen_line }
    in
    --Z80Screen (Vector24.repeat line) BorderWhite False
    Z80Screen (Vector24.repeat line) BorderWhite


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


getScreenDataOffset : Int -> ( Vector24.Index, Vector8.Index, Vector32.Index )
getScreenDataOffset addr =
    let
        ( lineNumber, addr32, line32 ) =
            getScreenDataIndex addr

        lineOffset =
            (lineNumber |> Vector24.indexToInt) * 8 + (addr32 |> Vector8.indexToInt)

        dataOffset =
            calcVectorDataOffset lineOffset
    in
    ( dataOffset.index24, dataOffset.index8, line32 )


getScreenDataIndex : Int -> ( Vector24.Index, Vector8.Index, Vector32.Index )
getScreenDataIndex addr =
    let
        lineNumber =
            addr // 256 |> Vector24.intToIndex |> Maybe.withDefault Vector24.Index0

        lineOffset =
            addr |> remainderBy 256

        addr32 =
            lineOffset // 32 |> Vector8.intToIndex |> Maybe.withDefault Vector8.Index0

        line32 =
            lineOffset |> remainderBy 32 |> Vector32.intToIndex |> Maybe.withDefault Vector32.Index0
    in
    ( lineNumber, addr32, line32 )


setScreenValue : Int -> Int -> Z80Screen -> Z80Screen
setScreenValue addr value z80screen =
    if addr < 0x1800 then
        let
            ( lineNumber, addr32, line32 ) =
                getScreenDataOffset addr

            lineOffset =
                (addr32 |> Vector8.indexToInt) * 32 + (line32 |> Vector32.indexToInt)

            line =
                z80screen.lines |> Vector24.get lineNumber

            newLine =
                { line | data = line.data |> Z80Memory.setMemValue lineOffset value }

            newData =
                z80screen.lines |> Vector24.set lineNumber newLine
        in
        { z80screen | lines = newData }

    else
        let
            offset =
                addr - 0x1800

            row =
                offset // 32 |> Vector24.intToIndex |> Maybe.withDefault Vector24.Index0

            oldrow =
                z80screen.lines |> Vector24.get row

            col =
                offset |> remainderBy 32 |> Vector32.intToIndex |> Maybe.withDefault Vector32.Index0

            new_row =
                { oldrow | attrs = oldrow.attrs |> Vector32.set col value }
        in
        { z80screen | lines = z80screen.lines |> Vector24.set row new_row }


getScreenValue : Int -> Z80Screen -> Int
getScreenValue addr z80screen =
    if addr < 0x1800 then
        let
            ( lineNumber, addr32, line32 ) =
                getScreenDataOffset addr

            lineOffset =
                (addr32 |> Vector8.indexToInt) * 32 + (line32 |> Vector32.indexToInt)

            line =
                z80screen.lines |> Vector24.get lineNumber
        in
        line.data |> Z80Memory.getMemValue lineOffset

    else
        let
            offset =
                addr - 0x1800

            row =
                offset // 32 |> Vector24.intToIndex |> Maybe.withDefault Vector24.Index0

            oldrow =
                z80screen.lines |> Vector24.get row

            col =
                offset |> remainderBy 32 |> Vector32.intToIndex |> Maybe.withDefault Vector32.Index0
        in
        oldrow.attrs |> Vector32.get col


memoryRow : Z80Memory -> Vector8.Index -> Vector32 Int
memoryRow screenLine index8 =
    let
        dataOffset =
            (index8 |> Vector8.indexToInt) * 32

        d0 =
            screenLine |> getMemValue (dataOffset + 0)

        d1 =
            screenLine |> getMemValue (dataOffset + 1)

        d2 =
            screenLine |> getMemValue (dataOffset + 2)

        d3 =
            screenLine |> getMemValue (dataOffset + 3)

        d4 =
            screenLine |> getMemValue (dataOffset + 4)

        d5 =
            screenLine |> getMemValue (dataOffset + 5)

        d6 =
            screenLine |> getMemValue (dataOffset + 6)

        d7 =
            screenLine |> getMemValue (dataOffset + 7)

        d8 =
            screenLine |> getMemValue (dataOffset + 8)

        d9 =
            screenLine |> getMemValue (dataOffset + 9)

        d10 =
            screenLine |> getMemValue (dataOffset + 10)

        d11 =
            screenLine |> getMemValue (dataOffset + 11)

        d12 =
            screenLine |> getMemValue (dataOffset + 12)

        d13 =
            screenLine |> getMemValue (dataOffset + 13)

        d14 =
            screenLine |> getMemValue (dataOffset + 14)

        d15 =
            screenLine |> getMemValue (dataOffset + 15)

        d16 =
            screenLine |> getMemValue (dataOffset + 16)

        d17 =
            screenLine |> getMemValue (dataOffset + 17)

        d18 =
            screenLine |> getMemValue (dataOffset + 18)

        d19 =
            screenLine |> getMemValue (dataOffset + 19)

        d20 =
            screenLine |> getMemValue (dataOffset + 20)

        d21 =
            screenLine |> getMemValue (dataOffset + 21)

        d22 =
            screenLine |> getMemValue (dataOffset + 22)

        d23 =
            screenLine |> getMemValue (dataOffset + 23)

        d24 =
            screenLine |> getMemValue (dataOffset + 24)

        d25 =
            screenLine |> getMemValue (dataOffset + 25)

        d26 =
            screenLine |> getMemValue (dataOffset + 26)

        d27 =
            screenLine |> getMemValue (dataOffset + 27)

        d28 =
            screenLine |> getMemValue (dataOffset + 28)

        d29 =
            screenLine |> getMemValue (dataOffset + 29)

        d30 =
            screenLine |> getMemValue (dataOffset + 30)

        d31 =
            screenLine |> getMemValue (dataOffset + 31)

        data_row =
            Vector32.from32 d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31
    in
    data_row
