module Z80Screen exposing (..)

import Array exposing (Array)
import Bitwise exposing (shiftRightBy)
import Byte exposing (Byte, getBit)
import Maybe
import ScreenStorage exposing (RawScreenData, ScreenLine, Z80Screen, memoryRow)
import SpectrumColour exposing (SpectrumColour, spectrumColour)
import Vector32 exposing (Vector32)
import Vector8 exposing (Vector8)


type alias ScreenData =
    { colour : Int
    , data : List Int
    }



-- line definition - length colour (3 bits) and brightness
-- ignore flash for now


isBitSet : Byte -> Int -> Bool
isBitSet value shift =
    getBit shift value


bitsToLines : Byte -> List Bool
bitsToLines datum =
    [ 7, 6, 5, 4, 3, 2, 1, 0 ] |> List.map (isBitSet datum)


type alias RunCount =
    { value : Bool
    , count : Int
    }


ints0to255 =
    List.range 0 255


bytes0to255 =
    ints0to255 |> List.map Byte.fromInt


intToBoolsCache : Array (List Bool)
intToBoolsCache =
    bytes0to255 |> List.map bitsToLines |> Array.fromList


intToBools : Int -> List Bool
intToBools index =
    intToBoolsCache |> Array.get index |> Maybe.withDefault []


foldBoolRunCounts : Bool -> List RunCount -> List RunCount
foldBoolRunCounts item list =
    case list of
        runcount :: tail ->
            if item == runcount.value then
                RunCount runcount.value (runcount.count + 1) :: tail

            else
                RunCount item 1 :: list

        _ ->
            [ RunCount item 1 ]



-- need to work out how to filter out lines of background colour
-- This isn't quite right - the colour is the attribute value
--single_line |> List.filter (\item -> item.colour /= z80env.border)
-- screenline is start length colour (0-7) and flash


type alias ScreenColourRun =
    { runcount : RunCount
    , colour : SpectrumColour
    }


pairToColour : Bool -> Int -> RunCount -> ScreenColourRun
pairToColour globalFlash raw_colour runcount =
    let
        -- This has been benchmarked as the fastest implementation
        ( flash, bright ) =
            case raw_colour |> Bitwise.and 0xC0 of
                0 ->
                    ( False, False )

                0x40 ->
                    ( False, True )

                0x80 ->
                    ( True, False )

                _ ->
                    ( True, True )

        value =
            if flash && globalFlash then
                not runcount.value

            else
                runcount.value

        colour_value =
            if value then
                -- ink
                Bitwise.and raw_colour 0x07

            else
                --paper
                Bitwise.and raw_colour 0x38 |> shiftRightBy 3

        colour =
            spectrumColour colour_value bright
    in
    ScreenColourRun runcount colour


runCounts0to255 : Array (List RunCount)
runCounts0to255 =
    -- lookup of data byte to [rc1, rc2, rc3]
    ints0to255
        |> List.map
            (\value ->
                value
                    |> intToBools
                    |> List.foldl foldBoolRunCounts []
                    |> List.reverse
            )
        |> Array.fromList


intToRcList : Int -> List RunCount
intToRcList index =
    runCounts0to255 |> Array.get index |> Maybe.withDefault []


foldRunCounts : RunCount -> List RunCount -> List RunCount
foldRunCounts item list =
    case list of
        runcount :: tail ->
            if item.value == runcount.value then
                RunCount runcount.value (runcount.count + item.count) :: tail

            else
                item :: list

        _ ->
            List.singleton item


mapScanLine : Bool -> Vector32 RawScreenData -> List ( Int, ScreenColourRun )
mapScanLine globalFlash v32 =
    v32
        |> Vector32.foldr
            (\raw list ->
                case list of
                    head :: tail ->
                        if head.colour == raw.colour then
                            ScreenData raw.colour (raw.data :: head.data) :: tail

                        else
                            ScreenData raw.colour [ raw.data ] :: list

                    _ ->
                        [ ScreenData raw.colour [ raw.data ] ]
            )
            []
        --    compacted list of ScreenData (colour + list of data bytes with that colour]
        |> List.foldr
            (\screendata linelist ->
                let
                    list2 : List RunCount
                    list2 =
                        screendata.data
                            |> List.map intToRcList
                            |> List.concat
                            |> List.foldr foldRunCounts []

                    newList : List ScreenColourRun
                    newList =
                        list2
                            |> List.map (pairToColour globalFlash screendata.colour)
                in
                newList ++ linelist
            )
            []
        --  list of pairs - add count to get the start position of the next item in the list
        |> List.foldl
            (\item list ->
                case list |> List.head of
                    Just ( head, headItem ) ->
                        ( head + headItem.runcount.count, item ) :: list

                    Nothing ->
                        List.singleton ( 0, item )
            )
            []
        -- spike - filter out the background nodes
        --|> List.filter (\( _, colourRun ) -> colourRun.runcount.value)
        |> List.reverse


mapScreenLine : Bool -> ScreenLine -> Vector8 (List ( Int, ScreenColourRun ))
mapScreenLine globalFlash screenLine =
    let
        rawData : Vector8 (Vector32 RawScreenData)
        rawData =
            Vector8.indices
                |> Vector8.map
                    (\dataIndex ->
                        let
                            dataRow =
                                memoryRow screenLine.data dataIndex

                            attr_row =
                                screenLine.attrs
                        in
                        Vector32.map2 (\data attr -> { colour = attr, data = data }) dataRow attr_row
                    )
    in
    rawData
        |> Vector8.map (mapScanLine globalFlash)
