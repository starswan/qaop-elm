module Z80Screen exposing (..)

import Array exposing (Array)
import Byte exposing (Byte, getBit)
import Maybe
import ScreenStorage exposing (RawScreenData, Z80Screen, rawScreenData)
import SpectrumColour exposing (SpectrumColour, spectrumColour)
import Vector32 exposing (Vector32)
import Z80Byte exposing (Z80Byte, getBit6, getBit7, getBits210, getBits543)


type alias ScreenData =
    { colour : Z80Byte
    , data : List Z80Byte
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
    { length : Int
    , colour : SpectrumColour
    }


intToColour : Bool -> Z80Byte -> Bool -> SpectrumColour
intToColour globalFlash raw_colour bitValue =
    let
        --colour_byte =
        --    Byte.fromInt raw_colour
        bright =
            --colour_byte |> getBit 6
            raw_colour |> getBit6

        --
        flash =
            --colour_byte |> getBit 7
            raw_colour |> getBit7

        --(flash, bright) = case raw_colour |> Bitwise.and 0xC0 of
        --    0 -> (False, False)
        --    1 -> (False, True)
        --    2 -> (True, False)
        --    _ -> (True, True)
        value =
            if flash && globalFlash then
                not bitValue

            else
                bitValue

        colour =
            if value then
                -- ink
                --Bitwise.and raw_colour 0x07
                raw_colour |> getBits210

            else
                --paper
                --Bitwise.and raw_colour 0x38 |> shiftRightBy 3
                raw_colour |> getBits543
    in
    spectrumColour colour bright


pairToColour : Bool -> Z80Byte -> RunCount -> ScreenColourRun
pairToColour globalFlash raw_colour runcount =
    let
        colour =
            intToColour globalFlash raw_colour runcount.value
    in
    ScreenColourRun runcount.count colour


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


intToRcList : Z80Byte -> List RunCount
intToRcList index =
    runCounts0to255 |> Array.get (index |> Z80Byte.toInt) |> Maybe.withDefault []


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


toDrawn : Bool -> ScreenData -> List ScreenColourRun -> List ScreenColourRun
toDrawn globalFlash screendata linelist =
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


foldUp : RawScreenData -> List ScreenData -> List ScreenData
foldUp raw list =
    case list of
        head :: tail ->
            if head.colour == raw.colour then
                ScreenData raw.colour (raw.data :: head.data) :: tail

            else
                ScreenData raw.colour [ raw.data ] :: list

        _ ->
            [ ScreenData raw.colour [ raw.data ] ]


screenLines : Z80Screen -> List (List ScreenColourRun)
screenLines z80_screen =
    let
        foldDrawn =
            toDrawn z80_screen.flash
    in
    z80_screen
        |> rawScreenData
        |> List.map (\x -> x |> Vector32.foldr foldUp [])
        |> List.map (\x -> x |> List.foldr foldDrawn [])
