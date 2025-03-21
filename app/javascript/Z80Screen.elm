module Z80Screen exposing (..)

import Array exposing (Array)
import Bitwise exposing (shiftRightBy)
import Byte exposing (Byte, getBit)
import Maybe
import ScreenStorage exposing (RawScreenData, Z80Screen, rawScreenData)
import SpectrumColour exposing (SpectrumColour, spectrumColour)
import Vector32 exposing (Vector32)


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
    { start : Int
    , value : Bool
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


foldRunCounts : Bool -> List RunCount -> List RunCount
foldRunCounts item list =
    case list of
        runcount :: tail ->
            if item == runcount.value then
                RunCount runcount.start runcount.value (runcount.count + 1) :: tail

            else
                RunCount (runcount.start + runcount.count) item 1 :: list

        _ ->
            [ RunCount 0 item 1 ]



-- need to work out how to filter out lines of background colour
-- This isn't quite right - the colour is the attribute value
--single_line |> List.filter (\item -> item.colour /= z80env.border)
-- screenline is start length colour (0-7) and flash


type alias ScreenColourRun =
    { start : Int
    , length : Int
    , colour : SpectrumColour
    }


pairToColour : Bool -> Int -> RunCount -> ScreenColourRun
pairToColour globalFlash raw_colour runcount =
    let
        colour_byte =
            Byte.fromInt raw_colour

        bright =
            colour_byte |> getBit 6

        flash =
            colour_byte |> getBit 7

        ink =
            Bitwise.and raw_colour 0x07

        paper =
            Bitwise.and raw_colour 0x38 |> shiftRightBy 3

        ( fg, bg ) =
            if flash && globalFlash then
                ( paper, ink )

            else
                ( ink, paper )

        colour =
            if runcount.value then
                fg

            else
                bg
    in
    ScreenColourRun runcount.start runcount.count (spectrumColour colour bright)


toDrawn : Bool -> ScreenData -> List ScreenColourRun -> List ScreenColourRun
toDrawn globalFlash screendata linelist =
    let
        listBools : List Bool
        listBools =
            screendata.data
                |> List.map intToBools
                |> List.concat

        newList : List ScreenColourRun
        newList =
            listBools
                |> List.foldl foldRunCounts []
                |> List.reverse
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
        foldDrawn = toDrawn z80_screen.flash
    in
    z80_screen
        |> rawScreenData
        |> List.map (\x -> x |> Vector32.foldr foldUp [])
        |> List.map (\x -> x |> List.foldr foldDrawn [])
