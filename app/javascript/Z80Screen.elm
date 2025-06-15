module Z80Screen exposing (..)

import Array exposing (Array)
import Byte exposing (Byte, getBit)
import Maybe
import ScreenAttr exposing (ScreenAttr)
import ScreenStorage exposing (RawScreenData, Z80Screen, rawScreenData)
import SpectrumColour exposing (SpectrumColour, spectrumColour)
import Vector32 exposing (Vector32)
import Z80Debug exposing (debugLog, debugTodo)


type alias ScreenData =
    { colour : ScreenAttr
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



-- Array lookup 0-255 list runcount
-- need to work out how to filter out lines of background colour
-- This isn't quite right - the colour is the attribute value
--single_line |> List.filter (\item -> item.colour /= z80env.border)
-- screenline is start length colour (0-7) and flash


type alias ScreenColourRun =
    { length : Int
    , colour : SpectrumColour
    }


intToColour : Bool -> ScreenAttr -> Bool -> SpectrumColour
intToColour globalFlash attr bitValue =
    let
        --colour_byte =
        --    Byte.fromInt raw_colour
        --bright =
        --    colour_byte |> getBit 6
        --flash =
        --    colour_byte |> getBit 7
        --(flash, bright) = case raw_colour |> Bitwise.and 0xC0 of
        --    0 -> (False, False)
        --    1 -> (False, True)
        --    2 -> (True, False)
        --    _ -> (True, True)
        value =
            if attr.flash && globalFlash then
                not bitValue

            else
                bitValue

        colour =
            if value then
                -- ink
                attr.ink

            else
                --paper
                attr.paper
    in
    spectrumColour colour attr.bright


pairToColour : Bool -> ScreenAttr -> RunCount -> ScreenColourRun
pairToColour globalFlash raw_colour runcount =
    let
        colour =
            intToColour globalFlash raw_colour runcount.value
    in
    ScreenColourRun runcount.count colour



--runCounts0to255 : Array (List RunCount)
--runCounts0to255 =
--    ints0to255
--        |> List.map Byte.fromInt
--        |> List.map bitsToLines
--        |> List.map (\bits -> bits |> List.foldl foldBoolRunCounts [] |> List.reverse)
--        |> Array.fromList


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
    --runCounts0to255 |> Array.get index |> Maybe.withDefault (debugTodo "intToRcList" (index |> String.fromInt) [])
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



--screenLines : Z80Screen -> List (List ScreenColourRun)
--screenLines z80_screen =
--    let
--        globalFlash =
--            z80_screen.flash
--
--        --z =
--        --    debugLog "runCounts0to255 " (runCounts0to255 |> Array.length |> String.fromInt)
--        -- list of Vector32 (colour, data)
--        raw : List (Vector32 RawScreenData)
--        raw =
--            z80_screen
--                |> rawScreenData
--
--        pairFunc : ScreenAttr -> RunCount -> ScreenColourRun
--        pairFunc =
--            pairToColour globalFlash
--
--        --new : List (Vector32 (List ScreenColourRun))
--        new : List (List ScreenColourRun)
--        new =
--            raw
--                |> List.map
--                    (\vector ->
--                        let
--                            vecRcList : Vector32 (List ScreenColourRun)
--                            vecRcList =
--                                vector
--                                    |> Vector32.map
--                                        (\rawScreenData ->
--                                            let
--                                                rcList : List RunCount
--                                                rcList =
--                                                    runCounts0to255
--                                                        |> Array.get rawScreenData.data
--                                                        --|> Maybe.withDefault (debugTodo "runCounts0to255" (rawScreenData.data |> String.fromInt) [])
--                                                        |> Maybe.withDefault []
--
--                                                colourRuns : List ScreenColourRun
--                                                colourRuns =
--                                                    rcList |> List.map (\runCount -> pairFunc rawScreenData.colour runCount)
--                                            in
--                                            colourRuns
--                                        )
--                        in
--                        vecRcList |> Vector32.toList |> List.concat
--                    )
--    in
--    new
