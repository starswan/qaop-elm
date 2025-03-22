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
    , groupedPixelData : List Int
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



--intsToBools : List Int -> List Bool
--intsToBools data =
--    data
--        |> List.map (\index -> Array.get index intToBoolsCache)
--        |> List.map (Maybe.withDefault [])
--        |> List.concat


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

        value =
            if flash && globalFlash then
                not runcount.value

            else
                runcount.value

        colour =
            if value then
                ink

            else
                paper
    in
    ScreenColourRun runcount.start runcount.count (spectrumColour colour bright)


runCounts0to255 : Array (List RunCount)
runCounts0to255 =
    -- lookup of data byte to [rc1, rc2, rc3]
    ints0to255
        |> List.map
            (\value ->
                value
                    |> intToBools
                    |> List.foldr foldRunCounts []
                    |> List.reverse
            )
        |> Array.fromList


makeColourRun : Bool -> Int -> Array (List ScreenColourRun)
makeColourRun globalFlash colourValue =
    ints0to255
        |> List.map
            (\dataValue ->
                let
                    runCountList : List RunCount
                    runCountList =
                        runCounts0to255 |> Array.get dataValue |> Maybe.withDefault []
                in
                runCountList
                    |> List.map (\rc -> pairToColour globalFlash colourValue rc)
            )
        |> Array.fromList


allColourRunsLookup : Array ( Array (List ScreenColourRun), Array (List ScreenColourRun) )
allColourRunsLookup =
    -- 1st lookup is for global flash off, the other for global flash on
    -- Array (data) -> Array (colour) -> (List ScreenColourRun)
    ints0to255
        |> List.map
            (\colourValue ->
                let
                    plainColourRuns : Array (List ScreenColourRun)
                    plainColourRuns =
                        makeColourRun False colourValue

                    flashColourRuns : Array (List ScreenColourRun)
                    flashColourRuns =
                        makeColourRun True colourValue
                in
                ( plainColourRuns, flashColourRuns )
            )
        |> Array.fromList



--toDrawn : ScreenData -> List ScreenColourRun -> List ScreenColourRun
--toDrawn screendata linelist =
--    let
--        newList : List ScreenColourRun
--        newList =
--            screendata.data
--                |> intsToBools
--                |> List.foldl foldRunCounts []
--                |> List.reverse
--                |> List.map (pairToColour screendata.colour)
--    in
--    newList ++ linelist


toDrawn : Bool -> ScreenData -> List ScreenColourRun -> List ScreenColourRun
toDrawn globalFlash screendata linelist =
    let
        listlistBools : List (List Bool)
        listlistBools =
            screendata.groupedPixelData
                |> List.map intToBools

        rcList2 : List RunCount
        rcList2 =
            listlistBools
                |> List.map
                    (\listbool8 ->
                        listbool8
                            |> List.foldl foldRunCounts []
                            |> List.reverse
                    )
                |> List.concat

        listBools : List Bool
        listBools =
            listlistBools
                |> List.concat

        rcList : List RunCount
        rcList =
            listBools
                |> List.foldl foldRunCounts []
                |> List.reverse

        newList : List ScreenColourRun
        newList =
            rcList |> List.map (pairToColour globalFlash screendata.colour)
    in
    newList ++ linelist


foldUp : RawScreenData -> List ScreenData -> List ScreenData
foldUp raw list =
    case list of
        head :: tail ->
            if head.colour == raw.colour then
                ScreenData raw.colour (raw.data :: head.groupedPixelData) :: tail

            else
                ScreenData raw.colour [ raw.data ] :: list

        _ ->
            [ ScreenData raw.colour [ raw.data ] ]


rawToColourRun : Bool -> RawScreenData -> List ScreenColourRun
rawToColourRun globalFlash rawData =
    let
        runCount =
            runCounts0to255 |> Array.get rawData.data
    in
    case runCount of
        Just rcList ->
            rcList |> List.map (\rc -> pairToColour globalFlash rawData.colour rc)

        Nothing ->
            []


screenLines : Z80Screen -> List (List ScreenColourRun)
screenLines z80_screen =
    let
        raw : List (Vector32 RawScreenData)
        raw =
            z80_screen |> rawScreenData

        foldups : List (List ScreenData)
        foldups =
            raw |> List.map (\x -> x |> Vector32.foldl foldUp [])

        drawn =
            foldups |> List.map (\x -> x |> List.foldl (toDrawn z80_screen.flash) [])

        -- This seems to work, but only does the left hand column
        --drawn2 = raw |> List.map (\x -> x |> Vector32.foldl (toDrawn2 z80_screen.flash) [])
        --pairList : List (Vector32 RawScreenData)
        --pairList =
        --    screenOffsets
        --        |> List.map
        --            (\( row_index, attr_index ) ->
        --                let
        --                    data_row =
        --                        range031 |> List.map (\index -> z80_screen.data |> getMemValue (row_index * 32 + index)) |> Vector32.fromListWithDefault 0 |> Tuple.second
        --
        --                    attr_row =
        --                       z80_screen.attrs |> Vector24.get attr_index
        --                in
        --                Vector32.map2 (\data attr -> { colour = attr, data = data }) data_row attr_row
        --            )
        --runs : List (Vector32 (List ScreenColourRun))
        --runs = raw |> List.map (\pair32 -> pair32 |> Vector32.map (\rawdata -> rawToColourRun z80_screen.flash rawdata))
        --squeeze: List (List ScreenColourRun)
        --squeeze = runs |> List.map (\vec32 -> vec32 |> Vector32.foldl (\item list  -> item ++ list) [] |> List.reverse)
        --squeeze = runs |> List.map (\vec32 -> vec32 |> Vector32.foldl (\item list -> list ++ item) [])
        --squeeze = runs |> List.map (\vec32 -> vec32 |> Vector32.toList |> List.concat)
    in
    --squeeze
    --drawn2
    drawn



-- rawScreenData produces List (Vector32 RawScreenData)
--rawScreenData : Z80Screen -> List (Vector32 RawScreenData)
--rawScreenData z80_screen =
--    screenOffsets
--        |> List.map
--            (\( row_index, attr_index ) ->
--                --let
--                --    x =
--                --        range031 |> List.map (mapScreen row_column z80_screen)
--                --in
--                --x
--                let
--                    data_row =
--                        range031 |> List.map (\index -> z80_screen.data |> getMemValue (row_index * 32 + index)) |> Vector32.fromListWithDefault 0 |> Tuple.second
--
--                    --attr_row =
--                    --    range031 |> List.map (\index -> z80_screen.attrs |> getMemValue (attr_index * 32 + index))
--                    attr_row =
--                        z80_screen.attrs |> Vector24.get attr_index
--
--                    --rows =
--                    --    List.extra.zip data_row attr_row
--                in
--                --rows |> List.map (\( data, attr ) -> { colour = attr, data = data })
--                Vector32.map2 (\data attr -> { colour = attr, data = data }) data_row attr_row
--            )
--
