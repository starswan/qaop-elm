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
    { length : Int
    , colour : SpectrumColour
    }


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


pairToColour : Bool -> Int -> RunCount -> ScreenColourRun
pairToColour globalFlash raw_colour runcount =
    let
        colour_byte =
            Byte.fromInt raw_colour

        bright =
            colour_byte |> getBit 6

        flash =
            colour_byte |> getBit 7

        value =
            if flash && globalFlash then
                not runcount.value

            else
                runcount.value

        colour =
            if value then
                -- ink
                Bitwise.and raw_colour 0x07

            else
                --paper
                Bitwise.and raw_colour 0x38 |> shiftRightBy 3
    in
    ScreenColourRun runcount.count (spectrumColour colour bright)


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



--(plainColourRuns, flashColourRuns) : Array ( Array (List ScreenColourRun), Array (List ScreenColourRun) )


plainColourRuns : Array (Array (List ScreenColourRun))
plainColourRuns =
    -- 1st lookup is for global flash off, the other for global flash on
    -- Array (data) -> Array (colour) -> (List ScreenColourRun)
    ints0to255
        |> List.map
            (\colourValue ->
                let
                    plainColour : Array (List ScreenColourRun)
                    plainColour =
                        makeColourRun False colourValue
                in
                plainColour
            )
        |> Array.fromList


flashColourRuns : Array (Array (List ScreenColourRun))
flashColourRuns =
    -- 1st lookup is for global flash off, the other for global flash on
    -- Array (data) -> Array (colour) -> (List ScreenColourRun)
    ints0to255
        |> List.map
            (\colourValue ->
                let
                    flashColour : Array (List ScreenColourRun)
                    flashColour =
                        makeColourRun True colourValue
                in
                flashColour
            )
        |> Array.fromList



--lookupColour : Bool -> Int -> RunCount -> ScreenColourRun
--lookupColour globalFlash raw_colour runcount =
--    let
--        lookup = if globalFlash then
--                    flashColourRuns
--                 else
--                    plainColourRuns
--        rcList = lookup |> Array.get raw_colour |> Maybe.withDefault ([] |> Array.fromList)
--    in
--


toDrawn : Bool -> ScreenData -> List ScreenColourRun -> List ScreenColourRun
toDrawn globalFlash screendata linelist =
    let
        list2 : List RunCount
        list2 =
            screendata.data
                |> List.map intToRcList
                |> List.concat
                |> List.foldl foldRunCounts []
                |> List.reverse

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


mapScreenLine : Bool -> ScreenLine -> Vector8 (List ( Int, ScreenColourRun ))
mapScreenLine globalFlash screenLine =
    let
        foldDrawn : ScreenData -> List ScreenColourRun -> List ScreenColourRun
        foldDrawn =
            toDrawn globalFlash

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

        --foldedLines : Vector8 (List ScreenData)
        --foldedLines =
        --    rawData |> Vector8.map (\v32 -> v32 |> Vector32.foldr foldUp [])
        --drawnFolded : Vector8 (List ScreenColourRun)
        --drawnFolded =
        --    rawData
        --    |> Vector8.map (\v32 -> v32 |> Vector32.foldr foldUp []) |> Vector8.map (\v32 -> v32 |> List.foldr foldDrawn [])
        --scrFolded : Vector8 (List ( Int, ScreenColourRun ))
        --scrFolded =
        --    rawData
        --        |> Vector8.map (\v32 -> v32 |> Vector32.foldr foldUp [])
        --        |> Vector8.map (\v32 -> v32 |> List.foldr foldDrawn [])
        --        |> Vector8.map (\scrList -> scrList |> List.foldl foldScr [] |> List.reverse)
        scrFolded : Vector8 (List ( Int, ScreenColourRun ))
        scrFolded =
            rawData
                |> Vector8.map
                    (\v32 ->
                        v32
                            |> Vector32.foldr foldUp []
                            |> List.foldr foldDrawn []
                            |> List.foldl foldScr []
                            |> List.reverse
                    )
    in
    scrFolded


foldScr : ScreenColourRun -> List ( Int, ScreenColourRun ) -> List ( Int, ScreenColourRun )
foldScr item list =
    case list |> List.head of
        Just ( head, headItem ) ->
            ( head + headItem.length, item ) :: list

        Nothing ->
            List.singleton ( 0, item )
