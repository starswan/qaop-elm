module Z80Screen exposing (..)

import Array exposing (Array)
import Bitwise exposing (shiftLeftBy, shiftRightBy)
import List.Extra as LE
import Maybe
import ScreenStorage exposing (RawScreenData, ScreenLine, Z80Screen, memoryRow)
import SpectrumColour exposing (SpectrumColour, spectrumColour)
import Vector32 exposing (Vector32)
import Vector7
import Vector8 exposing (Vector8)


type alias ScreenData =
    { colour : Int
    , data : List Int
    }


bitsToLines : Int -> Vector8 Bool
bitsToLines datum =
    Vector8.initializeFromInt (\index -> 1 |> shiftLeftBy (7 - index))
        |> Vector8.map (\mask -> (mask |> Bitwise.and datum) /= 0)


type alias RunCount =
    { value : Bool
    , count : Int
    }


type alias RunCountList =
    { initialValue : Bool
    , firstCount : Int
    , counts : List Int
    }



-- need to work out how to filter out lines of background colour
-- This isn't quite right - the colour is the attribute value
--single_line |> List.filter (\item -> item.colour /= z80env.border)
-- screenline is start length colour (0-7) and flash


type alias ScreenColourRun =
    { count : Int
    , colour : SpectrumColour
    }


pairToColour : Bool -> Int -> Bool -> Int -> ScreenColourRun
pairToColour globalFlash raw_colour rcvalue rccount =
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
                not rcvalue

            else
                rcvalue

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
    ScreenColourRun rccount colour


runCounts0to255 : Array RunCountList
runCounts0to255 =
    -- lookup of data byte to [rc1, rc2, rc3]
    List.range 0 255
        |> List.map
            (\value ->
                value
                    |> bitsToLines
                    |> (\v8 ->
                            let
                                ( item, vec7 ) =
                                    v8 |> Vector8.uncons
                            in
                            ( item, vec7 )
                       )
                    --|> Vector8.toList
                    |> (\( boolitem, v7list ) ->
                            let
                                v7group : List ( Bool, List Bool )
                                v7group =
                                    v7list |> Vector7.toList |> LE.group
                            in
                            case v7group of
                                [] ->
                                    -- degenerate case, can't happen as 7 list can never be empty
                                    ( ( boolitem, [] ), [] )

                                ( first, flist ) :: rest ->
                                    if boolitem == first then
                                        ( ( boolitem, boolitem :: flist ), rest )

                                    else
                                        ( ( boolitem, [] ), v7group )
                       )
                    --   ((Bool, List Bool), List (Bool, List Bool))
                    --|> LE.group
                    -- This should be madelled as a non-empty list somehow
                    --|> List.map (\( first, rest ) -> RunCount first (1 + (rest |> List.length)))
                    |> (\( ( bhead, blist ), listbpair ) ->
                            let
                                x =
                                    listbpair |> List.map (\( bool, listbool ) -> RunCount bool (1 + (listbool |> List.length)))
                            in
                            RunCountList bhead (1 + (blist |> List.length)) (x |> List.map (\rc -> rc.count))
                       )
            )
        |> Array.fromList


intToRcList : Int -> RunCountList
intToRcList index =
    runCounts0to255 |> Array.get index |> Maybe.withDefault (RunCountList True 0 [])


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
        --    This version looks tidy but is way too slow (20.4Hz vs 24.1Hz)
        --|> Vector32.toList
        --|> LE.groupWhile (\left right -> left.colour == right.colour)
        --|> List.map
        --    (\( head, list ) ->
        --        let
        --            data =
        --                list |> List.map (\x -> x.data)
        --        in
        --        ScreenData head.colour (head.data :: data)
        --    )
        --    compacted list of ScreenData (colour + list of data bytes with that colour]
        |> List.foldr
            (\screendata linelist ->
                let
                    list1 : List RunCountList
                    list1 =
                        screendata.data
                            |> List.map intToRcList
                            |> List.foldr
                                (\item list ->
                                    case list of
                                        runcount :: tail ->
                                            let
                                                lastValue =
                                                    if (item.counts |> List.length |> modBy 2) /= 0 then
                                                        item.initialValue

                                                    else
                                                        item.initialValue |> not
                                            in
                                            if lastValue == runcount.initialValue then
                                                let
                                                    newhead =
                                                        RunCountList item.initialValue item.firstCount (item.counts ++ [ runcount.firstCount ] ++ runcount.counts)
                                                in
                                                newhead :: tail

                                            else
                                                item :: list

                                        _ ->
                                            List.singleton item
                                )
                                []

                    newList : List ScreenColourRun
                    newList =
                        list1
                            |> List.map
                                (\rclist ->
                                    let
                                        head =
                                            pairToColour globalFlash screendata.colour rclist.initialValue rclist.firstCount

                                        tail =
                                            rclist.counts
                                                |> List.foldr
                                                    (\item ( bool, list ) ->
                                                        let
                                                            new =
                                                                if bool then
                                                                    pairToColour globalFlash screendata.colour rclist.initialValue item

                                                                else
                                                                    pairToColour globalFlash screendata.colour (rclist.initialValue |> not) item
                                                        in
                                                        ( bool |> not, new :: list )
                                                    )
                                                    ( rclist.initialValue |> not, [] )
                                                |> Tuple.second
                                    in
                                    head :: tail
                                )
                            |> List.concat
                in
                newList ++ linelist
            )
            []
        --  list of pairs - add count to get the start position of the next item in the list
        |> List.foldl
            (\item list ->
                case list |> List.head of
                    Just ( head, headItem ) ->
                        ( head + headItem.count, item ) :: list

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
