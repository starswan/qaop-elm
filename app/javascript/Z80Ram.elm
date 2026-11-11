module Z80Ram exposing (..)

import Array exposing (Array)
import Bitwise exposing (shiftRightBy)
import Dict exposing (Dict)
import ScreenStorage exposing (Z80Screen, getScreenValue, setScreenValue)
import Set exposing (Set)
import Z80Debug exposing (debugTodo)


type alias Z80Ram =
    { screen : Z80Screen
    , ula_ram : Array (Array Int)
    , bank2 : Array (Array Int)
    , bank3 : Array (Array Int)
    }


constructor : Z80Ram
constructor =
    let
        --ula =
        --    List.repeat (16384 - 6912) 0 |> Z80MemoryDict.constructor
        ula =
            List.repeat 37 (List.repeat 256 0 |> Array.fromList) |> Array.fromList

        -- 64 arrays of 256 seems to give a pretty good performance profile
        ram2 =
            List.repeat 64 (List.repeat 256 0 |> Array.fromList) |> Array.fromList

        ram3 =
            List.repeat 64 (List.repeat 256 0 |> Array.fromList) |> Array.fromList
    in
    Z80Ram ScreenStorage.constructor ula ram2 ram3


getRamValue : Int -> Z80Ram -> Int
getRamValue addr z80ram =
    if addr < 16384 then
        let
            ram_addr =
                addr - 6912
        in
        if ram_addr >= 0 then
            --z80ram.ula_ram |> getMemValue ram_addr
            z80ram.ula_ram |> getDoubleArrayValue ram_addr

        else
            z80ram.screen |> getScreenValue addr

    else if addr < 32768 then
        z80ram.bank2 |> getDoubleArrayValue (addr - 16384)

    else
        z80ram.bank3 |> getDoubleArrayValue (addr - 32768)


getDoubleArrayValue : Int -> Array (Array Int) -> Int
getDoubleArrayValue addr memArray =
    let
        addr1 =
            addr |> shiftRightBy 8

        addr2 =
            addr |> Bitwise.and 0xFF
    in
    case Array.get addr1 memArray of
        Just array ->
            case array |> Array.get addr2 of
                Just a ->
                    a

                Nothing ->
                    debugTodo "getDoubleArrayValue" (String.fromInt addr1 ++ String.fromInt addr2) -1

        Nothing ->
            debugTodo "getDoubleArrayValue" (String.fromInt addr) -1


foldSingleRamBank : List ( Int, Int ) -> Dict Int Int -> Int -> Array (Array Int) -> Array (Array Int)
foldSingleRamBank d2 ramdict offset bank2 =
    let
        d2keys : Set Int
        d2keys =
            d2 |> List.map Tuple.first |> List.foldl (\addr set -> set |> Set.insert (addr |> shiftRightBy 8)) Set.empty
    in
    bank2
        |> Array.indexedMap
            (\index arrayItem ->
                let
                    result =
                        if d2keys |> Set.member index then
                            arrayItem
                                |> Array.indexedMap
                                    (\subIndex item ->
                                        ramdict |> Dict.get (offset + index * 256 + subIndex) |> Maybe.withDefault item
                                    )

                        else
                            arrayItem
                in
                result
            )


foldDictIntoRam : Dict Int Int -> Z80Ram -> Z80Ram
foldDictIntoRam ramdict z80_ram =
    let
        ( ( d0, d1 ), d2, d3 ) =
            ramdict
                |> Dict.foldl
                    (\addr value ( ( t0, t1 ), t2, t3 ) ->
                        if addr < 16384 then
                            let
                                ram_addr =
                                    addr - 6912
                            in
                            if ram_addr >= 0 then
                                ( ( t0, ( ram_addr, value ) :: t1 ), t2, t3 )

                            else
                                ( ( ( addr, value ) :: t0, t1 ), t2, t3 )

                        else if addr < 32768 then
                            ( ( t0, t1 ), ( addr - 16384, value ) :: t2, t3 )

                        else
                            ( ( t0, t1 ), t2, ( addr - 32768, value ) :: t3 )
                    )
                    ( ( [], [] ), [], [] )

        x0 =
            d0
                |> List.foldl
                    (\( addr, value ) screen ->
                        screen |> setScreenValue addr value
                    )
                    z80_ram.screen

        x1 =
            z80_ram.ula_ram |> foldSingleRamBank d1 ramdict 6912

        bank2 =
            z80_ram.bank2
                |> foldSingleRamBank d2 ramdict 16384

        x3 =
            z80_ram.bank3 |> foldSingleRamBank d3 ramdict 32768
    in
    { screen = x0, ula_ram = x1, bank2 = bank2, bank3 = x3 }
