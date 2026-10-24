module Z80Ram exposing (..)

import Dict exposing (Dict)
import ScreenStorage exposing (Z80Screen, getScreenValue, setScreenValue)
import Z80MemoryDict exposing (Z80MemoryDict, getMemValue, mergeMemoryDict, setMemValue)


type alias Z80Ram =
    { screen : Z80Screen
    , ula_ram : Z80MemoryDict
    , bank2 : Z80MemoryDict
    , bank3 : Z80MemoryDict
    }


constructor : Z80Ram
constructor =
    let
        ula =
            List.repeat (16384 - 6912) 0 |> Z80MemoryDict.constructor

        ram2 =
            List.repeat 16384 0 |> Z80MemoryDict.constructor

        ram3 =
            List.repeat 16384 0 |> Z80MemoryDict.constructor
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
            z80ram.ula_ram |> getMemValue ram_addr

        else
            z80ram.screen |> getScreenValue addr

    else if addr < 32768 then
        z80ram.bank2 |> getMemValue (addr - 16384)

    else
        z80ram.bank3 |> getMemValue (addr - 32768)


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
            z80_ram.ula_ram |> mergeMemoryDict d1

        x2 =
            z80_ram.bank2 |> mergeMemoryDict d2

        x3 =
            z80_ram.bank3 |> mergeMemoryDict d3
    in
    { screen = x0, ula_ram = x1, bank2 = x2, bank3 = x3 }
