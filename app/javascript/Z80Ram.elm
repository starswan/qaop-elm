module Z80Ram exposing (..)

import Dict exposing (Dict)
import ScreenStorage exposing (Z80Screen, getScreenValue, setScreenValue)
import Z80MemoryDict exposing (Z80MemoryDict, getMemValue, setMemValue)


type alias Z80Ram =
    { screen : Z80Screen
    , ula_ram : Z80MemoryDict
    , bank2 : Z80MemoryDict
    , bank3 : Z80MemoryDict
    }


type alias RamDict =
    { ulaDict : Dict Int Int
    , bank2Dict : Dict Int Int
    , bank3Dict : Dict Int Int
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


foldDictIntoRam : RamDict -> Z80Ram -> Z80Ram
foldDictIntoRam ramdict z80_ram =
    let
        newUla =
            ramdict.ulaDict
                |> Dict.foldl
                    (\addr value z80ram ->
                        if addr < 16384 then
                            let
                                ram_addr =
                                    addr - 6912
                            in
                            if ram_addr >= 0 then
                                { z80ram | ula_ram = z80ram.ula_ram |> setMemValue ram_addr value }

                            else
                                { z80ram | screen = z80ram.screen |> setScreenValue addr value }

                        else if addr < 32768 then
                            { z80ram | bank2 = z80ram.bank2 |> setMemValue (addr - 16384) value }

                        else
                            { z80ram | bank3 = z80ram.bank3 |> setMemValue (addr - 32768) value }
                    )
                    z80_ram

        newBank2 =
            ramdict.bank2Dict
                |> Dict.foldl
                    (\addr value z80ram ->
                        if addr < 16384 then
                            let
                                ram_addr =
                                    addr - 6912
                            in
                            if ram_addr >= 0 then
                                { z80ram | ula_ram = z80ram.ula_ram |> setMemValue ram_addr value }

                            else
                                { z80ram | screen = z80ram.screen |> setScreenValue addr value }

                        else if addr < 32768 then
                            { z80ram | bank2 = z80ram.bank2 |> setMemValue (addr - 16384) value }

                        else
                            { z80ram | bank3 = z80ram.bank3 |> setMemValue (addr - 32768) value }
                    )
                    z80_ram

        newBank3 =
            ramdict.bank3Dict
                |> Dict.foldl
                    (\addr value z80ram ->
                        if addr < 16384 then
                            let
                                ram_addr =
                                    addr - 6912
                            in
                            if ram_addr >= 0 then
                                { z80ram | ula_ram = z80ram.ula_ram |> setMemValue ram_addr value }

                            else
                                { z80ram | screen = z80ram.screen |> setScreenValue addr value }

                        else if addr < 32768 then
                            { z80ram | bank2 = z80ram.bank2 |> setMemValue (addr - 16384) value }

                        else
                            { z80ram | bank3 = z80ram.bank3 |> setMemValue (addr - 32768) value }
                    )
                    z80_ram
    in
    { ulaDict = newUla, bank2Dict = newBank2, bank3Dict = newBank3 }
