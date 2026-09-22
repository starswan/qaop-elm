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


emptyRamDict =
    { ulaDict = Dict.empty, bank2Dict = Dict.empty, bank3Dict = Dict.empty }


ramDictInsert : Int -> Int -> RamDict -> RamDict
ramDictInsert addr value dict =
    if addr < 16384 then
        { dict | ulaDict = dict.ulaDict |> Dict.insert addr value }

    else if addr < 32768 then
        { dict | bank2Dict = dict.bank2Dict |> Dict.insert (addr - 16384) value }

    else
        { dict | bank3Dict = dict.bank3Dict |> Dict.insert (addr - 32768) value }


ramDictGet : Int -> RamDict -> Maybe Int
ramDictGet addr dict =
    if addr < 16384 then
        dict.ulaDict |> Dict.get addr

    else if addr < 32768 then
        dict.bank2Dict |> Dict.get (addr - 16384)

    else
        dict.bank3Dict |> Dict.get (addr - 32768)


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
                    (\addr value ramthing ->
                        let
                            ram_addr =
                                addr - 6912
                        in
                        if ram_addr >= 0 then
                            { ramthing | ula_ram = ramthing.ula_ram |> setMemValue ram_addr value }

                        else
                            { ramthing | screen = ramthing.screen |> setScreenValue addr value }
                    )
                    { ula_ram = z80_ram.ula_ram, screen = z80_ram.screen }

        newBank2 =
            ramdict.bank2Dict
                |> Dict.foldl
                    (\addr value rambank ->
                        rambank |> setMemValue addr value
                    )
                    z80_ram.bank2

        newBank3 =
            ramdict.bank3Dict
                |> Dict.foldl
                    (\addr value rambank -> rambank |> setMemValue addr value)
                    z80_ram.bank3
    in
    { screen = newUla.screen, ula_ram = newUla.ula_ram, bank2 = newBank2, bank3 = newBank3 }
