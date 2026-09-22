module Z80Ram exposing (..)

import Array exposing (Array)
import Bitwise exposing (shiftRightBy)
import Dict exposing (Dict)
import ScreenStorage exposing (Z80Screen, getScreenValue, setScreenValue)
import Z80Debug exposing (debugTodo)
import Z80MemoryDict exposing (Z80MemoryDict, getMemValue, setMemValue)


type alias Z80Ram =
    { screen : Z80Screen
    , ula_ram : Z80MemoryDict
    , bank2 : Array (Array Int)
    , bank3 : Array (Array Int)
    }


constructor : Z80Ram
constructor =
    let
        ula =
            List.repeat (16384 - 6912) 0 |> Z80MemoryDict.constructor

        ram2 =
            Array.repeat 16 (Array.repeat 1024 0)
    in
    Z80Ram ScreenStorage.constructor ula ram2 ram2


getNestedRamValue : Int -> Array (Array Int) -> Int
getNestedRamValue raw bank =
    let
        list =
            raw |> shiftRightBy 10

        index =
            raw |> Bitwise.and 0x03FF
    in
    case bank |> Array.get list of
        Just array ->
            case array |> Array.get index of
                Just a ->
                    a

                Nothing ->
                    debugTodo "rambank2 getRamValue" (raw |> String.fromInt) -1

        Nothing ->
            debugTodo "rambank2 getRamValue" (raw |> String.fromInt) -1


setNestedRamValue : Int -> Int -> Array (Array Int) -> Array (Array Int)
setNestedRamValue raw value bank =
    let
        list =
            raw |> shiftRightBy 10

        index =
            raw |> Bitwise.and 0x03FF
    in
    case bank |> Array.get list of
        Just array ->
            let
                new =
                    array |> Array.set index value
            in
            bank |> Array.set list new

        Nothing ->
            debugTodo "rambank2 set" (raw |> String.fromInt) bank


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
        z80ram.bank2 |> getNestedRamValue (addr - 16384)

    else
        z80ram.bank3 |> getNestedRamValue (addr - 32768)


foldDictIntoRam : Dict Int Int -> Z80Ram -> Z80Ram
foldDictIntoRam ramdict z80_ram =
    ramdict
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
                    { z80ram | bank2 = z80ram.bank2 |> setNestedRamValue (addr - 16384) value }

                else
                    { z80ram | bank3 = z80ram.bank3 |> setNestedRamValue (addr - 32768) value }
            )
            z80_ram
