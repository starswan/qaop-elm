module Z80Ram exposing (..)

import ScreenStorage exposing (Z80Screen, getScreenValue, setScreenValue)
import Z80Byte exposing (Nybble(..), Z80Byte(..), zeroByte)
import Z80Memory exposing (Z80Memory, getMemValue, setMemValue)


type alias Z80Ram =
    { screen : Z80Screen
    , non_screen : Z80Memory
    }


constructor : Z80Ram
constructor =
    let
        ram =
            List.repeat (49152 - 6912) zeroByte
    in
    Z80Ram ScreenStorage.constructor (Z80Memory.constructor ram)


getRamValue : Int -> Z80Ram -> Z80Byte
getRamValue addr z80ram =
    let
        ram_addr =
            addr - 6912
    in
    if ram_addr >= 0 then
        z80ram.non_screen |> getMemValue ram_addr

    else
        z80ram.screen |> getScreenValue addr


getRam16Value : Int -> Z80Ram -> (Z80Byte, Z80Byte)
getRam16Value addr z80ram =
    let
        ram_addr =
            addr - 6912

        ram_addr1 =
            addr + 1 - 6912
    in
    let
        low =
            if ram_addr >= 0 then
                z80ram.non_screen |> getMemValue ram_addr

            else
                z80ram.screen |> getScreenValue addr

        high =
            if ram_addr1 >= 0 then
                z80ram.non_screen |> getMemValue ram_addr1

            else
                z80ram.screen |> getScreenValue (addr + 1)
    in
    --Bitwise.or low (shiftLeftBy8 high)
    (high, low)


setRamValue : Int -> Z80Byte -> Z80Ram -> Z80Ram
setRamValue addr value z80ram =
    if addr >= 6912 then
        { z80ram | non_screen = z80ram.non_screen |> setMemValue (addr - 6912) value }

    else
        { z80ram | screen = z80ram.screen |> setScreenValue addr value }
