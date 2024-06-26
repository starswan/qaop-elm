module Z80Word exposing (..)

import Bitwise
import Utils exposing (byte, char)
import Z80Byte exposing (Z80Byte, intToZ80, z80ToInt)


type
    Z80Word
    -- low high
    = Z80Word Z80Byte Z80Byte


top8Bits : Z80Word -> Z80Byte
top8Bits z80address =
    case z80address of
        Z80Word low high ->
            high


lower8Bits : Z80Word -> Z80Byte
lower8Bits z80address =
    case z80address of
        Z80Word low high ->
            low


wordPlusOffset : Z80Byte -> Z80Word -> Z80Word
wordPlusOffset z80byte z80word =
    let
        newvalue =
            (z80word |> z80wordToInt) + byte (z80byte |> z80ToInt) |> char
    in
    --Z80Word loval hival
    newvalue |> toZ80Word


z80wordToInt : Z80Word -> Int
z80wordToInt z80word =
    case z80word of
        Z80Word low high ->
            256 * (high |> z80ToInt) + (low |> z80ToInt)


toZ80Word : Int -> Z80Word
toZ80Word newvalue =
    let
        loval =
            newvalue |> remainderBy 256 |> intToZ80

        hival =
            newvalue // 256 |> intToZ80
    in
    Z80Word loval hival


incrementBy1 : Z80Word -> Z80Word
incrementBy1 z80word =
    ((z80word |> z80wordToInt) + 1) |> Bitwise.and 0xFFFF |> toZ80Word


incrementBy2 : Z80Word -> Z80Word
incrementBy2 z80word =
    ((z80word |> z80wordToInt) + 2) |> Bitwise.and 0xFFFF |> toZ80Word
