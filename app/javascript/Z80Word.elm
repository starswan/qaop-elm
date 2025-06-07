module Z80Word exposing (..)

import Bitwise
import Utils exposing (byte, char)
import Z80Byte exposing (Z80Byte, intToZ80, z80ToInt, zeroByte)


type alias Z80Word =
    { low : Z80Byte
    , high : Z80Byte
    }


zeroWord =
    { low = zeroByte, high = zeroByte }



-- low high
--= Z80Word Z80Byte Z80Byte


top8Bits : Z80Word -> Z80Byte
top8Bits z80address =
    z80address.high


lower8Bits : Z80Word -> Z80Byte
lower8Bits z80address =
    z80address.low


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
    256 * (z80word.high |> z80ToInt) + (z80word.low |> z80ToInt)


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


jumpBy : Z80Byte -> Z80Word -> Z80Word
jumpBy offset z80word =
    ((z80word |> z80wordToInt) + (offset |> z80ToInt |> byte)) |> Bitwise.and 0xFFFF |> toZ80Word


incrementBy3 : Z80Word -> Z80Word
incrementBy3 z80word =
    ((z80word |> z80wordToInt) + 3) |> Bitwise.and 0xFFFF |> toZ80Word


incrementBy4 : Z80Word -> Z80Word
incrementBy4 z80word =
    ((z80word |> z80wordToInt) + 4) |> Bitwise.and 0xFFFF |> toZ80Word


decrementBy1 : Z80Word -> Z80Word
decrementBy1 z80word =
    ((z80word |> z80wordToInt) - 1) |> Bitwise.and 0xFFFF |> toZ80Word


decrementBy2 : Z80Word -> Z80Word
decrementBy2 z80word =
    ((z80word |> z80wordToInt) - 2) |> Bitwise.and 0xFFFF |> toZ80Word
