module Z80Word exposing (..)

import Bitwise
import Utils exposing (byte, char)
import Z80Byte exposing (Z80Byte, zeroByte)


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
            (z80word |> toInt) + byte (z80byte |> Z80Byte.toInt) |> char
    in
    --Z80Word loval hival
    newvalue |> fromInt


toInt : Z80Word -> Int
toInt z80word =
    256 * (z80word.high |> Z80Byte.toInt) + (z80word.low |> Z80Byte.toInt)


fromInt : Int -> Z80Word
fromInt newvalue =
    let
        loval =
            newvalue |> remainderBy 256 |> Z80Byte.fromInt

        hival =
            newvalue // 256 |> Z80Byte.fromInt
    in
    Z80Word loval hival


incrementBy1 : Z80Word -> Z80Word
incrementBy1 z80word =
    ((z80word |> toInt) + 1) |> Bitwise.and 0xFFFF |> fromInt


incrementBy2 : Z80Word -> Z80Word
incrementBy2 z80word =
    ((z80word |> toInt) + 2) |> Bitwise.and 0xFFFF |> fromInt



--jumpBy : Z80Byte -> Z80Word -> Z80Word
--jumpBy offset z80word =
--    ((z80word |> Z80Word.toInt) + (offset |> Z80Byte.toInt |> byte)) |> Bitwise.and 0xFFFF |> Z80Word.fromInt


incrementBy3 : Z80Word -> Z80Word
incrementBy3 z80word =
    ((z80word |> toInt) + 3) |> Bitwise.and 0xFFFF |> fromInt


incrementBy4 : Z80Word -> Z80Word
incrementBy4 z80word =
    ((z80word |> toInt) + 4) |> Bitwise.and 0xFFFF |> fromInt


decrementBy1 : Z80Word -> Z80Word
decrementBy1 z80word =
    ((z80word |> toInt) - 1) |> Bitwise.and 0xFFFF |> fromInt


decrementBy2 : Z80Word -> Z80Word
decrementBy2 z80word =
    ((z80word |> toInt) - 2) |> Bitwise.and 0xFFFF |> fromInt
