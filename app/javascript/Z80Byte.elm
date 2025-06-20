module Z80Byte exposing (..)

import Bitwise
import Nybble exposing (Nybble(..))
import Utils exposing (BitTest(..), bitMaskFromBit, inverseBitMaskFromBit, shiftLeftBy8)


type
    Z80Byte
    -- high low
    = Z80Byte Nybble Nybble


zeroByte =
    Z80Byte Hex0 Hex0


ffByte =
    Z80Byte HexF HexF


hexThirtyEightByte =
    Z80Byte Hex3 Hex8


toInt : Z80Byte -> Int
toInt z80byte =
    case z80byte of
        Z80Byte high low ->
            let
                highVal =
                    high |> Nybble.toInt

                lowVal =
                    low |> Nybble.toInt
            in
            (highVal |> shiftLeftBy8) |> Bitwise.or lowVal


fromInt : Int -> Z80Byte
fromInt int =
    let
        low =
            (int |> Bitwise.and 0xFF) |> Nybble.fromInt

        high =
            (int |> Bitwise.shiftRightBy 8) |> Nybble.fromInt
    in
    Z80Byte high low


getLowNybble : Z80Byte -> Nybble
getLowNybble z80byte =
    case z80byte of
        Z80Byte _ low ->
            low


getHighNybble : Z80Byte -> Nybble
getHighNybble z80byte =
    case z80byte of
        Z80Byte high _ ->
            high


getBit6 : Z80Byte -> Bool
getBit6 z80byte =
    case z80byte of
        Z80Byte high _ ->
            [ Hex4, Hex5, Hex6, Hex7, HexF, HexE, HexD, HexC ] |> List.member high


getBit7 : Z80Byte -> Bool
getBit7 z80byte =
    case z80byte of
        Z80Byte high _ ->
            [ Hex8, Hex9, HexA, HexB, HexF, HexE, HexD, HexC ] |> List.member high


getBits210 : Z80Byte -> Int
getBits210 z80byte =
    case z80byte of
        Z80Byte _ low ->
            case low of
                Hex0 ->
                    0

                Hex1 ->
                    1

                Hex2 ->
                    2

                Hex3 ->
                    3

                Hex4 ->
                    4

                Hex5 ->
                    5

                Hex6 ->
                    6

                Hex7 ->
                    7

                Hex8 ->
                    0

                Hex9 ->
                    1

                HexA ->
                    2

                HexB ->
                    3

                HexC ->
                    4

                HexD ->
                    5

                HexE ->
                    6

                HexF ->
                    7


getBits543 : Z80Byte -> Int
getBits543 z80byte =
    case z80byte of
        Z80Byte high low ->
            let
                bit3 =
                    [ Hex8, Hex9, HexA, HexB, HexC, HexD, HexE, HexF ] |> List.member low

                bits45 =
                    case high of
                        Hex0 ->
                            0

                        Hex1 ->
                            2

                        Hex2 ->
                            4

                        Hex3 ->
                            6

                        Hex4 ->
                            0

                        Hex5 ->
                            2

                        Hex6 ->
                            4

                        Hex7 ->
                            6

                        Hex8 ->
                            0

                        Hex9 ->
                            2

                        HexA ->
                            4

                        HexB ->
                            6

                        HexC ->
                            0

                        HexD ->
                            2

                        HexE ->
                            4

                        HexF ->
                            6
            in
            if bit3 then
                bits45 |> Bitwise.or 1

            else
                bits45



--top8Bits : Z80Byte -> Int
--top8Bits z80byte =
--    z80byte.high |> nybbleToInt


resetBit : BitTest -> Z80Byte -> Z80Byte
resetBit testType z80byte =
    testType |> inverseBitMaskFromBit |> Bitwise.and (z80byte |> toInt) |> fromInt



--case bitTest of
--    Bit_0 -> z80byte
--
--
--    Bit_1 -> z80byte
--
--
--    Bit_2 -> z80byte
--
--
--    Bit_3 -> z80byte
--
--
--    Bit_4 -> z80byte
--
--
--    Bit_5 -> z80byte
--
--
--    Bit_6 -> z80byte
--
--
--    Bit_7 -> z80byte


setBit : BitTest -> Z80Byte -> Z80Byte
setBit testType z80byte =
    testType |> bitMaskFromBit |> Bitwise.or (z80byte |> toInt) |> fromInt


addOne : Z80Byte -> Z80Byte
addOne z80byte =
    z80byte |> toInt |> (\x -> x + 1) |> fromInt


subOne : Z80Byte -> Z80Byte
subOne z80byte =
    z80byte |> toInt |> (\x -> x - 1) |> fromInt
