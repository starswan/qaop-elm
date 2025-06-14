module Z80Byte exposing (..)

import Bitwise
import Nybble exposing (Nybble(..))
import Utils exposing (BitTest(..), bitMaskFromBit, inverseBitMaskFromBit, shiftLeftBy8)


type
    Z80Byte
    -- high low
    = Z80Byte Nybble Nybble


zeroByte =
    Z80Byte NybbleZero NybbleZero


ffByte =
    Z80Byte NybbleFifteen NybbleFifteen


hexThirtyEightByte =
    Z80Byte NybbleThree NybbleEight


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
            [ NybbleFour, NybbleFive, NybbleSix, NybbleSeven, NybbleFifteen, NybbleFourteen, NybbleThirteen, NybbleTwelve ] |> List.member high


getBit7 : Z80Byte -> Bool
getBit7 z80byte =
    case z80byte of
        Z80Byte high _ ->
            [ NybbleEight, NybbleNine, NybbleTen, NybbleEleven, NybbleFifteen, NybbleFourteen, NybbleThirteen, NybbleTwelve ] |> List.member high


getBits210 : Z80Byte -> Int
getBits210 z80byte =
    case z80byte of
        Z80Byte _ low ->
            case low of
                NybbleZero ->
                    0

                NybbleOne ->
                    1

                NybbleTwo ->
                    2

                NybbleThree ->
                    3

                NybbleFour ->
                    4

                NybbleFive ->
                    5

                NybbleSix ->
                    6

                NybbleSeven ->
                    7

                NybbleEight ->
                    0

                NybbleNine ->
                    1

                NybbleTen ->
                    2

                NybbleEleven ->
                    3

                NybbleTwelve ->
                    4

                NybbleThirteen ->
                    5

                NybbleFourteen ->
                    6

                NybbleFifteen ->
                    7


getBits543 : Z80Byte -> Int
getBits543 z80byte =
    case z80byte of
        Z80Byte high low ->
            let
                bit3 =
                    [ NybbleEight, NybbleNine, NybbleTen, NybbleEleven, NybbleTwelve, NybbleThirteen, NybbleFourteen, NybbleFifteen ] |> List.member low

                bits45 =
                    case high of
                        NybbleZero ->
                            0

                        NybbleOne ->
                            2

                        NybbleTwo ->
                            4

                        NybbleThree ->
                            6

                        NybbleFour ->
                            0

                        NybbleFive ->
                            2

                        NybbleSix ->
                            4

                        NybbleSeven ->
                            6

                        NybbleEight ->
                            0

                        NybbleNine ->
                            2

                        NybbleTen ->
                            4

                        NybbleEleven ->
                            6

                        NybbleTwelve ->
                            0

                        NybbleThirteen ->
                            2

                        NybbleFourteen ->
                            4

                        NybbleFifteen ->
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
