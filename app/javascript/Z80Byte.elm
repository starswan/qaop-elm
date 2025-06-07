module Z80Byte exposing (..)

import Bitwise
import Dict
import Utils exposing (BitTest(..), bitMaskFromBit, inverseBitMaskFromBit, shiftLeftBy8)


type Nybble
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Eleven
    | Twelve
    | Thirteen
    | Fourteen
    | Fifteen


type
    Z80Byte
    -- high low
    = Z80Byte Nybble Nybble


zeroByte =
    Z80Byte Zero Zero


ffByte =
    Z80Byte Fifteen Fifteen


hexThirtyEightByte =
    Z80Byte Three Eight


nybbleToInt : Nybble -> Int
nybbleToInt nybble =
    case nybble of
        Zero ->
            0

        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Eleven ->
            11

        Twelve ->
            12

        Thirteen ->
            13

        Fourteen ->
            14

        Fifteen ->
            15


nybbles =
    [ Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Eleven, Twelve, Thirteen, Fourteen, Fifteen ]


nybblesDict =
    nybbles |> List.indexedMap Tuple.pair |> Dict.fromList


intToNybble : Int -> Nybble
intToNybble nybble =
    case nybblesDict |> Dict.get nybble of
        Just a ->
            a

        Nothing ->
            Zero


z80ToInt : Z80Byte -> Int
z80ToInt z80byte =
    case z80byte of
        Z80Byte high low ->
            let
                highVal =
                    high |> nybbleToInt

                lowVal =
                    low |> nybbleToInt
            in
            (highVal |> shiftLeftBy8) |> Bitwise.or lowVal


intToZ80 : Int -> Z80Byte
intToZ80 int =
    let
        low =
            (int |> Bitwise.and 0xFF) |> intToNybble

        high =
            (int |> Bitwise.shiftRightBy 8) |> intToNybble
    in
    Z80Byte high low


getLowNybble : Z80Byte -> Nybble
getLowNybble z80byte =
    case z80byte of
        Z80Byte high low ->
            low


getHighNybble : Z80Byte -> Nybble
getHighNybble z80byte =
    case z80byte of
        Z80Byte high low ->
            high


getBit6 : Z80Byte -> Bool
getBit6 z80byte =
    case z80byte of
        Z80Byte high low ->
            [ Four, Five, Six, Seven, Fifteen, Fourteen, Thirteen, Twelve ] |> List.member high


getBit7 : Z80Byte -> Bool
getBit7 z80byte =
    case z80byte of
        Z80Byte high low ->
            [ Eight, Nine, Ten, Eleven, Fifteen, Fourteen, Thirteen, Twelve ] |> List.member high


getBits210 : Z80Byte -> Int
getBits210 z80byte =
    case z80byte of
        Z80Byte high low ->
            case low of
                Zero ->
                    0

                One ->
                    1

                Two ->
                    2

                Three ->
                    3

                Four ->
                    4

                Five ->
                    5

                Six ->
                    6

                Seven ->
                    7

                Eight ->
                    0

                Nine ->
                    1

                Ten ->
                    2

                Eleven ->
                    3

                Twelve ->
                    4

                Thirteen ->
                    5

                Fourteen ->
                    6

                Fifteen ->
                    7


getBits543 : Z80Byte -> Int
getBits543 z80byte =
    case z80byte of
        Z80Byte high low ->
            let
                bit3 =
                    [ Eight, Nine, Ten, Eleven, Twelve, Thirteen, Fourteen, Fifteen ] |> List.member low

                bits45 =
                    case high of
                        Zero ->
                            0

                        One ->
                            2

                        Two ->
                            4

                        Three ->
                            6

                        Four ->
                            0

                        Five ->
                            2

                        Six ->
                            4

                        Seven ->
                            6

                        Eight ->
                            0

                        Nine ->
                            2

                        Ten ->
                            4

                        Eleven ->
                            6

                        Twelve ->
                            0

                        Thirteen ->
                            2

                        Fourteen ->
                            4

                        Fifteen ->
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
    testType |> inverseBitMaskFromBit |> Bitwise.and (z80byte |> z80ToInt) |> intToZ80



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
    testType |> bitMaskFromBit |> Bitwise.or (z80byte |> z80ToInt) |> intToZ80


addOne : Z80Byte -> Z80Byte
addOne z80byte =
    z80byte |> z80ToInt |> (\x -> x + 1) |> intToZ80


subOne : Z80Byte -> Z80Byte
subOne z80byte =
    z80byte |> z80ToInt |> (\x -> x - 1) |> intToZ80
