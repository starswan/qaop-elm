--
-- $Id$
--


module Utils exposing (..)

import Array exposing (Array)
import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Dict exposing (Dict)
import Hex
import Process
import Task


compact list =
    List.filterMap identity list


shiftLeftBy8 : Int -> Int
shiftLeftBy8 reg =
    shiftLeftBy 8 reg


shiftLeftBy1 : Int -> Int
shiftLeftBy1 reg =
    shiftLeftBy 1 reg


shiftRightBy8 : Int -> Int
shiftRightBy8 reg =
    shiftRightBy 8 reg


shiftRightBy1 : Int -> Int
shiftRightBy1 reg =
    shiftRightBy 1 reg


listToDict : List Int -> Dict Int Int
listToDict intlist =
    let
        x =
            List.indexedMap Tuple.pair intlist
    in
    Dict.fromList x



--arrayToDict : Array Int -> Dict Int Int
--arrayToDict intlist =
--    let
--        x =
--            Array.indexedMap Tuple.pair intlist
--    in
--    Dict.fromArray x


digitToString : Int -> Int -> String
digitToString pad int =
    int |> String.fromInt |> String.padLeft pad '0'


byte : Int -> Int
byte value =
    if Bitwise.and value 0x80 /= 0 then
        value - 256

    else
        value



--char : Int -> Int
--char value =
--    Bitwise.and value 0xFFFF


char : Int -> Int
char =
    0xFFFF |> Bitwise.and


toHexString : Int -> String
toHexString value =
    "0x" ++ (Hex.toString value |> String.toUpper |> String.padLeft 4 '0')



-- just used for debugging


toHexString2 : Int -> String
toHexString2 value =
    "0x" ++ (Hex.toString value |> String.toUpper |> String.padLeft 2 '0')


toPlainHexString2 : Int -> String
toPlainHexString2 value =
    Hex.toString value |> String.toUpper |> String.padLeft 2 '0'



--https://stackoverflow.com/questions/40599512/how-to-achieve-behavior-of-settimeout-in-elm
--delay : Float -> msg -> Cmd msg
--delay time msg =
--    -- create a task that sleeps for `time`
--    Process.sleep time
--        |> -- once the sleep is over, ignore its output (using `always`)
--           -- and then we create a new task that simply returns a success, and the msg
--           Task.andThen (always <| Task.succeed msg)
--        |> -- finally, we ask Elm to perform the Task, which
--           -- takes the result of the above task and
--           -- returns it to our update function
--           Task.perform identity


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


c_DECIMAL_PLACES =
    3


loop_time_in_ms : Int -> Int -> Int
loop_time_in_ms elapsed_millis count =
    (10 ^ c_DECIMAL_PLACES) * elapsed_millis // count


time_display : Int -> Int -> String
time_display elapsed_millis count =
    let
        loop_time =
            loop_time_in_ms elapsed_millis count

        time_string =
            loop_time |> String.fromInt |> String.reverse |> String.toList |> List.drop c_DECIMAL_PLACES |> String.fromList |> String.reverse

        last =
            loop_time |> modBy (10 ^ c_DECIMAL_PLACES) |> digitToString c_DECIMAL_PLACES
    in
    "time " ++ time_string ++ "." ++ last ++ " ms "


speed_in_hz : Int -> Int -> String
speed_in_hz elapsed_millis count =
    let
        loop_time =
            loop_time_in_ms elapsed_millis count

        speed_in_mhz =
            1000000 / (loop_time |> toFloat) * 1000 |> round

        speed_in_hz_mant =
            speed_in_mhz // 1000

        speed_in_hz_frac =
            speed_in_mhz |> modBy 1000
    in
    (speed_in_hz_mant |> String.fromInt) ++ "." ++ (speed_in_hz_frac |> String.fromInt |> String.padLeft 3 '0')


type BitTest
    = Bit_0
    | Bit_1
    | Bit_2
    | Bit_3
    | Bit_4
    | Bit_5
    | Bit_6
    | Bit_7


bitMaskFromBit : BitTest -> Int
bitMaskFromBit testType =
    case testType of
        Bit_0 ->
            0x01

        Bit_1 ->
            0x02

        Bit_2 ->
            0x04

        Bit_3 ->
            0x08

        Bit_4 ->
            0x10

        Bit_5 ->
            0x20

        Bit_6 ->
            0x40

        Bit_7 ->
            0x80


inverseBitMaskFromBit : BitTest -> Int
inverseBitMaskFromBit testType =
    case testType of
        Bit_0 ->
            0xFE

        Bit_1 ->
            0xFD

        Bit_2 ->
            0xFB

        Bit_3 ->
            0xF7

        Bit_4 ->
            0xEF

        Bit_5 ->
            0xDF

        Bit_6 ->
            0xBF

        Bit_7 ->
            0x7F


wordPlusOffset : Int -> Int -> Int
wordPlusOffset z80byte z80word =
    let
        newvalue =
            z80word + byte z80byte |> char
    in
    --Z80Word loval hival
    newvalue


setBit : BitTest -> Int -> Int
setBit testType input =
    testType |> bitMaskFromBit |> Bitwise.or input


clearBit : BitTest -> Int -> Int
clearBit testType input =
    testType |> inverseBitMaskFromBit |> Bitwise.and input
