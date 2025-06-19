module Nybble exposing (..)

import Dict


type Nybble
    = Hex0
    | Hex1
    | Hex2
    | Hex3
    | Hex4
    | Hex5
    | Hex6
    | Hex7
    | Hex8
    | Hex9
    | HexA
    | HexB
    | HexC
    | HexD
    | HexE
    | HexF


nybbles =
    [ Hex0, Hex1, Hex2, Hex3, Hex4, Hex5, Hex6, Hex7, Hex8, Hex9, HexA, HexB, HexC, HexD, HexE, HexF ]


nybblesDict =
    nybbles |> List.indexedMap Tuple.pair |> Dict.fromList


fromInt : Int -> Nybble
fromInt nybble =
    case nybblesDict |> Dict.get nybble of
        Just a ->
            a

        Nothing ->
            Hex0


toInt : Nybble -> Int
toInt nybble =
    case nybble of
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
            8

        Hex9 ->
            9

        HexA ->
            0x0A

        HexB ->
            0x0B

        HexC ->
            0x0C

        HexD ->
            0x0D

        HexE ->
            0x0E

        HexF ->
            0x0F
