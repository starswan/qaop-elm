module Nybble exposing (..)

import Dict


type Nybble
    = NybbleZero
    | NybbleOne
    | NybbleTwo
    | NybbleThree
    | NybbleFour
    | NybbleFive
    | NybbleSix
    | NybbleSeven
    | NybbleEight
    | NybbleNine
    | NybbleTen
    | NybbleEleven
    | NybbleTwelve
    | NybbleThirteen
    | NybbleFourteen
    | NybbleFifteen


nybbles =
    [ NybbleZero, NybbleOne, NybbleTwo, NybbleThree, NybbleFour, NybbleFive, NybbleSix, NybbleSeven, NybbleEight, NybbleNine, NybbleTen, NybbleEleven, NybbleTwelve, NybbleThirteen, NybbleFourteen, NybbleFifteen ]


nybblesDict =
    nybbles |> List.indexedMap Tuple.pair |> Dict.fromList


fromInt : Int -> Nybble
fromInt nybble =
    case nybblesDict |> Dict.get nybble of
        Just a ->
            a

        Nothing ->
            NybbleZero


toInt : Nybble -> Int
toInt nybble =
    case nybble of
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
            8

        NybbleNine ->
            9

        NybbleTen ->
            10

        NybbleEleven ->
            11

        NybbleTwelve ->
            12

        NybbleThirteen ->
            13

        NybbleFourteen ->
            14

        NybbleFifteen ->
            15
