module Decoders exposing (..)

import Array exposing (Array)
import Bytes.Decode exposing (Decoder, Step(..), loop, map, succeed)


array_decoder : Int -> Decoder Int -> Decoder (Array Int)
array_decoder size decoder =
    loop ( size, Array.empty ) (arrayStep decoder)


arrayStep : Decoder Int -> ( Int, Array Int ) -> Decoder (Step ( Int, Array Int ) (Array Int))
arrayStep decoder ( n, xs ) =
    if n <= 0 then
        succeed (Done xs)

    else
        map (\x -> Loop ( n - 1, Array.push x xs )) decoder


listWithLengthDecoder : Int -> Decoder a -> Decoder (List a)
listWithLengthDecoder len decoder =
    loop ( len, [] ) (listStep decoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        succeed (Done (xs |> List.reverse))

    else
        map (\x -> Loop ( n - 1, x :: xs )) decoder
