module Z80Memory exposing (..)

import Dict exposing (Dict)
import Utils exposing (toHexString)
import Z80Byte exposing (Nybble(..), Z80Byte, zeroByte)
import Z80Debug exposing (debugTodo)


type Z80Memory
    = Z80Memory (Dict Int Z80Byte)


constructor : List Z80Byte -> Z80Memory
constructor list =
    Z80Memory (ramListToDict list)


ramListToDict : List Z80Byte -> Dict Int Z80Byte
ramListToDict list =
    let
        ramarray =
            List.indexedMap Tuple.pair list
    in
    Dict.fromList ramarray


getMemValue : Int -> Z80Memory -> Z80Byte
getMemValue addr z80mem =
    case z80mem of
        Z80Memory z80dict ->
            case Dict.get addr z80dict of
                Just a ->
                    a

                Nothing ->
                    debugTodo ("Z80Memory:getValue " ++ (addr |> toHexString)) (Dict.size z80dict |> toHexString) zeroByte


setMemValue : Int -> Z80Byte -> Z80Memory -> Z80Memory
setMemValue addr value z80mem =
    case z80mem of
        Z80Memory dict ->
            dict |> Dict.insert addr value |> Z80Memory



-- This delivers the values in the order of the keys


getDataItems : Z80Memory -> List Z80Byte
getDataItems z80mem =
    case z80mem of
        Z80Memory dict ->
            dict |> Dict.values
