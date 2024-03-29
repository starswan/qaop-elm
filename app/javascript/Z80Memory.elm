module Z80Memory exposing (..)

import Dict exposing (Dict)
import Utils exposing (toHexString)
import Z80Debug exposing (debug_todo)
type alias Z80Memory =
   {
      mainDict: Dict Int Int
   }

constructor: List Int -> Z80Memory
constructor list =
   let
      ramarray = List.indexedMap Tuple.pair list
   in
      Z80Memory (Dict.fromList ramarray)

getValue: Int -> Z80Memory -> Int
getValue addr z80dict  =
    case Dict.get addr z80dict.mainDict of
        Just a ->
          a
        Nothing ->
          debug_todo ("Z80Memory:getValue " ++ (addr |> toHexString)) (Dict.size z80dict.mainDict |> toHexString) -1

-- insert value at address addr (except that 16384 has been already subtracted)
set_value: Int -> Int -> Z80Memory -> Z80Memory
set_value addr value z80mem =
   { z80mem | mainDict = z80mem.mainDict |> Dict.insert addr value }

