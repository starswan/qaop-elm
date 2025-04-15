module Group0xF0 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Group0x70 exposing (miniDict70)
import Group0xC0 exposing (delta_dict_C0)
import Group0xE0 exposing (delta_dict_E0, miniDictE0)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), IXIYHL, Z80)


lt40_delta_dict : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
lt40_delta_dict =
    delta_dict_E0
        |> Dict.union delta_dict_C0


list0255 =
    List.range 0 255


lt40_array : Array (Maybe (IXIYHL -> Z80ROM -> Z80 -> Z80Delta))
lt40_array =
    let
        delta_funcs =
            list0255 |> List.map (\index -> lt40_delta_dict |> Dict.get index)
    in
    delta_funcs |> Array.fromList


xYDict : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
xYDict =
    miniDictE0
        |> Dict.union miniDict70
