module Group0xF0 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Group0x70 exposing (miniDict70)
import Group0xC0 exposing (delta_dict_C0)
import Group0xE0 exposing (delta_dict_E0, miniDictE0)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), IXIYHL, Z80)


miniDictF0 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDictF0 =
    Dict.fromList
        [ ( 0xF9, ld_sp_hl )
        ]


ld_sp_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_sp_hl ixiyhl rom48k z80 =
    -- case 0xF9: SP=xy; time+=2; break;
    let
        v =
            case ixiyhl of
                IXIY_IX ->
                    z80.main.ix

                IXIY_IY ->
                    z80.main.iy
    in
    --{ z80 | env = { env | sp = v } |> addCpuTimeEnv 2 }
    SpAndCpuTimeWithPc v 2 z80.pc


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
    miniDictF0
        |> Dict.union miniDict70
        |> Dict.union miniDictE0
