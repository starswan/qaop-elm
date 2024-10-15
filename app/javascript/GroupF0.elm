module GroupF0 exposing (..)

import Array exposing (Array)
import Bitwise
import Dict exposing (Dict)
import Group0x00 exposing (delta_dict_00, miniDict00)
import Group0x10 exposing (delta_dict_10)
import Group0x20 exposing (delta_dict_20, miniDict20)
import Group0x30 exposing (delta_dict_30)
import Group0x40 exposing (delta_dict_40, miniDict40)
import Group0x50 exposing (delta_dict_50, miniDict50)
import Group0x60 exposing (delta_dict_60, miniDict60)
import Group0x70 exposing (delta_dict_70, miniDict70)
import Group0x80 exposing (delta_dict_80, miniDict80)
import Group0x90 exposing (delta_dict_90)
import Group0xA0 exposing (delta_dict_A0)
import Group0xB0 exposing (delta_dict_B0)
import Group0xC0 exposing (delta_dict_C0)
import Group0xE0 exposing (delta_dict_E0)
import Z80Delta exposing (Z80Delta)
import Z80Env exposing (addCpuTimeEnv)
import Z80Flags exposing (c_FS, cp)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, imm8, jp_z80, rst_z80)


lt40_dict_lite : Dict Int (Z80ROM -> Z80 -> Z80)
lt40_dict_lite =
    Dict.fromList
        [ ( 0xF9, execute_0xF9 )
        , ( 0xFA, execute_0xFA )
        , ( 0xFE, execute_0xFE )
        , ( 0xFF, execute_0xFF )
        ]


execute_0xF9 : Z80ROM -> Z80 -> Z80
execute_0xF9 _ z80 =
    -- case 0xF9: SP=HL; time+=2; break;
    let
        env =
            z80.env
    in
    { z80 | env = { env | sp = z80.main.hl } |> addCpuTimeEnv 2 }


execute_0xFA : Z80ROM -> Z80 -> Z80
execute_0xFA rom48k z80 =
    -- case 0xFA: jp((Ff&FS)!=0); break;
    z80 |> jp_z80 (Bitwise.and z80.flags.ff c_FS /= 0) rom48k


execute_0xFE : Z80ROM -> Z80 -> Z80
execute_0xFE rom48k z80 =
    -- case 0xFE: cp(imm8()); break;
    let
        v =
            imm8 z80.pc z80.env.time rom48k z80.env.ram

        flags =
            z80.flags |> cp v.value

        env_1 =
            z80.env
    in
    { z80 | flags = flags, env = { env_1 | time = v.time }, pc = v.pc }


execute_0xFF : Z80ROM -> Z80 -> Z80
execute_0xFF _ z80 =
    z80 |> rst_z80 0xFF


lt40_delta_dict : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
lt40_delta_dict =
    delta_dict_00
        |> Dict.union delta_dict_80
        |> Dict.union delta_dict_90
        |> Dict.union delta_dict_A0
        |> Dict.union delta_dict_10
        |> Dict.union delta_dict_20
        |> Dict.union delta_dict_30
        |> Dict.union delta_dict_B0
        |> Dict.union delta_dict_40
        |> Dict.union delta_dict_50
        |> Dict.union delta_dict_60
        |> Dict.union delta_dict_70
        |> Dict.union delta_dict_C0
        |> Dict.union delta_dict_E0


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
    miniDict40
        |> Dict.union miniDict20
        |> Dict.union miniDict00
        |> Dict.union miniDict50
        |> Dict.union miniDict60
        |> Dict.union miniDict70
        |> Dict.union miniDict80
