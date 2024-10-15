module Group0x50 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..), delta_noop)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_h, get_h_ixiy, get_l, get_l_ixiy, hl_deref_with_z80)


delta_dict_50 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_50 =
    Dict.fromList
        [ ( 0x56, execute_0x56 )
        , ( 0x5E, execute_0x5E )
        ]


miniDict50 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict50 =
    Dict.fromList
        [ ( 0x54, ld_d_h )
        , ( 0x55, ld_d_l )
        , ( 0x5C, ld_e_h )
        , ( 0x5D, ld_e_l )
        ]


delta_dict_lite_50 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_50 =
    Dict.fromList
        [ -- case 0x52: break;
          ( 0x52, delta_noop )
        , ( 0x57, execute_0x57 )
        , -- case 0x5B: break;
          ( 0x5B, delta_noop )
        , ( 0x5F, execute_0x5F )
        ]


ld_d_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_d_h ixiyhl rom z80 =
    -- case 0x54: D=HL>>>8; break;
    --z80 |> set_d (get_h ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | d = get_h_ixiy ixiyhl z80.main } |> MainRegs


ld_d_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_d_l ixiyhl rom z80 =
    -- case 0x55: D=HL&0xFF; break;
    --z80 |> set_d (get_l ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | d = get_l_ixiy ixiyhl z80.main } |> MainRegs


execute_0x56 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x56 ixiyhl rom48k z80 =
    -- case 0x56: D=env.mem(HL); time+=3; break;
    let
        main =
            z80.main

        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_d value.value
    MainRegsWithPcAndCpuTime { main | d = value.value } value.pc value.time


execute_0x57 : Z80ROM -> Z80 -> Z80Delta
execute_0x57 rom z80 =
    -- case 0x57: D=A; break;
    --z80 |> set_d z80.flags.a
    let
        main =
            z80.main
    in
    { main | d = z80.flags.a } |> MainRegs


ld_e_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_e_h ixiyhl rom z80 =
    -- case 0x5C: E=HL>>>8; break;
    --z80 |> set_e (get_h ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | e = get_h_ixiy ixiyhl z80.main } |> MainRegs


ld_e_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_e_l ixiyhl rom z80 =
    -- case 0x5D: E=HL&0xFF; break;
    --z80 |> set_e (get_l ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | e = get_l_ixiy ixiyhl z80.main } |> MainRegs


execute_0x5E : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x5E ixiyhl rom48k z80 =
    -- case 0x5E: E=env.mem(HL); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        main =
            z80.main
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_e value.value
    MainRegsWithPcAndCpuTime { main | e = value.value } value.pc value.time


execute_0x5F : Z80ROM -> Z80 -> Z80Delta
execute_0x5F rom z80 =
    -- case 0x5F: E=A; break;
    --z80 |> set_e z80.flags.a
    let
        main =
            z80.main
    in
    { main | e = z80.flags.a } |> MainRegs
