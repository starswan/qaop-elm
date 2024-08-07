module Group0x40 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..), delta_noop)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL, Z80, get_h, get_l, hl_deref_with_z80)


delta_dict_40 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_40 =
    Dict.fromList
        [ ( 0x44, execute_0x44 )
        , ( 0x45, execute_0x45 )
        , ( 0x46, execute_0x46 )
        , ( 0x4C, execute_0x4C )
        , ( 0x4D, execute_0x4D )
        , ( 0x4E, execute_0x4E )
        ]


delta_dict_lite_40 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_40 =
    Dict.fromList
        [ -- case 0x40: break;
          ( 0x40, delta_noop )
        , ( 0x41, execute_0x41 )
        , ( 0x42, execute_0x42 )
        , ( 0x43, execute_0x43 )
        , ( 0x47, execute_0x47 )
        , ( 0x48, execute_0x48 )
        , -- case 0x49: break;
          ( 0x49, delta_noop )
        , ( 0x4A, execute_0x4A )
        , ( 0x4B, execute_0x4B )
        , ( 0x4F, execute_0x4F )
        ]


execute_0x41 : Z80ROM -> Z80 -> Z80Delta
execute_0x41 rom z80 =
    -- case 0x41: B=C; break;
    --z80 |> set_b z80.main.c
    let
        main =
            z80.main
    in
    { main | b = main.c } |> MainRegs


execute_0x42 : Z80ROM -> Z80 -> Z80Delta
execute_0x42 rom z80 =
    -- case 0x42: B=D; break;
    --z80 |> set_b z80.main.d
    let
        main =
            z80.main
    in
    { main | b = main.d } |> MainRegs


execute_0x43 : Z80ROM -> Z80 -> Z80Delta
execute_0x43 rom z80 =
    -- case 0x43: B=E; break;
    --z80 |> set_b z80.main.e
    let
        main =
            z80.main
    in
    { main | b = main.e } |> MainRegs


execute_0x44 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x44 ixiyhl rom z80 =
    -- case 0x44: B=HL>>>8; break;
    -- case 0x44: B=xy>>>8; break;
    --z80 |> set_b (get_h ixiyhl z80.main)
    let
        main =
            z80.main
    in
    MainRegsWithPc { main | b = get_h ixiyhl z80.main } z80.pc


execute_0x45 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x45 ixiyhl rom z80 =
    -- case 0x45: B=HL&0xFF; break;
    -- case 0x45: B=xy&0xFF; break;
    --  z80 |> set_b (get_l ixiyhl z80.main)
    let
        main =
            z80.main
    in
    MainRegsWithPc { main | b = get_l ixiyhl z80.main } z80.pc


execute_0x46 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x46 ixiyhl rom48k z80 =
    -- case 0x46: B=env.mem(HL); time+=3; break;
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        main =
            z80.main
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_b value.value
    MainRegsWithPcAndCpuTime { main | b = value.value } value.pc value.time


execute_0x47 : Z80ROM -> Z80 -> Z80Delta
execute_0x47 rom z80 =
    -- case 0x47: B=A; break;
    --z80 |> set_b z80.flags.a
    let
        main =
            z80.main
    in
    { main | b = z80.flags.a } |> MainRegs


execute_0x48 : Z80ROM -> Z80 -> Z80Delta
execute_0x48 rom z80 =
    -- case 0x48: C=B; break;
    --z80 |> set_c z80.main.b
    let
        main =
            z80.main
    in
    { main | c = z80.main.b } |> MainRegs


execute_0x4A : Z80ROM -> Z80 -> Z80Delta
execute_0x4A rom z80 =
    -- case 0x4A: C=D; break;
    --z80 |> set_c z80.main.d
    let
        main =
            z80.main
    in
    { main | c = z80.main.d } |> MainRegs


execute_0x4B : Z80ROM -> Z80 -> Z80Delta
execute_0x4B rom z80 =
    -- case 0x4B: C=E; break;
    --z80 |> set_c z80.main.e
    let
        main =
            z80.main
    in
    { main | c = z80.main.e } |> MainRegs


execute_0x4C : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x4C ixiyhl rom z80 =
    -- case 0x4C: C=HL>>>8; break;
    --z80 |> set_c (get_h ixiyhl z80.main)
    let
        main =
            z80.main
    in
    MainRegsWithPc { main | c = get_h ixiyhl z80.main } z80.pc


execute_0x4D : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x4D ixiyhl rom z80 =
    -- case 0x4D: C=HL&0xFF; break;
    --z80 |> set_c (get_l ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | c = get_l ixiyhl z80.main } |> MainRegs


execute_0x4E : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x4E ixiyhl rom48k z80 =
    -- case 0x4E: C=env.mem(HL); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        main =
            z80.main
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_c value.value
    MainRegsWithPcAndCpuTime { main | c = value.value } value.pc value.time


execute_0x4F : Z80ROM -> Z80 -> Z80Delta
execute_0x4F rom z80 =
    -- case 0x4F: C=A; break;
    --z80 |> set_c z80.flags.a
    let
        main =
            z80.main
    in
    { main | c = z80.flags.a } |> MainRegs
