module Group0x40 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_h_ixiy, get_l_ixiy, hl_deref_with_z80_ixiy)


miniDict40 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict40 =
    Dict.fromList
        [ ( 0x44, execute_0x44 )
        , ( 0x45, execute_0x45 )
        , ( 0x46, ld_b_indirect_hl )
        , ( 0x4C, ld_c_h )
        , ( 0x4D, ld_c_l )
        , ( 0x4E, ld_c_indirect_hl )
        ]


execute_0x44 : IXIY -> Z80ROM -> Z80 -> Z80Delta
execute_0x44 ixiyhl rom z80 =
    -- case 0x44: B=xy>>>8; break;
    --z80 |> set_b (get_h ixiyhl z80.main)
    let
        main =
            z80.main
    in
    MainRegsWithPc { main | b = get_h_ixiy ixiyhl z80.main } z80.pc


execute_0x45 : IXIY -> Z80ROM -> Z80 -> Z80Delta
execute_0x45 ixiyhl rom z80 =
    -- case 0x45: B=xy&0xFF; break;
    --  z80 |> set_b (get_l ixiyhl z80.main)
    let
        main =
            z80.main
    in
    MainRegsWithPc { main | b = get_l_ixiy ixiyhl z80.main } z80.pc


ld_b_indirect_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_b_indirect_hl ixiyhl rom48k z80 =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k

        main =
            z80.main
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_b value.value
    MainRegsWithPcAndCpuTime { main | b = value.value } value.pc value.time


ld_c_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_c_h ixiyhl rom z80 =
    -- case 0x4C: C=HL>>>8; break;
    --z80 |> set_c (get_h ixiyhl z80.main)
    let
        main =
            z80.main
    in
    MainRegsWithPc { main | c = get_h_ixiy ixiyhl z80.main } z80.pc


ld_c_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_c_l ixiyhl rom z80 =
    -- case 0x4D: C=HL&0xFF; break;
    --z80 |> set_c (get_l ixiyhl z80.main)
    let
        main =
            z80.main
    in
    MainRegsWithPc { main | c = get_l_ixiy ixiyhl z80.main } z80.pc


ld_c_indirect_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_c_indirect_hl ixiyhl rom48k z80 =
    -- case 0x4E: C=env.mem(HL); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k

        main =
            z80.main
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_c value.value
    MainRegsWithPcAndCpuTime { main | c = value.value } value.pc value.time
