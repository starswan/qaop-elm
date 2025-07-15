module Group0x60 exposing (..)

import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import Z80Core exposing (Z80Core, hl_deref_with_z80_ixiy)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL(..), set_h, set_h_ixiy, set_l, set_l_ixiy)


miniDict60 : Dict Int (IXIY -> Z80ROM -> Z80Core -> Z80Delta)
miniDict60 =
    Dict.fromList
        [ ( 0x66, ld_h_indirect_hl )
        , ( 0x67, ld_h_a )
        , ( 0x6E, ld_l_indirect_hl )
        , ( 0x6F, ld_l_a )
        ]


ld_h_indirect_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
ld_h_indirect_hl ixiyhl rom48k z80 =
    -- case 0x66: HL=HL&0xFF|env.mem(HL)<<8; time+=3; break;
    -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k

        main =
            z80.main
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_h_z80 value.value HL |> add_cpu_time 3
    MainRegsWithPcAndCpuTime (main |> set_h value.value HL) value.pc (value.time |> addCpuTimeTime 3)


ld_h_a : IXIY -> Z80ROM -> Z80Core -> Z80Delta
ld_h_a ixiyhl rom z80 =
    -- case 0x67: HL=HL&0xFF|A<<8; break;
    -- case 0x67: xy=xy&0xFF|A<<8; break;
    --z80 |> set_h_z80 z80.flags.a ixiyhl
    let
        value =
            z80.main |> set_h_ixiy z80.flags.a ixiyhl
    in
    MainRegsWithPc value z80.pc


ld_l_indirect_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
ld_l_indirect_hl ixiyhl rom48k z80 =
    -- case 0x6E: HL=HL&0xFF00|env.mem(HL); time+=3; break;
    -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k

        main =
            z80.main
    in
    MainRegsWithPcAndCpuTime (main |> set_l value.value HL) value.pc (value.time |> addCpuTimeTime 3)


ld_l_a : IXIY -> Z80ROM -> Z80Core -> Z80Delta
ld_l_a ixiyhl rom z80 =
    -- case 0x6F: HL=HL&0xFF00|A; break;
    -- case 0x6F: xy=xy&0xFF00|A; break;
    MainRegsWithPc (z80.main |> set_l_ixiy z80.flags.a ixiyhl) z80.pc
