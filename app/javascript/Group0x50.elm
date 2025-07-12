module Group0x50 exposing (..)

import Dict exposing (Dict)
import Z80Core exposing (Z80Core, hl_deref_with_z80_ixiy)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, get_h_ixiy, get_l_ixiy)


miniDict50 : Dict Int (IXIY -> Z80ROM -> Z80Core -> Z80Delta)
miniDict50 =
    Dict.fromList
        [ ( 0x56, ld_d_indirect_hl )
        , ( 0x5E, ld_e_indirect_hl )
        ]



--ld_d_h : IXIY -> Z80ROM -> Z80Core -> Z80Delta
--ld_d_h ixiyhl rom z80 =
--    -- case 0x54: D=HL>>>8; break;
--    --z80 |> set_d (get_h ixiyhl z80.main)
--    let
--        main =
--            z80.main
--    in
--    MainRegsWithPc { main | d = get_h_ixiy ixiyhl z80.main } z80.pc
--
--
--ld_d_l : IXIY -> Z80ROM -> Z80Core -> Z80Delta
--ld_d_l ixiyhl rom z80 =
--    -- case 0x55: D=HL&0xFF; break;
--    --z80 |> set_d (get_l ixiyhl z80.main)
--    let
--        main =
--            z80.main
--    in
--    MainRegsWithPc { main | d = get_l_ixiy ixiyhl z80.main } z80.pc


ld_d_indirect_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
ld_d_indirect_hl ixiyhl rom48k z80 =
    -- case 0x56: D=env.mem(HL); time+=3; break;
    let
        main =
            z80.main

        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_d value.value
    MainRegsWithPcAndCpuTime { main | d = value.value } value.pc value.time


ld_e_indirect_hl : IXIY -> Z80ROM -> Z80Core -> Z80Delta
ld_e_indirect_hl ixiyhl rom48k z80 =
    -- case 0x5E: E=env.mem(HL); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k

        main =
            z80.main
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_e value.value
    MainRegsWithPcAndCpuTime { main | e = value.value } value.pc value.time
