module Group0xA0 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (z80_and, z80_xor)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_h_ixiy, get_l_ixiy, hl_deref_with_z80_ixiy)


miniDictA0 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDictA0 =
    Dict.fromList
        [ ( 0xA4, and_h )
        , ( 0xA5, and_l )
        , ( 0xAC, xor_h )
        , ( 0xAD, xor_l )
        , ( 0xA6, and_indirect_hl )
        , ( 0xAE, xor_indirect_hl )
        ]


and_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
and_h ixiyhl _ z80 =
    -- case 0xA4: and(HL>>>8); break;
    -- case 0xA4: and(xy>>>8); break;
    --z80 |> set_flag_regs (z80_and (get_h ixiyhl z80.main) z80.flags)
    FlagRegsWithPc (z80.flags |> z80_and (get_h_ixiy ixiyhl z80.main)) z80.pc


and_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
and_l ixiyhl _ z80 =
    -- case 0xA5: and(HL&0xFF); break;
    -- case 0xA5: and(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_and (get_l ixiyhl z80.main) z80.flags)
    FlagRegsWithPc (z80.flags |> z80_and (get_l_ixiy ixiyhl z80.main)) z80.pc


and_indirect_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
and_indirect_hl ixiyhl rom48k z80 =
    -- case 0xA6: and(env.mem(HL)); time+=3; break;
    -- case 0xA6: and(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k
    in
    FlagsWithPcAndTime (z80.flags |> z80_and value.value) value.pc value.time


xor_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
xor_h ixiyhl _ z80 =
    -- case 0xAC: xor(HL>>>8); break;
    -- case 0xAC: xor(xy>>>8); break;
    FlagRegsWithPc (z80.flags |> z80_xor (get_h_ixiy ixiyhl z80.main)) z80.pc


xor_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
xor_l ixiyhl _ z80 =
    -- case 0xAD: xor(HL&0xFF); break;
    -- case 0xAD: xor(xy&0xFF); break;
    FlagRegsWithPc (z80.flags |> z80_xor (get_l_ixiy ixiyhl z80.main)) z80.pc


xor_indirect_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
xor_indirect_hl ixiyhl rom48k z80 =
    -- case 0xAE: xor(env.mem(HL)); time+=3; break;
    -- case 0xAE: xor(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k
    in
    FlagsWithPcAndTime (z80.flags |> z80_xor value.value) value.pc value.time
