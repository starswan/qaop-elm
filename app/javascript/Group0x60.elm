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
        [ ( 0x67, ld_h_a )
        , ( 0x6F, ld_l_a )
        ]


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


ld_l_a : IXIY -> Z80ROM -> Z80Core -> Z80Delta
ld_l_a ixiyhl rom z80 =
    -- case 0x6F: HL=HL&0xFF00|A; break;
    -- case 0x6F: xy=xy&0xFF00|A; break;
    MainRegsWithPc (z80.main |> set_l_ixiy z80.flags.a ixiyhl) z80.pc
