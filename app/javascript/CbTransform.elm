module CbTransform exposing (..)

import Dict exposing (Dict)
import RegisterChange exposing (RegisterChange(..), Shifter(..))
import Z80Types exposing (MainWithIndexRegisters)


cbSingleByteMainRegs : Dict Int (MainWithIndexRegisters -> RegisterChange)
cbSingleByteMainRegs =
    Dict.fromList
        [ ( 0xCB06, rlc_indirect_hl )
        , ( 0xCB0E, rrc_indirect_hl )
        , ( 0xCB16, rl_indirect_hl )
        , ( 0xCB1E, rr_indirect_hl )
        , ( 0xCB26, sla_indirect_hl )
        , ( 0xCB2E, sra_indirect_hl )
        , ( 0xCB36, sll_indirect_hl )
        , ( 0xCB3E, srl_indirect_hl )
        ]


rlc_indirect_hl : MainWithIndexRegisters -> RegisterChange
rlc_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter0 z80_main.hl


rrc_indirect_hl : MainWithIndexRegisters -> RegisterChange
rrc_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter1 z80_main.hl


rl_indirect_hl : MainWithIndexRegisters -> RegisterChange
rl_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter2 z80_main.hl


rr_indirect_hl : MainWithIndexRegisters -> RegisterChange
rr_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter3 z80_main.hl


sla_indirect_hl : MainWithIndexRegisters -> RegisterChange
sla_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter4 z80_main.hl


sra_indirect_hl : MainWithIndexRegisters -> RegisterChange
sra_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter5 z80_main.hl


sll_indirect_hl : MainWithIndexRegisters -> RegisterChange
sll_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter6 z80_main.hl


srl_indirect_hl : MainWithIndexRegisters -> RegisterChange
srl_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter7 z80_main.hl
