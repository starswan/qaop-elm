module GroupCB exposing (..)

import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..))
import Dict exposing (Dict)
import RegisterChange exposing (CBRegisterFlagChange(..), Shifter(..))
import SingleEnvWithMain exposing (SingleEnvMainChange(..))
import Utils exposing (BitTest(..), bitMaskFromBit, inverseBitMaskFromBit, shiftLeftBy8, shiftRightBy8)
import Z80Change exposing (Z80Change(..))
import Z80Flags exposing (FlagRegisters, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7, testBit)
import Z80Registers exposing (ChangeMainRegister(..), CoreRegister(..))
import Z80Types exposing (MainWithIndexRegisters)


singleEnvMainRegsCB : Dict Int ( MainWithIndexRegisters -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegsCB =
    Dict.fromList
        [ ( 0x46, ( bit_0_indirect_hl, TwelveTStates ) )
        , ( 0x4E, ( bit_1_indirect_hl, TwelveTStates ) )
        , ( 0x56, ( bit_2_indirect_hl, TwelveTStates ) )
        , ( 0x5E, ( bit_3_indirect_hl, TwelveTStates ) )
        , ( 0x66, ( bit_4_indirect_hl, TwelveTStates ) )
        , ( 0x6E, ( bit_5_indirect_hl, TwelveTStates ) )
        , ( 0x76, ( bit_6_indirect_hl, TwelveTStates ) )
        , ( 0x7E, ( bit_7_indirect_hl, TwelveTStates ) )
        ]


bit_0_indirect_hl : MainWithIndexRegisters -> SingleEnvMainChange
bit_0_indirect_hl z80_main =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    IndirectBitTest Bit_0 z80_main.hl


bit_1_indirect_hl : MainWithIndexRegisters -> SingleEnvMainChange
bit_1_indirect_hl z80_main =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    IndirectBitTest Bit_1 z80_main.hl


bit_2_indirect_hl : MainWithIndexRegisters -> SingleEnvMainChange
bit_2_indirect_hl z80_main =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    IndirectBitTest Bit_2 z80_main.hl


bit_3_indirect_hl : MainWithIndexRegisters -> SingleEnvMainChange
bit_3_indirect_hl z80_main =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    IndirectBitTest Bit_3 z80_main.hl


bit_4_indirect_hl : MainWithIndexRegisters -> SingleEnvMainChange
bit_4_indirect_hl z80_main =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    IndirectBitTest Bit_4 z80_main.hl


bit_5_indirect_hl : MainWithIndexRegisters -> SingleEnvMainChange
bit_5_indirect_hl z80_main =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    IndirectBitTest Bit_5 z80_main.hl


bit_6_indirect_hl : MainWithIndexRegisters -> SingleEnvMainChange
bit_6_indirect_hl z80_main =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    IndirectBitTest Bit_6 z80_main.hl


bit_7_indirect_hl : MainWithIndexRegisters -> SingleEnvMainChange
bit_7_indirect_hl z80_main =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    IndirectBitTest Bit_7 z80_main.hl


singleByteMainRegsCB : Dict Int ( CBRegisterFlagChange, InstructionDuration )
singleByteMainRegsCB =
    Dict.fromList
        [ -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
          ( 0x06, ( RegisterChangeShifter Shifter0 .hl, FifteenTStates ) )

        -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
        , ( 0x0E, ( RegisterChangeShifter Shifter1 .hl, FifteenTStates ) )
        , ( 0x16, ( RegisterChangeShifter Shifter2 .hl, FifteenTStates ) )
        , ( 0x1E, ( RegisterChangeShifter Shifter3 .hl, FifteenTStates ) )
        , ( 0x26, ( RegisterChangeShifter Shifter4 .hl, FifteenTStates ) )
        , ( 0x2E, ( RegisterChangeShifter Shifter5 .hl, FifteenTStates ) )
        , ( 0x36, ( RegisterChangeShifter Shifter6 .hl, FifteenTStates ) )
        , ( 0x3E, ( RegisterChangeShifter Shifter7 .hl, FifteenTStates ) )

        -- reset bit0
        , ( 0x80, ( TransformMainRegisters (resetBbit Bit_0), EightTStates ) )
        , ( 0x81, ( TransformMainRegisters (resetCbit Bit_0), EightTStates ) )
        , ( 0x82, ( TransformMainRegisters (resetDbit Bit_0), EightTStates ) )
        , ( 0x83, ( TransformMainRegisters (resetEbit Bit_0), EightTStates ) )
        , ( 0x84, ( TransformMainRegisters (resetHbit Bit_0), EightTStates ) )
        , ( 0x85, ( TransformMainRegisters (resetLbit Bit_0), EightTStates ) )
        , ( 0x86, ( resetHLbit Bit_0, EightTStates ) )

        -- reset bit1
        , ( 0x88, ( TransformMainRegisters (resetBbit Bit_1), EightTStates ) )
        , ( 0x89, ( TransformMainRegisters (resetCbit Bit_1), EightTStates ) )
        , ( 0x8A, ( TransformMainRegisters (resetDbit Bit_1), EightTStates ) )
        , ( 0x8B, ( TransformMainRegisters (resetEbit Bit_1), EightTStates ) )
        , ( 0x8C, ( TransformMainRegisters (resetHbit Bit_1), EightTStates ) )
        , ( 0x8D, ( TransformMainRegisters (resetLbit Bit_1), EightTStates ) )
        , ( 0x8E, ( resetHLbit Bit_1, EightTStates ) )

        -- reset bit2
        , ( 0x90, ( TransformMainRegisters (resetBbit Bit_2), EightTStates ) )
        , ( 0x91, ( TransformMainRegisters (resetCbit Bit_2), EightTStates ) )
        , ( 0x92, ( TransformMainRegisters (resetDbit Bit_2), EightTStates ) )
        , ( 0x93, ( TransformMainRegisters (resetEbit Bit_2), EightTStates ) )
        , ( 0x94, ( TransformMainRegisters (resetHbit Bit_2), EightTStates ) )
        , ( 0x95, ( TransformMainRegisters (resetLbit Bit_2), EightTStates ) )
        , ( 0x96, ( resetHLbit Bit_2, EightTStates ) )

        -- reset bit3
        , ( 0x98, ( TransformMainRegisters (resetBbit Bit_3), EightTStates ) )
        , ( 0x99, ( TransformMainRegisters (resetCbit Bit_3), EightTStates ) )
        , ( 0x9A, ( TransformMainRegisters (resetDbit Bit_3), EightTStates ) )
        , ( 0x9B, ( TransformMainRegisters (resetEbit Bit_3), EightTStates ) )
        , ( 0x9C, ( TransformMainRegisters (resetHbit Bit_3), EightTStates ) )
        , ( 0x9D, ( TransformMainRegisters (resetLbit Bit_3), EightTStates ) )
        , ( 0x9E, ( resetHLbit Bit_3, EightTStates ) )

        -- reset bit4
        , ( 0xA0, ( TransformMainRegisters (resetBbit Bit_4), EightTStates ) )
        , ( 0xA1, ( TransformMainRegisters (resetCbit Bit_4), EightTStates ) )
        , ( 0xA2, ( TransformMainRegisters (resetDbit Bit_4), EightTStates ) )
        , ( 0xA3, ( TransformMainRegisters (resetEbit Bit_4), EightTStates ) )
        , ( 0xA4, ( TransformMainRegisters (resetHbit Bit_4), EightTStates ) )
        , ( 0xA5, ( TransformMainRegisters (resetLbit Bit_4), EightTStates ) )
        , ( 0xA6, ( resetHLbit Bit_4, EightTStates ) )

        -- reset bit5
        , ( 0xA8, ( TransformMainRegisters (resetBbit Bit_5), EightTStates ) )
        , ( 0xA9, ( TransformMainRegisters (resetCbit Bit_5), EightTStates ) )
        , ( 0xAA, ( TransformMainRegisters (resetDbit Bit_5), EightTStates ) )
        , ( 0xAB, ( TransformMainRegisters (resetEbit Bit_5), EightTStates ) )
        , ( 0xAC, ( TransformMainRegisters (resetHbit Bit_5), EightTStates ) )
        , ( 0xAD, ( TransformMainRegisters (resetLbit Bit_5), EightTStates ) )
        , ( 0xAE, ( resetHLbit Bit_5, EightTStates ) )

        -- reset bit6
        , ( 0xB0, ( TransformMainRegisters (resetBbit Bit_6), EightTStates ) )
        , ( 0xB1, ( TransformMainRegisters (resetCbit Bit_6), EightTStates ) )
        , ( 0xB2, ( TransformMainRegisters (resetDbit Bit_6), EightTStates ) )
        , ( 0xB3, ( TransformMainRegisters (resetEbit Bit_6), EightTStates ) )
        , ( 0xB4, ( TransformMainRegisters (resetHbit Bit_6), EightTStates ) )
        , ( 0xB5, ( TransformMainRegisters (resetLbit Bit_6), EightTStates ) )
        , ( 0xB6, ( resetHLbit Bit_6, EightTStates ) )

        -- reset bit7
        , ( 0xB8, ( TransformMainRegisters (resetBbit Bit_7), EightTStates ) )
        , ( 0xB9, ( TransformMainRegisters (resetCbit Bit_7), EightTStates ) )
        , ( 0xBA, ( TransformMainRegisters (resetDbit Bit_7), EightTStates ) )
        , ( 0xBB, ( TransformMainRegisters (resetEbit Bit_7), EightTStates ) )
        , ( 0xBC, ( TransformMainRegisters (resetHbit Bit_7), EightTStates ) )
        , ( 0xBD, ( TransformMainRegisters (resetLbit Bit_7), EightTStates ) )
        , ( 0xBE, ( resetHLbit Bit_7, EightTStates ) )

        -- set bit0
        , ( 0xC0, ( TransformMainRegisters (setBbit Bit_0), EightTStates ) )
        , ( 0xC1, ( TransformMainRegisters (setCbit Bit_0), EightTStates ) )
        , ( 0xC2, ( TransformMainRegisters (setDbit Bit_0), EightTStates ) )
        , ( 0xC3, ( TransformMainRegisters (setEbit Bit_0), EightTStates ) )
        , ( 0xC4, ( TransformMainRegisters (setHbit Bit_0), EightTStates ) )
        , ( 0xC5, ( TransformMainRegisters (setLbit Bit_0), EightTStates ) )
        , ( 0xC6, ( setHLbit Bit_0, EightTStates ) )

        -- set bit1
        , ( 0xC8, ( TransformMainRegisters (setBbit Bit_1), EightTStates ) )
        , ( 0xC9, ( TransformMainRegisters (setCbit Bit_1), EightTStates ) )
        , ( 0xCA, ( TransformMainRegisters (setDbit Bit_1), EightTStates ) )
        , ( 0xCB, ( TransformMainRegisters (setEbit Bit_1), EightTStates ) )
        , ( 0xCC, ( TransformMainRegisters (setHbit Bit_1), EightTStates ) )
        , ( 0xCD, ( TransformMainRegisters (setLbit Bit_1), EightTStates ) )
        , ( 0xCE, ( setHLbit Bit_1, EightTStates ) )

        -- set bit2
        , ( 0xD0, ( TransformMainRegisters (setBbit Bit_2), EightTStates ) )
        , ( 0xD1, ( TransformMainRegisters (setCbit Bit_2), EightTStates ) )
        , ( 0xD2, ( TransformMainRegisters (setDbit Bit_2), EightTStates ) )
        , ( 0xD3, ( TransformMainRegisters (setEbit Bit_2), EightTStates ) )
        , ( 0xD4, ( TransformMainRegisters (setHbit Bit_2), EightTStates ) )
        , ( 0xD5, ( TransformMainRegisters (setLbit Bit_2), EightTStates ) )
        , ( 0xD6, ( setHLbit Bit_2, EightTStates ) )

        -- set bDt3
        , ( 0xD8, ( TransformMainRegisters (setBbit Bit_3), EightTStates ) )
        , ( 0xD9, ( TransformMainRegisters (setCbit Bit_3), EightTStates ) )
        , ( 0xDA, ( TransformMainRegisters (setDbit Bit_3), EightTStates ) )
        , ( 0xDB, ( TransformMainRegisters (setEbit Bit_3), EightTStates ) )
        , ( 0xDC, ( TransformMainRegisters (setHbit Bit_3), EightTStates ) )
        , ( 0xDD, ( TransformMainRegisters (setLbit Bit_3), EightTStates ) )
        , ( 0xDE, ( setHLbit Bit_3, EightTStates ) )

        -- set bit4
        , ( 0xE0, ( TransformMainRegisters (setBbit Bit_4), EightTStates ) )
        , ( 0xE1, ( TransformMainRegisters (setCbit Bit_4), EightTStates ) )
        , ( 0xE2, ( TransformMainRegisters (setDbit Bit_4), EightTStates ) )
        , ( 0xE3, ( TransformMainRegisters (setEbit Bit_4), EightTStates ) )
        , ( 0xE4, ( TransformMainRegisters (setHbit Bit_4), EightTStates ) )
        , ( 0xE5, ( TransformMainRegisters (setLbit Bit_4), EightTStates ) )
        , ( 0xE6, ( setHLbit Bit_4, EightTStates ) )

        -- set bEt5
        , ( 0xE8, ( TransformMainRegisters (setBbit Bit_5), EightTStates ) )
        , ( 0xE9, ( TransformMainRegisters (setCbit Bit_5), EightTStates ) )
        , ( 0xEA, ( TransformMainRegisters (setDbit Bit_5), EightTStates ) )
        , ( 0xEB, ( TransformMainRegisters (setEbit Bit_5), EightTStates ) )
        , ( 0xEC, ( TransformMainRegisters (setHbit Bit_5), EightTStates ) )
        , ( 0xED, ( TransformMainRegisters (setLbit Bit_5), EightTStates ) )
        , ( 0xEE, ( setHLbit Bit_5, EightTStates ) )

        -- set bit6
        , ( 0xF0, ( TransformMainRegisters (setBbit Bit_6), EightTStates ) )
        , ( 0xF1, ( TransformMainRegisters (setCbit Bit_6), EightTStates ) )
        , ( 0xF2, ( TransformMainRegisters (setDbit Bit_6), EightTStates ) )
        , ( 0xF3, ( TransformMainRegisters (setEbit Bit_6), EightTStates ) )
        , ( 0xF4, ( TransformMainRegisters (setHbit Bit_6), EightTStates ) )
        , ( 0xF5, ( TransformMainRegisters (setLbit Bit_6), EightTStates ) )
        , ( 0xF6, ( setHLbit Bit_6, EightTStates ) )

        -- set bFt7
        , ( 0xF8, ( TransformMainRegisters (setBbit Bit_7), EightTStates ) )
        , ( 0xF9, ( TransformMainRegisters (setCbit Bit_7), EightTStates ) )
        , ( 0xFA, ( TransformMainRegisters (setDbit Bit_7), EightTStates ) )
        , ( 0xFB, ( TransformMainRegisters (setEbit Bit_7), EightTStates ) )
        , ( 0xFC, ( TransformMainRegisters (setHbit Bit_7), EightTStates ) )
        , ( 0xFD, ( TransformMainRegisters (setLbit Bit_7), EightTStates ) )
        , ( 0xFE, ( setHLbit Bit_7, EightTStates ) )
        ]


resetBbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
resetBbit bitMask z80_main =
    -- case 0x80: B=B&~(1<<o); break;
    { z80_main | b = bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.b }


resetCbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
resetCbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    { z80_main | c = bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.c }


resetDbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
resetDbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    { z80_main | d = bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.d }


resetEbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
resetEbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    { z80_main | e = bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.e }


resetHbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
resetHbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    let
        new_h =
            bitMask |> inverseBitMaskFromBit |> Bitwise.and (z80_main.hl |> shiftRightBy8)
    in
    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 new_h) }


resetLbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
resetLbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    let
        new_l =
            bitMask |> inverseBitMaskFromBit |> Bitwise.and (z80_main.hl |> Bitwise.and 0xFF)
    in
    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF00) new_l }


resetHLbit : BitTest -> CBRegisterFlagChange
resetHLbit bitMask =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitReset bitMask .hl


setBbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
setBbit bitMask z80_main =
    -- case 0x80: B=B&~(1<<o); break;
    { z80_main | b = bitMask |> bitMaskFromBit |> Bitwise.or z80_main.b }


setCbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
setCbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    { z80_main | c = bitMask |> bitMaskFromBit |> Bitwise.or z80_main.c }


setDbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
setDbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    { z80_main | d = bitMask |> bitMaskFromBit |> Bitwise.or z80_main.d }


setEbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
setEbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    { z80_main | e = bitMask |> bitMaskFromBit |> Bitwise.or z80_main.e }


setHbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
setHbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    let
        new_h =
            bitMask |> bitMaskFromBit |> Bitwise.or (z80_main.hl |> shiftRightBy8)
    in
    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 new_h) }


setLbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
setLbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    let
        new_l =
            bitMask |> bitMaskFromBit |> Bitwise.or (z80_main.hl |> Bitwise.and 0xFF)
    in
    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF00) new_l }


setHLbit : BitTest -> CBRegisterFlagChange
setHLbit bitMask =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitSet bitMask .hl


singleByteMainAndFlagRegistersCB : Dict Int ( MainWithIndexRegisters -> FlagRegisters -> Z80Change, InstructionDuration )
singleByteMainAndFlagRegistersCB =
    Dict.fromList
        [ ( 0x00, ( rlc_b, EightTStates ) )
        , ( 0x01, ( rlc_c, EightTStates ) )
        , ( 0x02, ( rlc_d, EightTStates ) )
        , ( 0x03, ( rlc_e, EightTStates ) )
        , ( 0x04, ( rlc_h, EightTStates ) )
        , ( 0x05, ( rlc_l, EightTStates ) )
        , ( 0x08, ( rrc_b, EightTStates ) )
        , ( 0x09, ( rrc_c, EightTStates ) )
        , ( 0x0A, ( rrc_d, EightTStates ) )
        , ( 0x0B, ( rrc_e, EightTStates ) )
        , ( 0x0C, ( rrc_h, EightTStates ) )
        , ( 0x0D, ( rrc_l, EightTStates ) )
        , ( 0x10, ( rl_b, EightTStates ) )
        , ( 0x11, ( rl_c, EightTStates ) )
        , ( 0x12, ( rl_d, EightTStates ) )
        , ( 0x13, ( rl_e, EightTStates ) )
        , ( 0x14, ( rl_h, EightTStates ) )
        , ( 0x15, ( rl_l, EightTStates ) )
        , ( 0x18, ( rr_b, EightTStates ) )
        , ( 0x19, ( rr_c, EightTStates ) )
        , ( 0x1A, ( rr_d, EightTStates ) )
        , ( 0x1B, ( rr_e, EightTStates ) )
        , ( 0x1C, ( rr_h, EightTStates ) )
        , ( 0x1D, ( rr_l, EightTStates ) )
        , ( 0x20, ( sla_b, EightTStates ) )
        , ( 0x21, ( sla_c, EightTStates ) )
        , ( 0x22, ( sla_d, EightTStates ) )
        , ( 0x23, ( sla_e, EightTStates ) )
        , ( 0x24, ( sla_h, EightTStates ) )
        , ( 0x25, ( sla_l, EightTStates ) )
        , ( 0x28, ( sra_b, EightTStates ) )
        , ( 0x29, ( sra_c, EightTStates ) )
        , ( 0x2A, ( sra_d, EightTStates ) )
        , ( 0x2B, ( sra_e, EightTStates ) )
        , ( 0x2C, ( sra_h, EightTStates ) )
        , ( 0x2D, ( sra_l, EightTStates ) )
        , ( 0x30, ( sll_b, EightTStates ) )
        , ( 0x31, ( sll_c, EightTStates ) )
        , ( 0x32, ( sll_d, EightTStates ) )
        , ( 0x33, ( sll_e, EightTStates ) )
        , ( 0x34, ( sll_h, EightTStates ) )
        , ( 0x35, ( sll_l, EightTStates ) )
        , ( 0x38, ( srl_b, EightTStates ) )
        , ( 0x39, ( srl_c, EightTStates ) )
        , ( 0x3A, ( srl_d, EightTStates ) )
        , ( 0x3B, ( srl_e, EightTStates ) )
        , ( 0x3C, ( srl_h, EightTStates ) )
        , ( 0x3D, ( srl_l, EightTStates ) )
        , ( 0x40, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x41, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x42, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x43, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x44, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x45, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x48, ( bit_1_b, EightTStates ) )
        , ( 0x49, ( bit_1_c, EightTStates ) )
        , ( 0x4A, ( bit_1_d, EightTStates ) )
        , ( 0x4B, ( bit_1_e, EightTStates ) )
        , ( 0x4C, ( bit_1_h, EightTStates ) )
        , ( 0x4D, ( bit_1_l, EightTStates ) )
        , ( 0x50, ( bit_2_b, EightTStates ) )
        , ( 0x51, ( bit_2_c, EightTStates ) )
        , ( 0x52, ( bit_2_d, EightTStates ) )
        , ( 0x53, ( bit_2_e, EightTStates ) )
        , ( 0x54, ( bit_2_h, EightTStates ) )
        , ( 0x55, ( bit_2_l, EightTStates ) )
        , ( 0x58, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x59, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x5A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x5B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x5C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x5D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x60, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x61, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x62, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x63, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x64, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x65, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x68, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x69, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x6A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x6B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x6C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x6D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x70, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x71, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x72, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x73, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x74, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x75, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x78, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x79, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x7A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x7B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x7C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x7D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        ]


rlc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_b z80_main z80_flags =
    z80_flags |> shifter0 z80_main.b |> FlagsWithRegisterChange RegisterB


rlc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter0 z80_main.c |> FlagsWithRegisterChange RegisterC


rlc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter0 z80_main.d |> FlagsWithRegisterChange RegisterD


rlc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    z80_flags |> shifter0 z80_main.e |> FlagsWithRegisterChange RegisterE


rlc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter0 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


rlc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter0 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


rrc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_b z80_main z80_flags =
    z80_flags |> shifter1 z80_main.b |> FlagsWithRegisterChange RegisterB


rrc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter1 z80_main.c |> FlagsWithRegisterChange RegisterC


rrc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter1 z80_main.d |> FlagsWithRegisterChange RegisterD


rrc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    z80_flags |> shifter1 z80_main.e |> FlagsWithRegisterChange RegisterE


rrc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter1 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


rrc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter1 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


rl_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter2 z80_main.b |> FlagsWithRegisterChange RegisterB


rl_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter2 z80_main.c |> FlagsWithRegisterChange RegisterC


rl_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter2 z80_main.d |> FlagsWithRegisterChange RegisterD


rl_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    z80_flags |> shifter2 z80_main.e |> FlagsWithRegisterChange RegisterE


rl_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter2 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


rl_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter2 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


rr_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter3 z80_main.b |> FlagsWithRegisterChange RegisterB


rr_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter3 z80_main.c |> FlagsWithRegisterChange RegisterC


rr_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter3 z80_main.d |> FlagsWithRegisterChange RegisterD


rr_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    z80_flags
        |> shifter3 z80_main.e
        |> FlagsWithRegisterChange RegisterE


rr_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter3 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


rr_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter3 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


sla_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter4 z80_main.b |> FlagsWithRegisterChange RegisterB


sla_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter4 z80_main.c |> FlagsWithRegisterChange RegisterC


sla_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter4 z80_main.d |> FlagsWithRegisterChange RegisterD


sla_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    z80_flags
        |> shifter4 z80_main.e
        |> FlagsWithRegisterChange RegisterE


sla_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter4 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


sla_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter4 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


sra_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter5 z80_main.b |> FlagsWithRegisterChange RegisterB


sra_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter5 z80_main.c |> FlagsWithRegisterChange RegisterC


sra_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter5 z80_main.d |> FlagsWithRegisterChange RegisterD


sra_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    z80_flags
        |> shifter5 z80_main.e
        |> FlagsWithRegisterChange RegisterE


sra_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter5 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


sra_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter5 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


sll_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter6 z80_main.b |> FlagsWithRegisterChange RegisterB


sll_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter6 z80_main.c |> FlagsWithRegisterChange RegisterC


sll_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter6 z80_main.d |> FlagsWithRegisterChange RegisterD


sll_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    z80_flags
        |> shifter6 z80_main.e
        |> FlagsWithRegisterChange RegisterE


sll_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter6 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


sll_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter6 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


srl_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter7 z80_main.b |> FlagsWithRegisterChange RegisterB


srl_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter7 z80_main.c |> FlagsWithRegisterChange RegisterC


srl_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter7 z80_main.d |> FlagsWithRegisterChange RegisterD


srl_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    z80_flags
        |> shifter7 z80_main.e
        |> FlagsWithRegisterChange RegisterE


srl_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter7 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


srl_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter7 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


bit_1_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_b z80_main z80_flags =
    -- case 0x40: bit(o,B); break;
    z80_flags |> testBit Bit_1 z80_main.b |> Z80ChangeFlags


bit_1_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_c z80_main z80_flags =
    z80_flags |> testBit Bit_1 z80_main.c |> Z80ChangeFlags


bit_1_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_d z80_main z80_flags =
    z80_flags |> testBit Bit_1 z80_main.d |> Z80ChangeFlags


bit_1_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_e z80_main z80_flags =
    z80_flags |> testBit Bit_1 z80_main.e |> Z80ChangeFlags


bit_1_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_h z80_main z80_flags =
    z80_flags |> testBit Bit_1 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags


bit_1_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_l z80_main z80_flags =
    z80_flags |> testBit Bit_1 (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags


bit_2_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_b z80_main z80_flags =
    -- case 0x40: bit(o,B); break;
    z80_flags |> testBit Bit_2 z80_main.b |> Z80ChangeFlags


bit_2_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_c z80_main z80_flags =
    -- case 0x41: bit(o,C); break;
    z80_flags |> testBit Bit_2 z80_main.c |> Z80ChangeFlags


bit_2_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_d z80_main z80_flags =
    -- case 0x42: bit(o,D); break;
    z80_flags |> testBit Bit_2 z80_main.d |> Z80ChangeFlags


bit_2_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_e z80_main z80_flags =
    z80_flags |> testBit Bit_2 z80_main.e |> Z80ChangeFlags


bit_2_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_h z80_main z80_flags =
    z80_flags |> testBit Bit_2 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags


bit_2_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_l z80_main z80_flags =
    z80_flags |> testBit Bit_2 (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags
