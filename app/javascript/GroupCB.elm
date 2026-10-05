module GroupCB exposing (..)

import Array exposing (Array)
import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..))
import Dict exposing (Dict)
import IXIYChange exposing (IXIYChange(..))
import SimpleFlagOps exposing (resetBit, rl_a, rlc_a, rr_a, rrc_a, setFlagBit, sla_a, sll_a, sra_a, srl_a)
import Utils exposing (BitTest(..), bitMaskFromBit, inverseBitMaskFromBit, shiftLeftBy8, shiftRightBy8)
import Z80Change exposing (Shifter(..), Z80Change(..))
import Z80Flags exposing (FlagRegisters, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7, testBit)
import Z80Registers exposing (ChangeMainRegister(..), ChangeSingle(..), CoreRegister(..))
import Z80Types exposing (MainWithIndexRegisters)


bit_0_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_0_indirect_hl z80_main z80_flags =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    Z80IndirectMainBitTest Bit_0 .hl


bit_1_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_indirect_hl z80_main z80_flags =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    Z80IndirectMainBitTest Bit_1 .hl


bit_2_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_indirect_hl z80_main z80_flags =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    Z80IndirectMainBitTest Bit_2 .hl


bit_3_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_3_indirect_hl z80_main z80_flags =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    Z80IndirectMainBitTest Bit_3 .hl


bit_4_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_4_indirect_hl z80_main z80_flags =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    Z80IndirectMainBitTest Bit_4 .hl


bit_5_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_5_indirect_hl z80_main z80_flags =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    Z80IndirectMainBitTest Bit_5 .hl


bit_6_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_6_indirect_hl z80_main z80_flags =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    Z80IndirectMainBitTest Bit_6 .hl


bit_7_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_7_indirect_hl z80_main z80_flags =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    Z80IndirectMainBitTest Bit_7 .hl


singleByteMainRegsCB80 : Dict Int ( MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleByteMainRegsCB80 =
    Dict.fromList
        [ -- reset bit0
          ( 0x80, ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_0), EightTStates ) )
        , ( 0x81, ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_0), EightTStates ) )
        , ( 0x82, ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_0), EightTStates ) )
        , ( 0x83, ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_0), EightTStates ) )
        , ( 0x84, ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_0), EightTStates ) )
        , ( 0x85, ( resetLbit Bit_0, EightTStates ) )
        , ( 0x86, ( resetHLbit Bit_0, EightTStates ) )

        -- reset bit1
        , ( 0x88, ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_1), EightTStates ) )
        , ( 0x89, ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_1), EightTStates ) )
        , ( 0x8A, ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_1), EightTStates ) )
        , ( 0x8B, ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_1), EightTStates ) )
        , ( 0x8C, ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_1), EightTStates ) )
        , ( 0x8D, ( resetLbit Bit_1, EightTStates ) )
        , ( 0x8E, ( resetHLbit Bit_1, EightTStates ) )

        -- reset bit2
        , ( 0x90, ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_2), EightTStates ) )
        , ( 0x91, ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_2), EightTStates ) )
        , ( 0x92, ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_2), EightTStates ) )
        , ( 0x93, ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_2), EightTStates ) )
        , ( 0x94, ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_2), EightTStates ) )
        , ( 0x95, ( resetLbit Bit_2, EightTStates ) )
        , ( 0x96, ( resetHLbit Bit_2, EightTStates ) )

        -- reset bit3
        , ( 0x98, ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_3), EightTStates ) )
        , ( 0x99, ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_3), EightTStates ) )
        , ( 0x9A, ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_3), EightTStates ) )
        , ( 0x9B, ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_3), EightTStates ) )
        , ( 0x9C, ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_3), EightTStates ) )
        , ( 0x9D, ( resetLbit Bit_3, EightTStates ) )
        , ( 0x9E, ( resetHLbit Bit_3, EightTStates ) )

        -- reset bit4
        , ( 0xA0, ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_4), EightTStates ) )
        , ( 0xA1, ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_4), EightTStates ) )
        , ( 0xA2, ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_4), EightTStates ) )
        , ( 0xA3, ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_4), EightTStates ) )
        , ( 0xA4, ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_4), EightTStates ) )
        , ( 0xA5, ( resetLbit Bit_4, EightTStates ) )
        , ( 0xA6, ( resetHLbit Bit_4, EightTStates ) )

        -- reset bit5
        , ( 0xA8, ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_5), EightTStates ) )
        , ( 0xA9, ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_5), EightTStates ) )
        , ( 0xAA, ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_5), EightTStates ) )
        , ( 0xAB, ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_5), EightTStates ) )
        , ( 0xAC, ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_5), EightTStates ) )
        , ( 0xAD, ( resetLbit Bit_5, EightTStates ) )
        , ( 0xAE, ( resetHLbit Bit_5, EightTStates ) )

        -- reset bit6
        , ( 0xB0, ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_6), EightTStates ) )
        , ( 0xB1, ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_6), EightTStates ) )
        , ( 0xB2, ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_6), EightTStates ) )
        , ( 0xB3, ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_6), EightTStates ) )
        , ( 0xB4, ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_6), EightTStates ) )
        , ( 0xB5, ( resetLbit Bit_6, EightTStates ) )
        , ( 0xB6, ( resetHLbit Bit_6, EightTStates ) )

        -- reset bit7
        , ( 0xB8, ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_7), EightTStates ) )
        , ( 0xB9, ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_7), EightTStates ) )
        , ( 0xBA, ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_7), EightTStates ) )
        , ( 0xBB, ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_7), EightTStates ) )
        , ( 0xBC, ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_7), EightTStates ) )
        , ( 0xBD, ( resetLbit Bit_7, EightTStates ) )
        , ( 0xBE, ( resetHLbit Bit_7, EightTStates ) )

        -- set bit0
        , ( 0xC0, ( \z80_main -> TransformMainRegistersCB (setBbit Bit_0), EightTStates ) )
        , ( 0xC1, ( \z80_main -> TransformMainRegistersCB (setCbit Bit_0), EightTStates ) )
        , ( 0xC2, ( \z80_main -> TransformMainRegistersCB (setDbit Bit_0), EightTStates ) )
        , ( 0xC3, ( \z80_main -> TransformMainRegistersCB (setEbit Bit_0), EightTStates ) )
        , ( 0xC4, ( \z80_main -> TransformMainRegistersCB (setHbit Bit_0), EightTStates ) )
        , ( 0xC5, ( setLbit Bit_0, EightTStates ) )
        , ( 0xC6, ( setHLbit Bit_0, EightTStates ) )

        -- set bit1
        , ( 0xC8, ( \z80_main -> TransformMainRegistersCB (setBbit Bit_1), EightTStates ) )
        , ( 0xC9, ( \z80_main -> TransformMainRegistersCB (setCbit Bit_1), EightTStates ) )
        , ( 0xCA, ( \z80_main -> TransformMainRegistersCB (setDbit Bit_1), EightTStates ) )
        , ( 0xCB, ( \z80_main -> TransformMainRegistersCB (setEbit Bit_1), EightTStates ) )
        , ( 0xCC, ( \z80_main -> TransformMainRegistersCB (setHbit Bit_1), EightTStates ) )
        , ( 0xCD, ( setLbit Bit_1, EightTStates ) )
        , ( 0xCE, ( setHLbit Bit_1, EightTStates ) )

        -- set bit2
        , ( 0xD0, ( \z80_main -> TransformMainRegistersCB (setBbit Bit_2), EightTStates ) )
        , ( 0xD1, ( \z80_main -> TransformMainRegistersCB (setCbit Bit_2), EightTStates ) )
        , ( 0xD2, ( \z80_main -> TransformMainRegistersCB (setDbit Bit_2), EightTStates ) )
        , ( 0xD3, ( \z80_main -> TransformMainRegistersCB (setEbit Bit_2), EightTStates ) )
        , ( 0xD4, ( \z80_main -> TransformMainRegistersCB (setHbit Bit_2), EightTStates ) )
        , ( 0xD5, ( setLbit Bit_2, EightTStates ) )
        , ( 0xD6, ( setHLbit Bit_2, EightTStates ) )

        -- set bDt3
        , ( 0xD8, ( \z80_main -> TransformMainRegistersCB (setBbit Bit_3), EightTStates ) )
        , ( 0xD9, ( \z80_main -> TransformMainRegistersCB (setCbit Bit_3), EightTStates ) )
        , ( 0xDA, ( \z80_main -> TransformMainRegistersCB (setDbit Bit_3), EightTStates ) )
        , ( 0xDB, ( \z80_main -> TransformMainRegistersCB (setEbit Bit_3), EightTStates ) )
        , ( 0xDC, ( \z80_main -> TransformMainRegistersCB (setHbit Bit_3), EightTStates ) )
        , ( 0xDD, ( setLbit Bit_3, EightTStates ) )
        , ( 0xDE, ( setHLbit Bit_3, EightTStates ) )

        -- set bit4
        , ( 0xE0, ( \z80_main -> TransformMainRegistersCB (setBbit Bit_4), EightTStates ) )
        , ( 0xE1, ( \z80_main -> TransformMainRegistersCB (setCbit Bit_4), EightTStates ) )
        , ( 0xE2, ( \z80_main -> TransformMainRegistersCB (setDbit Bit_4), EightTStates ) )
        , ( 0xE3, ( \z80_main -> TransformMainRegistersCB (setEbit Bit_4), EightTStates ) )
        , ( 0xE4, ( \z80_main -> TransformMainRegistersCB (setHbit Bit_4), EightTStates ) )
        , ( 0xE5, ( setLbit Bit_4, EightTStates ) )
        , ( 0xE6, ( setHLbit Bit_4, EightTStates ) )

        -- set bEt5
        , ( 0xE8, ( \z80_main -> TransformMainRegistersCB (setBbit Bit_5), EightTStates ) )
        , ( 0xE9, ( \z80_main -> TransformMainRegistersCB (setCbit Bit_5), EightTStates ) )
        , ( 0xEA, ( \z80_main -> TransformMainRegistersCB (setDbit Bit_5), EightTStates ) )
        , ( 0xEB, ( \z80_main -> TransformMainRegistersCB (setEbit Bit_5), EightTStates ) )
        , ( 0xEC, ( \z80_main -> TransformMainRegistersCB (setHbit Bit_5), EightTStates ) )
        , ( 0xED, ( setLbit Bit_5, EightTStates ) )
        , ( 0xEE, ( setHLbit Bit_5, EightTStates ) )

        -- set bit6
        , ( 0xF0, ( \z80_main -> TransformMainRegistersCB (setBbit Bit_6), EightTStates ) )
        , ( 0xF1, ( \z80_main -> TransformMainRegistersCB (setCbit Bit_6), EightTStates ) )
        , ( 0xF2, ( \z80_main -> TransformMainRegistersCB (setDbit Bit_6), EightTStates ) )
        , ( 0xF3, ( \z80_main -> TransformMainRegistersCB (setEbit Bit_6), EightTStates ) )
        , ( 0xF4, ( \z80_main -> TransformMainRegistersCB (setHbit Bit_6), EightTStates ) )
        , ( 0xF5, ( setLbit Bit_6, EightTStates ) )
        , ( 0xF6, ( setHLbit Bit_6, EightTStates ) )

        -- set bFt7
        , ( 0xF8, ( \z80_main -> TransformMainRegistersCB (setBbit Bit_7), EightTStates ) )
        , ( 0xF9, ( \z80_main -> TransformMainRegistersCB (setCbit Bit_7), EightTStates ) )
        , ( 0xFA, ( \z80_main -> TransformMainRegistersCB (setDbit Bit_7), EightTStates ) )
        , ( 0xFB, ( \z80_main -> TransformMainRegistersCB (setEbit Bit_7), EightTStates ) )
        , ( 0xFC, ( \z80_main -> TransformMainRegistersCB (setHbit Bit_7), EightTStates ) )
        , ( 0xFD, ( setLbit Bit_7, EightTStates ) )
        , ( 0xFE, ( setHLbit Bit_7, EightTStates ) )
        ]


rl_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_indirect_hl z80_main z80_flags =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter2 .hl


rr_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_indirect_hl z80_main z80_flags =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter3 .hl


sla_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_indirect_hl z80_main z80_flags =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter4 .hl


sra_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_indirect_hl z80_main z80_flags =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter5 .hl


sll_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_indirect_hl z80_main z80_flags =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter6 .hl


srl_indirect_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_indirect_hl z80_main z80_flags =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter7 .hl


resetBbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
resetBbit bitMask z80_main =
    -- case 0x80: B=B&~(1<<o); break;
    -- SingleRegisterChange ChangeSingleB (bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.b)
    { z80_main | b = bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.b }


resetCbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
resetCbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    --SingleRegisterChange ChangeSingleC (bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.c)
    { z80_main | c = bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.c }


resetDbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
resetDbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    --SingleRegisterChange ChangeSingleD (bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.d)
    { z80_main | d = bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.d }


resetEbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
resetEbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    --SingleRegisterChange ChangeSingleE (bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.e)
    { z80_main | e = bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.e }


resetHbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
resetHbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    --SingleRegisterChange ChangeSingleH (bitMask |> inverseBitMaskFromBit |> Bitwise.and (z80_main.hl |> shiftRightBy8))
    let
        new_h =
            bitMask |> inverseBitMaskFromBit |> Bitwise.and (z80_main.hl |> shiftRightBy8)
    in
    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 new_h) }


resetLbit : BitTest -> MainWithIndexRegisters -> IXIYChange
resetLbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange ChangeSingleL (bitMask |> inverseBitMaskFromBit |> Bitwise.and (z80_main.hl |> Bitwise.and 0xFF))


resetHLbit : BitTest -> MainWithIndexRegisters -> IXIYChange
resetHLbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitReset bitMask z80_main.hl


setBbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
setBbit bitMask z80_main =
    -- case 0x80: B=B&~(1<<o); break;
    --SingleRegisterChange ChangeSingleB (bitMask |> bitMaskFromBit |> Bitwise.or z80_main.b)
    { z80_main | b = bitMask |> bitMaskFromBit |> Bitwise.or z80_main.b }


setCbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
setCbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    --SingleRegisterChange ChangeSingleC (bitMask |> bitMaskFromBit |> Bitwise.or z80_main.c)
    { z80_main | c = bitMask |> bitMaskFromBit |> Bitwise.or z80_main.c }


setDbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
setDbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    --SingleRegisterChange ChangeSingleD (bitMask |> bitMaskFromBit |> Bitwise.or z80_main.d)
    { z80_main | d = bitMask |> bitMaskFromBit |> Bitwise.or z80_main.d }


setEbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
setEbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    --SingleRegisterChange ChangeSingleE (bitMask |> bitMaskFromBit |> Bitwise.or z80_main.e)
    { z80_main | e = bitMask |> bitMaskFromBit |> Bitwise.or z80_main.e }


setHbit : BitTest -> MainWithIndexRegisters -> MainWithIndexRegisters
setHbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    --SingleRegisterChange ChangeSingleH (bitMask |> bitMaskFromBit |> Bitwise.or (z80_main.hl |> shiftRightBy8))
    let
        new_h =
            bitMask |> bitMaskFromBit |> Bitwise.or (z80_main.hl |> shiftRightBy8)
    in
    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 new_h) }


setLbit : BitTest -> MainWithIndexRegisters -> IXIYChange
setLbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange ChangeSingleL (bitMask |> bitMaskFromBit |> Bitwise.or (z80_main.hl |> Bitwise.and 0xFF))


setHLbit : BitTest -> MainWithIndexRegisters -> IXIYChange
setHLbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitSet bitMask z80_main.hl


singleByteMainAndFlagRegistersCB : Array ( MainWithIndexRegisters -> FlagRegisters -> Z80Change, InstructionDuration )
singleByteMainAndFlagRegistersCB =
    Array.fromList
        [ ( rlc_b, EightTStates )
        , ( rlc_c, EightTStates )
        , ( rlc_d, EightTStates )
        , ( rlc_e, EightTStates )
        , ( rlc_h, EightTStates )
        , ( rlc_l, EightTStates )

        -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
        , ( \z80_main z80_flags -> RegisterChangeShifter Shifter0 .hl, FifteenTStates )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc rlc_a, EightTStates )
        , ( rrc_b, EightTStates )
        , ( rrc_c, EightTStates )
        , ( rrc_d, EightTStates )
        , ( rrc_e, EightTStates )
        , ( rrc_h, EightTStates )
        , ( rrc_l, EightTStates )
        , ( \z80_main z80_flags -> RegisterChangeShifter Shifter1 .hl, FifteenTStates )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc rrc_a, EightTStates )
        , ( rl_b, EightTStates )
        , ( rl_c, EightTStates )
        , ( rl_d, EightTStates )
        , ( rl_e, EightTStates )
        , ( rl_h, EightTStates )
        , ( rl_l, EightTStates )
        , ( rl_indirect_hl, FifteenTStates )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc rl_a, EightTStates )
        , ( rr_b, EightTStates )
        , ( rr_c, EightTStates )
        , ( rr_d, EightTStates )
        , ( rr_e, EightTStates )
        , ( rr_h, EightTStates )
        , ( rr_l, EightTStates )
        , ( rr_indirect_hl, FifteenTStates )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc rr_a, EightTStates )
        , ( sla_b, EightTStates )
        , ( sla_c, EightTStates )
        , ( sla_d, EightTStates )
        , ( sla_e, EightTStates )
        , ( sla_h, EightTStates )
        , ( sla_l, EightTStates )
        , ( sla_indirect_hl, FifteenTStates )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc sla_a, EightTStates )
        , ( sra_b, EightTStates )
        , ( sra_c, EightTStates )
        , ( sra_d, EightTStates )
        , ( sra_e, EightTStates )
        , ( sra_h, EightTStates )
        , ( sra_l, EightTStates )
        , ( sra_indirect_hl, FifteenTStates )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc sra_a, EightTStates )
        , ( sll_b, EightTStates )
        , ( sll_c, EightTStates )
        , ( sll_d, EightTStates )
        , ( sll_e, EightTStates )
        , ( sll_h, EightTStates )
        , ( sll_l, EightTStates )
        , ( sll_indirect_hl, FifteenTStates )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc sll_a, EightTStates )

        -- case 0x00: B=shifter(o,B); break;
        , ( \z80_main z80_flags -> z80_flags |> shifter7 z80_main.b |> FlagsWithRegisterChange RegisterB, EightTStates )
        , ( srl_c, EightTStates )
        , ( srl_d, EightTStates )
        , ( srl_e, EightTStates )
        , ( srl_h, EightTStates )
        , ( srl_l, EightTStates )
        , ( srl_indirect_hl, FifteenTStates )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc srl_a, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.b |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.c |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.d |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.e |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates )
        , ( bit_0_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_0 z80_flags.a), EightTStates )
        , ( bit_1_b, EightTStates )
        , ( bit_1_c, EightTStates )
        , ( bit_1_d, EightTStates )
        , ( bit_1_e, EightTStates )
        , ( bit_1_h, EightTStates )
        , ( bit_1_l, EightTStates )
        , ( bit_1_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_1 z80_flags.a), EightTStates )
        , ( bit_2_b, EightTStates )
        , ( bit_2_c, EightTStates )
        , ( bit_2_d, EightTStates )
        , ( bit_2_e, EightTStates )
        , ( bit_2_h, EightTStates )
        , ( bit_2_l, EightTStates )
        , ( bit_2_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_2 z80_flags.a), EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.b |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.c |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.d |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.e |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates )
        , ( bit_3_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_3 z80_flags.a), EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.b |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.c |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.d |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.e |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates )
        , ( bit_4_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_4 z80_flags.a), EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.b |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.c |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.d |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.e |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates )
        , ( bit_5_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_5 z80_flags.a), EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.b |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.c |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.d |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.e |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates )
        , ( bit_6_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_6 z80_flags.a), EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.b |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.c |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.d |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.e |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates )
        , ( bit_7_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_7 z80_flags.a), EightTStates )
        ]


singleByteMainAndFlagRegistersCB80 : Dict Int ( MainWithIndexRegisters -> FlagRegisters -> Z80Change, InstructionDuration )
singleByteMainAndFlagRegistersCB80 =
    Dict.fromList
        [ ( 0x87, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_0), EightTStates ) )
        , ( 0x8F, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_1), EightTStates ) )
        , ( 0x97, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_2), EightTStates ) )
        , ( 0x9F, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_3), EightTStates ) )
        , ( 0xA7, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_4), EightTStates ) )
        , ( 0xAF, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_5), EightTStates ) )
        , ( 0xB7, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_6), EightTStates ) )
        , ( 0xBF, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_7), EightTStates ) )
        , ( 0xC7, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_0), EightTStates ) )
        , ( 0xCF, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_1), EightTStates ) )
        , ( 0xD7, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_2), EightTStates ) )
        , ( 0xDF, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_3), EightTStates ) )
        , ( 0xE7, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_4), EightTStates ) )
        , ( 0xEF, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_5), EightTStates ) )
        , ( 0xF7, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_6), EightTStates ) )
        , ( 0xFF, ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_7), EightTStates ) )
        ]


rlc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_b z80_main z80_flags =
    z80_flags |> shifter0 z80_main.b |> FlagsWithRegisterChange RegisterB


rlc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    --z80_flags |> shifter_c shifter0 z80_main.c
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
