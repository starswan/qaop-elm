module GroupCB exposing (..)

import Array exposing (Array)
import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..))
import IXIYChange exposing (IXIYChange(..))
import SimpleFlagOps exposing (resetBit, rl_a, rlc_a, rr_a, rrc_a, setFlagBit, sla_a, sll_a, sra_a, srl_a)
import Utils exposing (BitTest(..), bitMaskFromBit, inverseBitMaskFromBit, shiftLeftBy8)
import Z80Change exposing (Shifter(..), Z80Change(..))
import Z80Flags exposing (FlagRegisters, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7, testBit)
import Z80Registers exposing (ChangeMainRegister(..), ChangeSingle(..), CoreRegister(..))
import Z80Types exposing (MainWithIndexRegisters, get_h, get_l)


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


singleByteMainRegsCB80 : Array ( IXIYChange, InstructionDuration )
singleByteMainRegsCB80 =
    Array.fromList
        [ -- reset bit0
          ( TransformMainRegistersCB (resetBbit Bit_0), EightTStates )
        , ( TransformMainRegistersCB (resetCbit Bit_0), EightTStates )
        , ( TransformMainRegistersCB (resetDbit Bit_0), EightTStates )
        , ( TransformMainRegistersCB (resetEbit Bit_0), EightTStates )
        , ( TransformMainRegistersCB (resetHbit Bit_0), EightTStates )
        , ( resetLbit Bit_0, EightTStates )
        , ( resetHLbit Bit_0, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_0), EightTStates )

        --bit1
        , ( TransformMainRegistersCB (resetBbit Bit_1), EightTStates )
        , ( TransformMainRegistersCB (resetCbit Bit_1), EightTStates )
        , ( TransformMainRegistersCB (resetDbit Bit_1), EightTStates )
        , ( TransformMainRegistersCB (resetEbit Bit_1), EightTStates )
        , ( TransformMainRegistersCB (resetHbit Bit_1), EightTStates )
        , ( resetLbit Bit_1, EightTStates )
        , ( resetHLbit Bit_1, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_1), EightTStates )

        --bit2
        , ( TransformMainRegistersCB (resetBbit Bit_2), EightTStates )
        , ( TransformMainRegistersCB (resetCbit Bit_2), EightTStates )
        , ( TransformMainRegistersCB (resetDbit Bit_2), EightTStates )
        , ( TransformMainRegistersCB (resetEbit Bit_2), EightTStates )
        , ( TransformMainRegistersCB (resetHbit Bit_2), EightTStates )
        , ( resetLbit Bit_2, EightTStates )
        , ( resetHLbit Bit_2, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_2), EightTStates )

        --bit3
        , ( TransformMainRegistersCB (resetBbit Bit_3), EightTStates )
        , ( TransformMainRegistersCB (resetCbit Bit_3), EightTStates )
        , ( TransformMainRegistersCB (resetDbit Bit_3), EightTStates )
        , ( TransformMainRegistersCB (resetEbit Bit_3), EightTStates )
        , ( TransformMainRegistersCB (resetHbit Bit_3), EightTStates )
        , ( resetLbit Bit_3, EightTStates )
        , ( resetHLbit Bit_3, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_3), EightTStates )

        --bit4
        , ( TransformMainRegistersCB (resetBbit Bit_4), EightTStates )
        , ( TransformMainRegistersCB (resetCbit Bit_4), EightTStates )
        , ( TransformMainRegistersCB (resetDbit Bit_4), EightTStates )
        , ( TransformMainRegistersCB (resetEbit Bit_4), EightTStates )
        , ( TransformMainRegistersCB (resetHbit Bit_4), EightTStates )
        , ( resetLbit Bit_4, EightTStates )
        , ( resetHLbit Bit_4, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_4), EightTStates )

        --bit5
        , ( TransformMainRegistersCB (resetBbit Bit_5), EightTStates )
        , ( TransformMainRegistersCB (resetCbit Bit_5), EightTStates )
        , ( TransformMainRegistersCB (resetDbit Bit_5), EightTStates )
        , ( TransformMainRegistersCB (resetEbit Bit_5), EightTStates )
        , ( TransformMainRegistersCB (resetHbit Bit_5), EightTStates )
        , ( resetLbit Bit_5, EightTStates )
        , ( resetHLbit Bit_5, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_5), EightTStates )

        --bit6
        , ( TransformMainRegistersCB (resetBbit Bit_6), EightTStates )
        , ( TransformMainRegistersCB (resetCbit Bit_6), EightTStates )
        , ( TransformMainRegistersCB (resetDbit Bit_6), EightTStates )
        , ( TransformMainRegistersCB (resetEbit Bit_6), EightTStates )
        , ( TransformMainRegistersCB (resetHbit Bit_6), EightTStates )
        , ( resetLbit Bit_6, EightTStates )
        , ( resetHLbit Bit_6, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_6), EightTStates )

        --bit7
        , ( TransformMainRegistersCB (resetBbit Bit_7), EightTStates )
        , ( TransformMainRegistersCB (resetCbit Bit_7), EightTStates )
        , ( TransformMainRegistersCB (resetDbit Bit_7), EightTStates )
        , ( TransformMainRegistersCB (resetEbit Bit_7), EightTStates )
        , ( TransformMainRegistersCB (resetHbit Bit_7), EightTStates )
        , ( resetLbit Bit_7, EightTStates )
        , ( resetHLbit Bit_7, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_7), EightTStates )

        --t0
        , ( TransformMainRegistersCB (setBbit Bit_0), EightTStates )
        , ( TransformMainRegistersCB (setCbit Bit_0), EightTStates )
        , ( TransformMainRegistersCB (setDbit Bit_0), EightTStates )
        , ( TransformMainRegistersCB (setEbit Bit_0), EightTStates )
        , ( TransformMainRegistersCB (setHbit Bit_0), EightTStates )
        , ( setLbit Bit_0, EightTStates )
        , ( setHLbit Bit_0, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_0), EightTStates )

        --t1
        , ( TransformMainRegistersCB (setBbit Bit_1), EightTStates )
        , ( TransformMainRegistersCB (setCbit Bit_1), EightTStates )
        , ( TransformMainRegistersCB (setDbit Bit_1), EightTStates )
        , ( TransformMainRegistersCB (setEbit Bit_1), EightTStates )
        , ( TransformMainRegistersCB (setHbit Bit_1), EightTStates )
        , ( setLbit Bit_1, EightTStates )
        , ( setHLbit Bit_1, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_1), EightTStates )

        --t2
        , ( TransformMainRegistersCB (setBbit Bit_2), EightTStates )
        , ( TransformMainRegistersCB (setCbit Bit_2), EightTStates )
        , ( TransformMainRegistersCB (setDbit Bit_2), EightTStates )
        , ( TransformMainRegistersCB (setEbit Bit_2), EightTStates )
        , ( TransformMainRegistersCB (setHbit Bit_2), EightTStates )
        , ( setLbit Bit_2, EightTStates )
        , ( setHLbit Bit_2, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_2), EightTStates )

        --t3
        , ( TransformMainRegistersCB (setBbit Bit_3), EightTStates )
        , ( TransformMainRegistersCB (setCbit Bit_3), EightTStates )
        , ( TransformMainRegistersCB (setDbit Bit_3), EightTStates )
        , ( TransformMainRegistersCB (setEbit Bit_3), EightTStates )
        , ( TransformMainRegistersCB (setHbit Bit_3), EightTStates )
        , ( setLbit Bit_3, EightTStates )
        , ( setHLbit Bit_3, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_3), EightTStates )

        -- set bit4
        , ( TransformMainRegistersCB (setBbit Bit_4), EightTStates )
        , ( TransformMainRegistersCB (setCbit Bit_4), EightTStates )
        , ( TransformMainRegistersCB (setDbit Bit_4), EightTStates )
        , ( TransformMainRegistersCB (setEbit Bit_4), EightTStates )
        , ( TransformMainRegistersCB (setHbit Bit_4), EightTStates )
        , ( setLbit Bit_4, EightTStates )
        , ( setHLbit Bit_4, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_4), EightTStates )

        --t5
        , ( TransformMainRegistersCB (setBbit Bit_5), EightTStates )
        , ( TransformMainRegistersCB (setCbit Bit_5), EightTStates )
        , ( TransformMainRegistersCB (setDbit Bit_5), EightTStates )
        , ( TransformMainRegistersCB (setEbit Bit_5), EightTStates )
        , ( TransformMainRegistersCB (setHbit Bit_5), EightTStates )
        , ( setLbit Bit_5, EightTStates )
        , ( setHLbit Bit_5, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_5), EightTStates )

        --t6
        , ( TransformMainRegistersCB (setBbit Bit_6), EightTStates )
        , ( TransformMainRegistersCB (setCbit Bit_6), EightTStates )
        , ( TransformMainRegistersCB (setDbit Bit_6), EightTStates )
        , ( TransformMainRegistersCB (setEbit Bit_6), EightTStates )
        , ( TransformMainRegistersCB (setHbit Bit_6), EightTStates )
        , ( setLbit Bit_6, EightTStates )
        , ( setHLbit Bit_6, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_6), EightTStates )

        --t7
        , ( TransformMainRegistersCB (setBbit Bit_7), EightTStates )
        , ( TransformMainRegistersCB (setCbit Bit_7), EightTStates )
        , ( TransformMainRegistersCB (setDbit Bit_7), EightTStates )
        , ( TransformMainRegistersCB (setEbit Bit_7), EightTStates )
        , ( TransformMainRegistersCB (setHbit Bit_7), EightTStates )
        , ( setLbit Bit_7, EightTStates )
        , ( setHLbit Bit_7, EightTStates )
        , ( IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_7), EightTStates )
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
            bitMask |> inverseBitMaskFromBit |> Bitwise.and (z80_main |> get_h)
    in
    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 new_h) }


resetLbit : BitTest -> IXIYChange
resetLbit bitMask =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange ChangeSingleL (\z80_main -> bitMask |> inverseBitMaskFromBit |> Bitwise.and (z80_main.hl |> Bitwise.and 0xFF))


resetHLbit : BitTest -> IXIYChange
resetHLbit bitMask =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitReset bitMask .hl


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
            bitMask |> bitMaskFromBit |> Bitwise.or (z80_main |> get_h)
    in
    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 new_h) }


setLbit : BitTest -> IXIYChange
setLbit bitMask =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange ChangeSingleL (\z80_main -> bitMask |> bitMaskFromBit |> Bitwise.or (z80_main.hl |> Bitwise.and 0xFF))


setHLbit : BitTest -> IXIYChange
setHLbit bitMask =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitSet bitMask .hl


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
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main |> get_h) |> Z80ChangeFlags, EightTStates )
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
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main |> get_h) |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates )
        , ( bit_3_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_3 z80_flags.a), EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.b |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.c |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.d |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.e |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main |> get_h) |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates )
        , ( bit_4_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_4 z80_flags.a), EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.b |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.c |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.d |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.e |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main |> get_h) |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates )
        , ( bit_5_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_5 z80_flags.a), EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.b |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.c |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.d |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.e |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main |> get_h) |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates )
        , ( bit_6_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_6 z80_flags.a), EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.b |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.c |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.d |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.e |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main |> get_h) |> Z80ChangeFlags, EightTStates )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates )
        , ( bit_7_indirect_hl, TwelveTStates )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_7 z80_flags.a), EightTStates )
        ]


rlc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_b main lags =
    --z80_flags |> shifter0 z80_main.b |> FlagsWithRegisterChange RegisterB
    FlagRegChangeFunc (\z80_main z80_flags -> z80_flags |> shifter0 z80_main.b) ChangeMainB


rlc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_c main flags =
    -- case 0x01: C=shifter(o,C); break;
    FlagRegChangeFunc (\z80_main z80_flags -> z80_flags |> shifter0 z80_main.c) ChangeMainC


rlc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_d main flags =
    -- case 0x02: D=shifter(o,D); break;
    FlagRegChangeFunc (\z80_main z80_flags -> z80_flags |> shifter0 z80_main.d) ChangeMainD


rlc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_e ain flags =
    -- case 0x03: E=shifter(o,E); break;
    FlagRegChangeFunc (\z80_main z80_flags -> z80_flags |> shifter0 z80_main.e) ChangeMainE


rlc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_h zmain z8flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    FlagRegChangeFunc (\z80_main z80_flags -> z80_flags |> shifter0 (z80_main |> get_h)) ChangeMainH


rlc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_l main flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    FlagRegChangeFunc (\z80_main z80_flags -> z80_flags |> shifter0 (z80_main |> get_l)) ChangeMainL


rrc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_b main flags =
    --z80_flags |> shifter1 z80_main.b |> FlagsWithRegisterChange RegisterB
    FlagRegChangeFunc (\z80_main z80_flags -> z80_flags |> shifter1 z80_main.b) ChangeMainB


rrc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_c zmain zlags =
    -- case 0x01: C=shifter(o,C); break;
    --z80_flags |> shifter1 z80_main.c |> FlagsWithRegisterChange RegisterC
    FlagRegChangeFunc (\z80_main z80_flags -> z80_flags |> shifter1 z80_main.c) ChangeMainC


rrc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_d z0_main z8_flags =
    -- case 0x02: D=shifter(o,D); break;
    --z80_flags |> shifter1 z80_main.d |> FlagsWithRegisterChange RegisterD
    FlagRegChangeFunc (\z80_main z80_flags -> z80_flags |> shifter1 z80_main.d) ChangeMainD


rrc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    z80_flags |> shifter1 z80_main.e |> FlagsWithRegisterChange RegisterE


rrc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter1 (z80_main |> get_h) z80_flags

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
            shifter2 (z80_main |> get_h) z80_flags

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
            shifter3 (z80_main |> get_h) z80_flags

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
            shifter4 (z80_main |> get_h) z80_flags

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
            shifter5 (z80_main |> get_h) z80_flags

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
            shifter6 (z80_main |> get_h) z80_flags

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
            shifter7 (z80_main |> get_h) z80_flags

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
    z80_flags |> testBit Bit_1 (z80_main |> get_h) |> Z80ChangeFlags


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
    z80_flags |> testBit Bit_2 (z80_main |> get_h) |> Z80ChangeFlags


bit_2_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_l z80_main z80_flags =
    z80_flags |> testBit Bit_2 (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags
