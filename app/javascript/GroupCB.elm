module GroupCB exposing (..)

import Array exposing (Array)
import Bitwise
import IXIYChange exposing (IXIYChange(..))
import InstructionDuration exposing (InstructionDuration(..))
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


singleByteMainRegsCB80 : Array ( MainWithIndexRegisters -> IXIYChange, InstructionDuration )
singleByteMainRegsCB80 =
    Array.fromList
        [ -- reset bit0
          ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_0), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_0), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_0), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_0), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_0), NumberedTStates 8 )
        , ( resetLbit Bit_0, NumberedTStates 8 )
        , ( resetHLbit Bit_0, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_0), NumberedTStates 8 )

        --bit1
        , ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_1), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_1), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_1), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_1), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_1), NumberedTStates 8 )
        , ( resetLbit Bit_1, NumberedTStates 8 )
        , ( resetHLbit Bit_1, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_1), NumberedTStates 8 )

        --bit2
        , ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_2), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_2), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_2), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_2), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_2), NumberedTStates 8 )
        , ( resetLbit Bit_2, NumberedTStates 8 )
        , ( resetHLbit Bit_2, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_2), NumberedTStates 8 )

        --bit3
        , ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_3), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_3), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_3), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_3), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_3), NumberedTStates 8 )
        , ( resetLbit Bit_3, NumberedTStates 8 )
        , ( resetHLbit Bit_3, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_3), NumberedTStates 8 )

        --bit4
        , ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_4), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_4), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_4), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_4), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_4), NumberedTStates 8 )
        , ( resetLbit Bit_4, NumberedTStates 8 )
        , ( resetHLbit Bit_4, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_4), NumberedTStates 8 )

        --bit5
        , ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_5), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_5), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_5), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_5), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_5), NumberedTStates 8 )
        , ( resetLbit Bit_5, NumberedTStates 8 )
        , ( resetHLbit Bit_5, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_5), NumberedTStates 8 )

        --bit6
        , ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_6), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_6), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_6), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_6), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_6), NumberedTStates 8 )
        , ( resetLbit Bit_6, NumberedTStates 8 )
        , ( resetHLbit Bit_6, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_6), NumberedTStates 8 )

        --bit7
        , ( \z80_main -> TransformMainRegistersCB (resetBbit Bit_7), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetCbit Bit_7), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetDbit Bit_7), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetEbit Bit_7), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (resetHbit Bit_7), NumberedTStates 8 )
        , ( resetLbit Bit_7, NumberedTStates 8 )
        , ( resetHLbit Bit_7, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> resetBit Bit_7), NumberedTStates 8 )

        --t0
        , ( \z80_main -> TransformMainRegistersCB (setBbit Bit_0), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setCbit Bit_0), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setDbit Bit_0), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setEbit Bit_0), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setHbit Bit_0), NumberedTStates 8 )
        , ( setLbit Bit_0, NumberedTStates 8 )
        , ( setHLbit Bit_0, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_0), NumberedTStates 8 )

        --t1
        , ( \z80_main -> TransformMainRegistersCB (setBbit Bit_1), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setCbit Bit_1), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setDbit Bit_1), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setEbit Bit_1), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setHbit Bit_1), NumberedTStates 8 )
        , ( setLbit Bit_1, NumberedTStates 8 )
        , ( setHLbit Bit_1, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_1), NumberedTStates 8 )

        --t2
        , ( \z80_main -> TransformMainRegistersCB (setBbit Bit_2), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setCbit Bit_2), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setDbit Bit_2), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setEbit Bit_2), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setHbit Bit_2), NumberedTStates 8 )
        , ( setLbit Bit_2, NumberedTStates 8 )
        , ( setHLbit Bit_2, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_2), NumberedTStates 8 )

        --t3
        , ( \z80_main -> TransformMainRegistersCB (setBbit Bit_3), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setCbit Bit_3), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setDbit Bit_3), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setEbit Bit_3), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setHbit Bit_3), NumberedTStates 8 )
        , ( setLbit Bit_3, NumberedTStates 8 )
        , ( setHLbit Bit_3, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_3), NumberedTStates 8 )

        -- set bit4
        , ( \z80_main -> TransformMainRegistersCB (setBbit Bit_4), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setCbit Bit_4), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setDbit Bit_4), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setEbit Bit_4), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setHbit Bit_4), NumberedTStates 8 )
        , ( setLbit Bit_4, NumberedTStates 8 )
        , ( setHLbit Bit_4, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_4), NumberedTStates 8 )

        --t5
        , ( \z80_main -> TransformMainRegistersCB (setBbit Bit_5), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setCbit Bit_5), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setDbit Bit_5), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setEbit Bit_5), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setHbit Bit_5), NumberedTStates 8 )
        , ( setLbit Bit_5, NumberedTStates 8 )
        , ( setHLbit Bit_5, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_5), NumberedTStates 8 )

        --t6
        , ( \z80_main -> TransformMainRegistersCB (setBbit Bit_6), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setCbit Bit_6), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setDbit Bit_6), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setEbit Bit_6), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setHbit Bit_6), NumberedTStates 8 )
        , ( setLbit Bit_6, NumberedTStates 8 )
        , ( setHLbit Bit_6, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_6), NumberedTStates 8 )

        --t7
        , ( \z80_main -> TransformMainRegistersCB (setBbit Bit_7), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setCbit Bit_7), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setDbit Bit_7), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setEbit Bit_7), NumberedTStates 8 )
        , ( \z80_main -> TransformMainRegistersCB (setHbit Bit_7), NumberedTStates 8 )
        , ( setLbit Bit_7, NumberedTStates 8 )
        , ( setHLbit Bit_7, NumberedTStates 8 )
        , ( \z80_main -> IXIYFlagChangeFunc (\z80_flags -> z80_flags |> setFlagBit Bit_7), NumberedTStates 8 )
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
            bitMask |> bitMaskFromBit |> Bitwise.or (z80_main |> get_h)
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
        [ ( rlc_b, NumberedTStates 8 )
        , ( rlc_c, NumberedTStates 8 )
        , ( rlc_d, NumberedTStates 8 )
        , ( rlc_e, NumberedTStates 8 )
        , ( rlc_h, NumberedTStates 8 )
        , ( rlc_l, NumberedTStates 8 )

        -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
        , ( \z80_main z80_flags -> RegisterChangeShifter Shifter0 .hl, NumberedTStates 15 )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc rlc_a, NumberedTStates 8 )
        , ( rrc_b, NumberedTStates 8 )
        , ( rrc_c, NumberedTStates 8 )
        , ( rrc_d, NumberedTStates 8 )
        , ( rrc_e, NumberedTStates 8 )
        , ( rrc_h, NumberedTStates 8 )
        , ( rrc_l, NumberedTStates 8 )
        , ( \z80_main z80_flags -> RegisterChangeShifter Shifter1 .hl, NumberedTStates 15 )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc rrc_a, NumberedTStates 8 )
        , ( rl_b, NumberedTStates 8 )
        , ( rl_c, NumberedTStates 8 )
        , ( rl_d, NumberedTStates 8 )
        , ( rl_e, NumberedTStates 8 )
        , ( rl_h, NumberedTStates 8 )
        , ( rl_l, NumberedTStates 8 )
        , ( rl_indirect_hl, NumberedTStates 15 )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc rl_a, NumberedTStates 8 )
        , ( rr_b, NumberedTStates 8 )
        , ( rr_c, NumberedTStates 8 )
        , ( rr_d, NumberedTStates 8 )
        , ( rr_e, NumberedTStates 8 )
        , ( rr_h, NumberedTStates 8 )
        , ( rr_l, NumberedTStates 8 )
        , ( rr_indirect_hl, NumberedTStates 15 )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc rr_a, NumberedTStates 8 )
        , ( sla_b, NumberedTStates 8 )
        , ( sla_c, NumberedTStates 8 )
        , ( sla_d, NumberedTStates 8 )
        , ( sla_e, NumberedTStates 8 )
        , ( sla_h, NumberedTStates 8 )
        , ( sla_l, NumberedTStates 8 )
        , ( sla_indirect_hl, NumberedTStates 15 )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc sla_a, NumberedTStates 8 )
        , ( sra_b, NumberedTStates 8 )
        , ( sra_c, NumberedTStates 8 )
        , ( sra_d, NumberedTStates 8 )
        , ( sra_e, NumberedTStates 8 )
        , ( sra_h, NumberedTStates 8 )
        , ( sra_l, NumberedTStates 8 )
        , ( sra_indirect_hl, NumberedTStates 15 )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc sra_a, NumberedTStates 8 )
        , ( sll_b, NumberedTStates 8 )
        , ( sll_c, NumberedTStates 8 )
        , ( sll_d, NumberedTStates 8 )
        , ( sll_e, NumberedTStates 8 )
        , ( sll_h, NumberedTStates 8 )
        , ( sll_l, NumberedTStates 8 )
        , ( sll_indirect_hl, NumberedTStates 15 )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc sll_a, NumberedTStates 8 )

        -- case 0x00: B=shifter(o,B); break;
        , ( \z80_main z80_flags -> z80_flags |> shifter7 z80_main.b |> FlagsWithRegisterChange RegisterB, NumberedTStates 8 )
        , ( srl_c, NumberedTStates 8 )
        , ( srl_d, NumberedTStates 8 )
        , ( srl_e, NumberedTStates 8 )
        , ( srl_h, NumberedTStates 8 )
        , ( srl_l, NumberedTStates 8 )
        , ( srl_indirect_hl, NumberedTStates 15 )
        , ( \z80_main z80_flags -> Z80FlagChangeFunc srl_a, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.b |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.c |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.d |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.e |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main |> get_h) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( bit_0_indirect_hl, NumberedTStates 12 )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_0 z80_flags.a), NumberedTStates 8 )
        , ( bit_1_b, NumberedTStates 8 )
        , ( bit_1_c, NumberedTStates 8 )
        , ( bit_1_d, NumberedTStates 8 )
        , ( bit_1_e, NumberedTStates 8 )
        , ( bit_1_h, NumberedTStates 8 )
        , ( bit_1_l, NumberedTStates 8 )
        , ( bit_1_indirect_hl, NumberedTStates 12 )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_1 z80_flags.a), NumberedTStates 8 )
        , ( bit_2_b, NumberedTStates 8 )
        , ( bit_2_c, NumberedTStates 8 )
        , ( bit_2_d, NumberedTStates 8 )
        , ( bit_2_e, NumberedTStates 8 )
        , ( bit_2_h, NumberedTStates 8 )
        , ( bit_2_l, NumberedTStates 8 )
        , ( bit_2_indirect_hl, NumberedTStates 12 )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_2 z80_flags.a), NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.b |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.c |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.d |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.e |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main |> get_h) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( bit_3_indirect_hl, NumberedTStates 12 )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_3 z80_flags.a), NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.b |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.c |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.d |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.e |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main |> get_h) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( bit_4_indirect_hl, NumberedTStates 12 )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_4 z80_flags.a), NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.b |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.c |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.d |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.e |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main |> get_h) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( bit_5_indirect_hl, NumberedTStates 12 )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_5 z80_flags.a), NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.b |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.c |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.d |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.e |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main |> get_h) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( bit_6_indirect_hl, NumberedTStates 12 )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_6 z80_flags.a), NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.b |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.c |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.d |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.e |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main |> get_h) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, NumberedTStates 8 )
        , ( bit_7_indirect_hl, NumberedTStates 12 )
        , ( \z80_main flags -> Z80FlagChangeFunc (\z80_flags -> z80_flags |> testBit Bit_7 z80_flags.a), NumberedTStates 8 )
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
        ( value, flags ) =
            shifter1 (z80_main |> get_h) z80_flags

        new_hl =
            Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister flags new_hl


rrc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        ( value, flags ) =
            shifter1 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister flags new_hl


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
        ( value, flags ) =
            shifter2 (z80_main |> get_h) z80_flags

        new_hl =
            Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister flags new_hl


rl_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        ( value, flags ) =
            shifter2 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister flags new_hl


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
        ( value, flags ) =
            shifter3 (z80_main |> get_h) z80_flags

        new_hl =
            Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister flags new_hl


rr_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        ( value, flags ) =
            shifter3 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister flags new_hl


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
        ( value, flags ) =
            shifter4 (z80_main |> get_h) z80_flags

        new_hl =
            Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister flags new_hl


sla_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        ( value, flags ) =
            shifter4 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister flags new_hl


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
        ( value, flags ) =
            shifter5 (z80_main |> get_h) z80_flags

        new_hl =
            Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister flags new_hl


sra_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        ( value, flags ) =
            shifter5 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister flags new_hl


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
        ( value, flags ) =
            shifter6 (z80_main |> get_h) z80_flags

        new_hl =
            Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister flags new_hl


sll_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        ( value, flags ) =
            shifter6 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister flags new_hl


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
        ( value, flags ) =
            shifter7 (z80_main |> get_h) z80_flags

        new_hl =
            Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister flags new_hl


srl_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        ( value, flags ) =
            shifter7 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister flags new_hl


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
