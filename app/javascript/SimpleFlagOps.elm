module SimpleFlagOps exposing (..)

import Bitwise exposing (complement)
import CpuTimeCTime exposing (CpuTimeIncrement(..), InstructionDuration(..))
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..))
import SingleEnvWithMain exposing (EightBitMain(..))
import Utils exposing (BitTest(..), bitMaskFromBit, inverseBitMaskFromBit, shiftLeftBy8, shiftRightBy8)
import Z80Change exposing (FlagChange(..))
import Z80Flags exposing (FlagRegisters, IntWithFlags, adc, c_FP, c_FS, cpl, daa, dec, getFlags, get_af, inc, rot, sbc, scf_ccf, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7, testBit, z80_add, z80_cp, z80_or, z80_sub, z80_xor)


singleByteFlags : Dict Int ( FlagRegisters -> FlagChange, PCIncrement, InstructionDuration )
singleByteFlags =
    Dict.fromList
        [ ( 0x07, ( rlca, IncrementByOne, FourTStates ) )
        , ( 0x0F, ( rrca, IncrementByOne, FourTStates ) )
        , ( 0x17, ( rla, IncrementByOne, FourTStates ) )
        , ( 0x1F, ( rra, IncrementByOne, FourTStates ) )
        , ( 0x27, ( z80_daa, IncrementByOne, FourTStates ) )
        , ( 0x2F, ( z80_cpl, IncrementByOne, FourTStates ) )
        , ( 0x37, ( scf, IncrementByOne, FourTStates ) )
        , ( 0x3C, ( inc_a, IncrementByOne, FourTStates ) )
        , ( 0x3D, ( dec_a, IncrementByOne, FourTStates ) )
        , ( 0x3F, ( ccf, IncrementByOne, FourTStates ) )
        , ( 0x47, ( ld_b_a, IncrementByOne, FourTStates ) )
        , ( 0x4F, ( ld_c_a, IncrementByOne, FourTStates ) )
        , ( 0x57, ( ld_d_a, IncrementByOne, FourTStates ) )
        , ( 0x5F, ( ld_e_a, IncrementByOne, FourTStates ) )
        , ( 0x67, ( ld_h_a, IncrementByOne, FourTStates ) )
        , ( 0x6F, ( ld_l_a, IncrementByOne, FourTStates ) )
        , ( 0x87, ( add_a_a, IncrementByOne, FourTStates ) )
        , ( 0x8F, ( adc_a_a, IncrementByOne, FourTStates ) )
        , ( 0x97, ( sub_a, IncrementByOne, FourTStates ) )
        , ( 0x9F, ( sbc_a, IncrementByOne, FourTStates ) )
        , ( 0xA7, ( and_a, IncrementByOne, FourTStates ) )
        , ( 0xAF, ( xor_a, IncrementByOne, FourTStates ) )
        , ( 0xB7, ( or_a, IncrementByOne, FourTStates ) )
        , ( 0xBF, ( cp_a, IncrementByOne, FourTStates ) )

        -- When item popped, z80_pop adds another 6 to make 11
        , ( 0xC0, ( ret_nz, IncrementByOne, FiveTStates ) )
        , ( 0xC8, ( ret_z, IncrementByOne, FiveTStates ) )
        , ( 0xD0, ( ret_nc, IncrementByOne, FiveTStates ) )
        , ( 0xD8, ( ret_c, IncrementByOne, FiveTStates ) )
        , ( 0xE0, ( ret_po, IncrementByOne, FiveTStates ) )
        , ( 0xE8, ( ret_pe, IncrementByOne, FiveTStates ) )
        , ( 0xF0, ( ret_p, IncrementByOne, FiveTStates ) )
        , ( 0xF5, ( push_af, IncrementByOne, ElevenTStates ) )
        , ( 0xF8, ( ret_m, IncrementByOne, FiveTStates ) )
        ]


singleByteFlagsDD : Dict Int ( FlagRegisters -> FlagChange, InstructionDuration )
singleByteFlagsDD =
    Dict.fromList
        [ ( 0x47, ( ld_b_a, EightTStates ) )
        , ( 0x4F, ( ld_c_a, EightTStates ) )
        , ( 0x57, ( ld_d_a, EightTStates ) )
        , ( 0x5F, ( ld_e_a, EightTStates ) )
        ]


singleByteFlagsFD : Dict Int ( FlagRegisters -> FlagChange, InstructionDuration )
singleByteFlagsFD =
    Dict.fromList
        [ ( 0x47, ( ld_b_a, EightTStates ) )
        , ( 0x4F, ( ld_c_a, EightTStates ) )
        , ( 0x57, ( ld_d_a, EightTStates ) )
        , ( 0x5F, ( ld_e_a, EightTStates ) )
        ]


singleByteFlagsCB : Dict Int ( FlagRegisters -> FlagChange, InstructionDuration )
singleByteFlagsCB =
    Dict.fromList
        [ ( 0x07, ( rlc_a, EightTStates ) )
        , ( 0x0F, ( rrc_a, EightTStates ) )
        , ( 0x17, ( rl_a, EightTStates ) )
        , ( 0x1F, ( rr_a, EightTStates ) )
        , ( 0x27, ( sla_a, EightTStates ) )
        , ( 0x2F, ( sra_a, EightTStates ) )
        , ( 0x37, ( sll_a, EightTStates ) )
        , ( 0x3F, ( srl_a, EightTStates ) )
        , ( 0x47, ( \z80_flags -> z80_flags |> testBit Bit_0 z80_flags.a |> OnlyFlags, EightTStates ) )
        , ( 0x4F, ( \z80_flags -> z80_flags |> testBit Bit_1 z80_flags.a |> OnlyFlags, EightTStates ) )
        , ( 0x57, ( \z80_flags -> z80_flags |> testBit Bit_2 z80_flags.a |> OnlyFlags, EightTStates ) )
        , ( 0x5F, ( \z80_flags -> z80_flags |> testBit Bit_3 z80_flags.a |> OnlyFlags, EightTStates ) )
        , ( 0x67, ( \z80_flags -> z80_flags |> testBit Bit_4 z80_flags.a |> OnlyFlags, EightTStates ) )
        , ( 0x6F, ( \z80_flags -> z80_flags |> testBit Bit_5 z80_flags.a |> OnlyFlags, EightTStates ) )
        , ( 0x77, ( \z80_flags -> z80_flags |> testBit Bit_6 z80_flags.a |> OnlyFlags, EightTStates ) )
        , ( 0x7F, ( \z80_flags -> z80_flags |> testBit Bit_7 z80_flags.a |> OnlyFlags, EightTStates ) )
        , ( 0x87, ( \z80_flags -> z80_flags |> resetBit Bit_0 |> OnlyFlags, EightTStates ) )
        , ( 0x8F, ( \z80_flags -> z80_flags |> resetBit Bit_1 |> OnlyFlags, EightTStates ) )
        , ( 0x97, ( \z80_flags -> z80_flags |> resetBit Bit_2 |> OnlyFlags, EightTStates ) )
        , ( 0x9F, ( \z80_flags -> z80_flags |> resetBit Bit_3 |> OnlyFlags, EightTStates ) )
        , ( 0xA7, ( \z80_flags -> z80_flags |> resetBit Bit_4 |> OnlyFlags, EightTStates ) )
        , ( 0xAF, ( \z80_flags -> z80_flags |> resetBit Bit_5 |> OnlyFlags, EightTStates ) )
        , ( 0xB7, ( \z80_flags -> z80_flags |> resetBit Bit_6 |> OnlyFlags, EightTStates ) )
        , ( 0xBF, ( \z80_flags -> z80_flags |> resetBit Bit_7 |> OnlyFlags, EightTStates ) )
        , ( 0xC7, ( \z80_flags -> z80_flags |> setBit Bit_0 |> OnlyFlags, EightTStates ) )
        , ( 0xCF, ( \z80_flags -> z80_flags |> setBit Bit_1 |> OnlyFlags, EightTStates ) )
        , ( 0xD7, ( \z80_flags -> z80_flags |> setBit Bit_2 |> OnlyFlags, EightTStates ) )
        , ( 0xDF, ( \z80_flags -> z80_flags |> setBit Bit_3 |> OnlyFlags, EightTStates ) )
        , ( 0xE7, ( \z80_flags -> z80_flags |> setBit Bit_4 |> OnlyFlags, EightTStates ) )
        , ( 0xEF, ( \z80_flags -> z80_flags |> setBit Bit_5 |> OnlyFlags, EightTStates ) )
        , ( 0xF7, ( \z80_flags -> z80_flags |> setBit Bit_6 |> OnlyFlags, EightTStates ) )
        , ( 0xFF, ( \z80_flags -> z80_flags |> setBit Bit_7 |> OnlyFlags, EightTStates ) )
        ]


rlca : FlagRegisters -> FlagChange
rlca z80_flags =
    -- case 0x07: rot(A*0x101>>>7); break;
    let
        a101 =
            z80_flags.a + (z80_flags.a |> shiftLeftBy8)
    in
    z80_flags |> rot (a101 |> Bitwise.shiftRightBy 7) |> OnlyFlags


rrca : FlagRegisters -> FlagChange
rrca z80_flags =
    -- case 0x0F: rot(A*0x80800000>>24); break;
    z80_flags |> rot (Bitwise.shiftRightBy 24 (z80_flags.a * 0x80800000)) |> OnlyFlags


rla : FlagRegisters -> FlagChange
rla z80_flags =
    -- case 0x17: rot(A<<1|Ff>>>8&1); break;
    z80_flags |> rot (Bitwise.or (Bitwise.shiftLeftBy 1 z80_flags.a) (Bitwise.and (shiftRightBy8 z80_flags.ff) 1)) |> OnlyFlags


rra : FlagRegisters -> FlagChange
rra z80_flags =
    -- case 0x1F: rot((A*0x201|Ff&0x100)>>>1); break;
    z80_flags |> rot (Bitwise.shiftRightBy 1 (Bitwise.or (z80_flags.a * 0x0201) (Bitwise.and z80_flags.ff 0x0100))) |> OnlyFlags


scf : FlagRegisters -> FlagChange
scf z80_flags =
    -- case 0x37: scf_ccf(0); break;
    z80_flags |> scf_ccf 0 |> OnlyFlags


inc_a : FlagRegisters -> FlagChange
inc_a z80_flags =
    -- case 0x3C: A=inc(A); break;
    let
        v =
            inc z80_flags.a z80_flags

        new_flags =
            v.flags
    in
    { new_flags | a = v.value } |> OnlyFlags


dec_a : FlagRegisters -> FlagChange
dec_a z80_flags =
    -- case 0x3D: A=dec(A); break;
    let
        v =
            dec z80_flags.a z80_flags

        new_flags =
            v.flags
    in
    { new_flags | a = v.value } |> OnlyFlags


ccf : FlagRegisters -> FlagChange
ccf z80_flags =
    -- case 0x3F: scf_ccf(Ff&0x100); break;
    z80_flags |> scf_ccf (Bitwise.and z80_flags.ff 0x0100) |> OnlyFlags


ld_b_a : FlagRegisters -> FlagChange
ld_b_a z80_flags =
    -- case 0x47: B=A; break;
    FlagChange8Bit RegisterB z80_flags.a


ld_c_a : FlagRegisters -> FlagChange
ld_c_a z80_flags =
    -- case 0x4F: C=A; break;
    FlagChange8Bit RegisterC z80_flags.a


z80_daa : FlagRegisters -> FlagChange
z80_daa z80_flags =
    -- case 0x27: daa(); break;
    z80_flags |> daa |> OnlyFlags


z80_cpl : FlagRegisters -> FlagChange
z80_cpl z80_flags =
    z80_flags |> cpl |> OnlyFlags


ld_d_a : FlagRegisters -> FlagChange
ld_d_a z80_flags =
    -- case 0x57: D=A; break;
    FlagChange8Bit RegisterD z80_flags.a


ld_e_a : FlagRegisters -> FlagChange
ld_e_a z80_flags =
    -- case 0x5F: E=A; break;
    FlagChange8Bit RegisterE z80_flags.a


ld_h_a : FlagRegisters -> FlagChange
ld_h_a z80_flags =
    -- case 0x67: HL=HL&0xFF|A<<8; break;
    -- case 0x67: xy=xy&0xFF|A<<8; break;
    FlagChangeH z80_flags.a


ld_l_a : FlagRegisters -> FlagChange
ld_l_a z80_flags =
    -- case 0x6F: HL=HL&0xFF00|A; break;
    -- case 0x6F: xy=xy&0xFF00|A; break;
    FlagChangeL z80_flags.a


add_a_a : FlagRegisters -> FlagChange
add_a_a z80_flags =
    -- case 0x87: add(A); break;
    z80_flags |> z80_add z80_flags.a |> OnlyFlags


adc_a_a : FlagRegisters -> FlagChange
adc_a_a z80_flags =
    -- case 0x8F: adc(A); break;
    z80_flags |> adc z80_flags.a |> OnlyFlags


sub_a : FlagRegisters -> FlagChange
sub_a z80_flags =
    -- case 0x97: sub(A); break;
    --z80 |> set_flag_regs (z80_sub z80.flags.a z80.flags)
    z80_flags |> z80_sub z80_flags.a |> OnlyFlags


sbc_a : FlagRegisters -> FlagChange
sbc_a z80_flags =
    -- case 0x9F: sbc(A); break;
    --z80 |> set_flag_regs (sbc z80.flags.a z80.flags)
    z80_flags |> sbc z80_flags.a |> OnlyFlags


and_a : FlagRegisters -> FlagChange
and_a z80_flags =
    -- case 0xA7: Fa=~(Ff=Fr=A); Fb=0; break;
    -- and a is correct - I guess the above is a faster implementation
    --z80_flags |> z80_and z80_flags.a |> OnlyFlags
    OnlyFlags { a = z80_flags.a, ff = z80_flags.a, fr = z80_flags.a, fb = 0, fa = complement z80_flags.a }


xor_a : FlagRegisters -> FlagChange
xor_a z80_flags =
    -- case 0xAF: A=Ff=Fr=Fb=0; Fa=0x100; break;
    z80_flags |> z80_xor z80_flags.a |> OnlyFlags


or_a : FlagRegisters -> FlagChange
or_a z80_flags =
    -- case 0xB7: or(A); break;
    --z80 |> set_flag_regs (z80_or z80.flags.a z80.flags)
    z80_flags |> z80_or z80_flags.a |> OnlyFlags


cp_a : FlagRegisters -> FlagChange
cp_a z80_flags =
    -- case 0xBF: cp(A); break;
    --z80 |> set_flag_regs (cp z80.flags.a z80.flags)
    z80_flags |> z80_cp z80_flags.a |> OnlyFlags


ret_nz : FlagRegisters -> FlagChange
ret_nz z80_flags =
    -- case 0xC0: time++; if(Fr!=0) MP=PC=pop(); break;
    if z80_flags.fr /= 0 then
        ReturnWithPop

    else
        EmptyFlagChange


ret_z : FlagRegisters -> FlagChange
ret_z z80_flags =
    -- case 0xC8: time++; if(Fr==0) MP=PC=pop(); break;
    if z80_flags.fr == 0 then
        ReturnWithPop

    else
        EmptyFlagChange


ret_nc : FlagRegisters -> FlagChange
ret_nc z80_flags =
    -- case 0xD0: time++; if((Ff&0x100)==0) MP=PC=pop(); break;
    if Bitwise.and z80_flags.ff 0x0100 == 0 then
        ReturnWithPop

    else
        EmptyFlagChange


ret_c : FlagRegisters -> FlagChange
ret_c z80_flags =
    -- case 0xD8: time++; if((Ff&0x100)!=0) MP=PC=pop(); break;
    if Bitwise.and z80_flags.ff 0x0100 /= 0 then
        ReturnWithPop

    else
        EmptyFlagChange


ret_po : FlagRegisters -> FlagChange
ret_po z80_flags =
    -- case 0xE0: time++; if((flags()&FP)==0) MP=PC=pop(); break;
    if Bitwise.and (z80_flags |> getFlags) c_FP == 0 then
        ReturnWithPop

    else
        EmptyFlagChange


ret_pe : FlagRegisters -> FlagChange
ret_pe z80_flags =
    -- case 0xE8: time++; if((flags()&FP)!=0) MP=PC=pop(); break;
    if Bitwise.and (z80_flags |> getFlags) c_FP /= 0 then
        ReturnWithPop

    else
        EmptyFlagChange


ret_p : FlagRegisters -> FlagChange
ret_p z80_flags =
    -- case 0xF0: time++; if((Ff&FS)==0) MP=PC=pop(); break;
    if Bitwise.and z80_flags.ff c_FS == 0 then
        ReturnWithPop

    else
        EmptyFlagChange


ret_m : FlagRegisters -> FlagChange
ret_m z80_flags =
    -- case 0xF8: time++; if((Ff&FS)!=0) MP=PC=pop(); break;
    if Bitwise.and z80_flags.ff c_FS /= 0 then
        ReturnWithPop

    else
        EmptyFlagChange


push_af : FlagRegisters -> FlagChange
push_af z80_flags =
    -- case 0xF5: push(A<<8|flags()); break;
    FlagChangePush (z80_flags |> get_af)


applyFlagShifter : (Int -> FlagRegisters -> IntWithFlags) -> FlagRegisters -> FlagChange
applyFlagShifter shifter z80_flags =
    --case 0x07: A=shifter(o,A); break;
    let
        value =
            shifter z80_flags.a z80_flags

        new_flags =
            value.flags
    in
    OnlyFlags { new_flags | a = value.value }


rlc_a : FlagRegisters -> FlagChange
rlc_a z80_flags =
    applyFlagShifter shifter0 z80_flags


rrc_a : FlagRegisters -> FlagChange
rrc_a z80_flags =
    applyFlagShifter shifter1 z80_flags


rl_a : FlagRegisters -> FlagChange
rl_a z80_flags =
    applyFlagShifter shifter2 z80_flags


rr_a : FlagRegisters -> FlagChange
rr_a z80_flags =
    applyFlagShifter shifter3 z80_flags


sla_a : FlagRegisters -> FlagChange
sla_a z80_flags =
    applyFlagShifter shifter4 z80_flags


sra_a : FlagRegisters -> FlagChange
sra_a z80_flags =
    applyFlagShifter shifter5 z80_flags


sll_a : FlagRegisters -> FlagChange
sll_a z80_flags =
    applyFlagShifter shifter6 z80_flags


srl_a : FlagRegisters -> FlagChange
srl_a z80_flags =
    applyFlagShifter shifter7 z80_flags


resetBit : BitTest -> FlagRegisters -> FlagRegisters
resetBit testType flagRegs =
    let
        new_a =
            testType |> inverseBitMaskFromBit |> Bitwise.and flagRegs.a
    in
    { flagRegs | a = new_a }


setBit : BitTest -> FlagRegisters -> FlagRegisters
setBit testType flagRegs =
    let
        new_a =
            testType |> bitMaskFromBit |> Bitwise.or flagRegs.a
    in
    { flagRegs | a = new_a }
