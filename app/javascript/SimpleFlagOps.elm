module SimpleFlagOps exposing (..)

import Bitwise exposing (complement)
import CpuTimeCTime exposing (InstructionDuration(..))
import Dict exposing (Dict)
import Utils exposing (BitTest(..), bitMaskFromBit, inverseBitMaskFromBit, shiftLeftBy8, shiftRightBy8)
import Z80Change exposing (FlagChange(..))
import Z80Flags exposing (FlagRegisters, IntWithFlags, adc, c_FP, c_FS, dec, getFlags, get_af, inc, rot, sbc, scf_ccf, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7, testBit, z80_add, z80_cp, z80_cpl, z80_daa, z80_or, z80_sub, z80_xor)
import Z80Types exposing (MainWithIndexRegisters)


singleByteFlags : Dict Int ( FlagChange, InstructionDuration )
singleByteFlags =
    Dict.fromList
        [ ( 0x07, ( FlagChangeFunc rlca, FourTStates ) )
        , ( 0x0F, ( FlagChangeFunc rrca, FourTStates ) )
        , ( 0x17, ( FlagChangeFunc rla, FourTStates ) )
        , ( 0x1F, ( FlagChangeFunc rra, FourTStates ) )
        , ( 0x27, ( FlagChangeFunc z80_daa, FourTStates ) )
        , ( 0x2F, ( FlagChangeFunc z80_cpl, FourTStates ) )
        , ( 0x37, ( FlagChangeFunc scf, FourTStates ) )
        , ( 0x3C, ( FlagChangeFunc inc_a, FourTStates ) )
        , ( 0x3D, ( FlagChangeFunc dec_a, FourTStates ) )
        , ( 0x3F, ( FlagChangeFunc ccf, FourTStates ) )
        , ( 0x47, ( FlagChangeMain ld_b_a, FourTStates ) )
        , ( 0x4F, ( FlagChangeMain ld_c_a, FourTStates ) )
        , ( 0x57, ( FlagChangeMain ld_d_a, FourTStates ) )
        , ( 0x5F, ( FlagChangeMain ld_e_a, FourTStates ) )
        , ( 0x67, ( FlagChangeMain ld_h_a, FourTStates ) )
        , ( 0x6F, ( FlagChangeMain ld_l_a, FourTStates ) )
        , ( 0x87, ( FlagChangeFunc add_a_a, FourTStates ) )
        , ( 0x8F, ( FlagChangeFunc adc_a_a, FourTStates ) )
        , ( 0x97, ( FlagChangeFunc sub_a, FourTStates ) )
        , ( 0x9F, ( FlagChangeFunc sbc_a, FourTStates ) )
        , ( 0xA7, ( FlagChangeFunc and_a, FourTStates ) )
        , ( 0xAF, ( FlagChangeFunc xor_a, FourTStates ) )
        , ( 0xB7, ( FlagChangeFunc or_a, FourTStates ) )
        , ( 0xBF, ( FlagChangeFunc cp_a, FourTStates ) )

        -- When item popped, z80_pop adds another 6 to make 11
        -- case 0xC0: time++; if(Fr!=0) MP=PC=pop(); break; ret_nz
        , ( 0xC0, ( ConditionalReturn (\flags -> flags.fr /= 0), FiveTStates ) )

        -- case 0xC8: time++; if(Fr==0) MP=PC=pop(); break; ret_z
        , ( 0xC8, ( ConditionalReturn (\flags -> flags.fr == 0), FiveTStates ) )

        -- case 0xD0: time++; if((Ff&0x100)==0) MP=PC=pop(); break; ret_nc
        , ( 0xD0, ( ConditionalReturn (\flags -> Bitwise.and flags.ff 0x0100 == 0), FiveTStates ) )

        -- case 0xD8: time++; if((Ff&0x100)!=0) MP=PC=pop(); break; ret_c
        , ( 0xD8, ( ConditionalReturn (\flags -> Bitwise.and flags.ff 0x0100 /= 0), FiveTStates ) )
        , ( 0xE0, ( ret_po, FiveTStates ) )
        , ( 0xE8, ( ret_pe, FiveTStates ) )
        , ( 0xF0, ( ret_p, FiveTStates ) )

        -- case 0xF5: push(A<<8|flags()); break;
        , ( 0xF5, ( FlagsPushAF, ElevenTStates ) )
        , ( 0xF8, ( ret_m, FiveTStates ) )
        ]


singleByteFlagsDD : Dict Int ( FlagChange, InstructionDuration )
singleByteFlagsDD =
    Dict.fromList
        [ ( 0x47, ( FlagChangeMain ld_b_a, EightTStates ) )
        , ( 0x4F, ( FlagChangeMain ld_c_a, EightTStates ) )
        , ( 0x57, ( FlagChangeMain ld_d_a, EightTStates ) )
        , ( 0x5F, ( FlagChangeMain ld_e_a, EightTStates ) )
        ]


singleByteFlagsFD : Dict Int ( FlagChange, InstructionDuration )
singleByteFlagsFD =
    Dict.fromList
        [ ( 0x47, ( FlagChangeMain ld_b_a, EightTStates ) )
        , ( 0x4F, ( FlagChangeMain ld_c_a, EightTStates ) )
        , ( 0x57, ( FlagChangeMain ld_d_a, EightTStates ) )
        , ( 0x5F, ( FlagChangeMain ld_e_a, EightTStates ) )
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
        , ( 0xC7, ( \z80_flags -> z80_flags |> setFlagBit Bit_0 |> OnlyFlags, EightTStates ) )
        , ( 0xCF, ( \z80_flags -> z80_flags |> setFlagBit Bit_1 |> OnlyFlags, EightTStates ) )
        , ( 0xD7, ( \z80_flags -> z80_flags |> setFlagBit Bit_2 |> OnlyFlags, EightTStates ) )
        , ( 0xDF, ( \z80_flags -> z80_flags |> setFlagBit Bit_3 |> OnlyFlags, EightTStates ) )
        , ( 0xE7, ( \z80_flags -> z80_flags |> setFlagBit Bit_4 |> OnlyFlags, EightTStates ) )
        , ( 0xEF, ( \z80_flags -> z80_flags |> setFlagBit Bit_5 |> OnlyFlags, EightTStates ) )
        , ( 0xF7, ( \z80_flags -> z80_flags |> setFlagBit Bit_6 |> OnlyFlags, EightTStates ) )
        , ( 0xFF, ( \z80_flags -> z80_flags |> setFlagBit Bit_7 |> OnlyFlags, EightTStates ) )
        ]


rlca : FlagRegisters -> FlagRegisters
rlca z80_flags =
    -- case 0x07: rot(A*0x101>>>7); break;
    let
        a101 =
            z80_flags.a + (z80_flags.a |> shiftLeftBy8)
    in
    z80_flags |> rot (a101 |> Bitwise.shiftRightBy 7)


rrca : FlagRegisters -> FlagRegisters
rrca z80_flags =
    -- case 0x0F: rot(A*0x80800000>>24); break;
    z80_flags |> rot (Bitwise.shiftRightBy 24 (z80_flags.a * 0x80800000))


rla : FlagRegisters -> FlagRegisters
rla z80_flags =
    -- case 0x17: rot(A<<1|Ff>>>8&1); break;
    z80_flags |> rot (Bitwise.or (Bitwise.shiftLeftBy 1 z80_flags.a) (Bitwise.and (shiftRightBy8 z80_flags.ff) 1))


rra : FlagRegisters -> FlagRegisters
rra z80_flags =
    -- case 0x1F: rot((A*0x201|Ff&0x100)>>>1); break;
    z80_flags |> rot (Bitwise.shiftRightBy 1 (Bitwise.or (z80_flags.a * 0x0201) (Bitwise.and z80_flags.ff 0x0100)))


scf : FlagRegisters -> FlagRegisters
scf z80_flags =
    -- case 0x37: scf_ccf(0); break;
    z80_flags |> scf_ccf 0


inc_a : FlagRegisters -> FlagRegisters
inc_a z80_flags =
    -- case 0x3C: A=inc(A); break;
    let
        v =
            inc z80_flags.a z80_flags

        new_flags =
            v.flags
    in
    { new_flags | a = v.value }


dec_a : FlagRegisters -> FlagRegisters
dec_a z80_flags =
    -- case 0x3D: A=dec(A); break;
    let
        v =
            dec z80_flags.a z80_flags

        new_flags =
            v.flags
    in
    { new_flags | a = v.value }


ccf : FlagRegisters -> FlagRegisters
ccf z80_flags =
    -- case 0x3F: scf_ccf(Ff&0x100); break;
    z80_flags |> scf_ccf (Bitwise.and z80_flags.ff 0x0100)


ld_b_a : FlagRegisters -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_b_a z80_flags z80_main =
    -- case 0x47: B=A; break;
    --FlagChange8Bit RegisterB z80_flags.a
    { z80_main | b = z80_flags.a }


ld_c_a : FlagRegisters -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_c_a z80_flags z80_main =
    -- case 0x4F: C=A; break;
    --FlagChange8Bit RegisterC z80_flags.a
    { z80_main | c = z80_flags.a }


ld_d_a : FlagRegisters -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_d_a z80_flags z80_main =
    -- case 0x57: D=A; break;
    --FlagChange8Bit RegisterD z80_flags.a
    { z80_main | d = z80_flags.a }


ld_e_a : FlagRegisters -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_e_a z80_flags z80_main =
    -- case 0x5F: E=A; break;
    --FlagChange8Bit RegisterE z80_flags.a
    { z80_main | e = z80_flags.a }


ld_h_a : FlagRegisters -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_h_a z80_flags main =
    -- case 0x67: HL=HL&0xFF|A<<8; break;
    -- case 0x67: xy=xy&0xFF|A<<8; break;
    --FlagChangeH z80_flags.a
    { main | hl = Bitwise.or (shiftLeftBy8 z80_flags.a) (Bitwise.and main.hl 0xFF) }


ld_l_a : FlagRegisters -> MainWithIndexRegisters -> MainWithIndexRegisters
ld_l_a z80_flags main =
    -- case 0x6F: HL=HL&0xFF00|A; break;
    -- case 0x6F: xy=xy&0xFF00|A; break;
    --FlagChangeL z80_flags.a
    { main | hl = Bitwise.or z80_flags.a (Bitwise.and main.hl 0xFF00) }


add_a_a : FlagRegisters -> FlagRegisters
add_a_a z80_flags =
    -- case 0x87: add(A); break;
    z80_flags |> z80_add z80_flags.a


adc_a_a : FlagRegisters -> FlagRegisters
adc_a_a z80_flags =
    -- case 0x8F: adc(A); break;
    z80_flags |> adc z80_flags.a


sub_a : FlagRegisters -> FlagRegisters
sub_a z80_flags =
    -- case 0x97: sub(A); break;
    z80_flags |> z80_sub z80_flags.a


sbc_a : FlagRegisters -> FlagRegisters
sbc_a z80_flags =
    -- case 0x9F: sbc(A); break;
    z80_flags |> sbc z80_flags.a


and_a : FlagRegisters -> FlagRegisters
and_a z80_flags =
    -- case 0xA7: Fa=~(Ff=Fr=A); Fb=0; break;
    -- and a is correct - I guess the above is a faster implementation
    { a = z80_flags.a, ff = z80_flags.a, fr = z80_flags.a, fb = 0, fa = complement z80_flags.a }


xor_a : FlagRegisters -> FlagRegisters
xor_a z80_flags =
    -- case 0xAF: A=Ff=Fr=Fb=0; Fa=0x100; break;
    z80_flags |> z80_xor z80_flags.a


or_a : FlagRegisters -> FlagRegisters
or_a z80_flags =
    -- case 0xB7: or(A); break;
    z80_flags |> z80_or z80_flags.a


cp_a : FlagRegisters -> FlagRegisters
cp_a z80_flags =
    -- case 0xBF: cp(A); break;
    z80_flags |> z80_cp z80_flags.a


ret_po : FlagChange
ret_po =
    -- case 0xE0: time++; if((flags()&FP)==0) MP=PC=pop(); break;
    ConditionalReturn (\flags -> Bitwise.and (flags |> getFlags) c_FP == 0)


ret_pe : FlagChange
ret_pe =
    -- case 0xE8: time++; if((flags()&FP)!=0) MP=PC=pop(); break;
    ConditionalReturn (\flags -> Bitwise.and (flags |> getFlags) c_FP /= 0)


ret_p : FlagChange
ret_p =
    -- case 0xF0: time++; if((Ff&FS)==0) MP=PC=pop(); break;
    ConditionalReturn (\flags -> Bitwise.and flags.ff c_FS == 0)


ret_m : FlagChange
ret_m =
    -- case 0xF8: time++; if((Ff&FS)!=0) MP=PC=pop(); break;
    ConditionalReturn (\flags -> Bitwise.and flags.ff c_FS /= 0)


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


setFlagBit : BitTest -> FlagRegisters -> FlagRegisters
setFlagBit testType flagRegs =
    let
        new_a =
            testType |> bitMaskFromBit |> Bitwise.or flagRegs.a
    in
    { flagRegs | a = new_a }
