module TripleWithFlags exposing (..)

import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..))
import Dict exposing (Dict)
import Z80Flags exposing (FlagRegisters, c_FP, c_FS, getFlags)


type TripleWithFlagsChange
    = Skip3ByteInstruction
    | AbsoluteJump Int
    | TripleSetIndirect Int Int
    | AbsoluteCall Int


triple16WithFlags : Dict Int ( Int -> FlagRegisters -> TripleWithFlagsChange, InstructionDuration )
triple16WithFlags =
    Dict.fromList
        [ ( 0x32, ( ld_indirect_nn_a, ThirteenTStates ) )
        , ( 0xC2, ( jp_nz, TenTStates ) )

        -- conditional calls get another 7 if the call is made
        , ( 0xC4, ( call_nz_nn, TenTStates ) )
        , ( 0xCA, ( jp_z_nn, TenTStates ) )
        , ( 0xCC, ( call_z_nn, TenTStates ) )
        , ( 0xD2, ( jp_nc_nn, TenTStates ) )
        , ( 0xD4, ( call_nc_nn, TenTStates ) )
        , ( 0xDA, ( jp_c_nn, TenTStates ) )
        , ( 0xDC, ( call_c_nn, TenTStates ) )
        , ( 0xE2, ( jp_po_nn, TenTStates ) )
        , ( 0xE4, ( call_po_nn, TenTStates ) )
        , ( 0xEA, ( jp_pe_nn, TenTStates ) )
        , ( 0xEC, ( call_pe_nn, TenTStates ) )
        , ( 0xF2, ( jp_p_nn, TenTStates ) )
        , ( 0xF4, ( call_p_nn, TenTStates ) )
        , ( 0xFA, ( jp_m_nn, TenTStates ) )
        , ( 0xFC, ( call_m_nn, TenTStates ) )
        ]


jp_nz : Int -> FlagRegisters -> TripleWithFlagsChange
jp_nz param z80_flags =
    -- case 0xC2: jp(Fr!=0); break;
    --jp_z80 (z80.flags.fr /= 0) z80
    if z80_flags.fr /= 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


jp_z_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_z_nn param z80_flags =
    -- case 0xCA: jp(Fr==0); break;
    --jp_z80 (z80.flags.fr == 0) z80
    if z80_flags.fr == 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


jp_nc_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_nc_nn param z80_flags =
    -- case 0xD2: jp((Ff&0x100)==0); break;
    --z80 |> jp_z80 ((Bitwise.and z80.flags.ff 0x100) == 0)
    if Bitwise.and z80_flags.ff 0x0100 == 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


jp_c_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_c_nn param z80_flags =
    -- case 0xDA: jp((Ff&0x100)!=0); break;
    --z80 |> jp_z80 ((Bitwise.and z80.flags.ff 0x100) /= 0)
    if Bitwise.and z80_flags.ff 0x0100 /= 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


jp_po_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_po_nn param z80_flags =
    -- case 0xE2: jp((flags()&FP)==0); break;
    if Bitwise.and (z80_flags |> getFlags) c_FP == 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


call_po_nn : Int -> FlagRegisters -> TripleWithFlagsChange
call_po_nn param z80_flags =
    -- case 0xE4: call((flags()&FP)==0); break;
    if Bitwise.and (z80_flags |> getFlags) c_FP == 0 then
        AbsoluteCall param

    else
        Skip3ByteInstruction


jp_pe_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_pe_nn param z80_flags =
    -- case 0xEA: jp((flags()&FP)!=0); break;
    if Bitwise.and (z80_flags |> getFlags) c_FP /= 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


call_pe_nn : Int -> FlagRegisters -> TripleWithFlagsChange
call_pe_nn param z80_flags =
    -- case 0xEC: call((flags()&FP)!=0); break;
    if Bitwise.and (z80_flags |> getFlags) c_FP /= 0 then
        AbsoluteCall param

    else
        Skip3ByteInstruction


jp_p_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_p_nn param z80_flags =
    -- case 0xF2: jp((Ff&FS)==0); break;
    if Bitwise.and z80_flags.ff c_FS == 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


call_p_nn : Int -> FlagRegisters -> TripleWithFlagsChange
call_p_nn param z80_flags =
    -- case 0xF4: call((Ff&FS)==0); break;
    if Bitwise.and z80_flags.ff c_FS == 0 then
        AbsoluteCall param

    else
        Skip3ByteInstruction


jp_m_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_m_nn param z80_flags =
    -- case 0xFA: jp((Ff&FS)!=0); break;
    if Bitwise.and z80_flags.ff c_FS /= 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


call_m_nn : Int -> FlagRegisters -> TripleWithFlagsChange
call_m_nn param z80_flags =
    -- case 0xFC: call((Ff&FS)!=0); break;
    if Bitwise.and z80_flags.ff c_FS /= 0 then
        AbsoluteCall param

    else
        Skip3ByteInstruction


ld_indirect_nn_a : Int -> FlagRegisters -> TripleWithFlagsChange
ld_indirect_nn_a param z80_flags =
    -- case 0x32: MP=(v=imm16())+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
    TripleSetIndirect param z80_flags.a


call_nz_nn : Int -> FlagRegisters -> TripleWithFlagsChange
call_nz_nn param z80_flags =
    -- case 0xC4: call(Fr!=0); break;
    if z80_flags.fr /= 0 then
        AbsoluteCall param

    else
        Skip3ByteInstruction


call_z_nn : Int -> FlagRegisters -> TripleWithFlagsChange
call_z_nn param z80_flags =
    -- case 0xCC: call(Fr==0); break;
    --call_z80 (z80.flags.fr == 0) z80
    if z80_flags.fr == 0 then
        AbsoluteCall param

    else
        Skip3ByteInstruction


call_nc_nn : Int -> FlagRegisters -> TripleWithFlagsChange
call_nc_nn param z80_flags =
    -- case 0xD4: call((Ff&0x100)==0); break;
    if Bitwise.and z80_flags.ff 0x0100 == 0 then
        AbsoluteCall param

    else
        Skip3ByteInstruction


call_c_nn : Int -> FlagRegisters -> TripleWithFlagsChange
call_c_nn param z80_flags =
    ---- case 0xDC: call((Ff&0x100)!=0); break;
    --0xDC -> z80 |> call (Bitwise.and z80.flags.ff 0x100 /= 0)
    if Bitwise.and z80_flags.ff 0x0100 /= 0 then
        AbsoluteCall param

    else
        Skip3ByteInstruction
