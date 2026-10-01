module SingleEnvWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import Utils exposing (BitTest, shiftRightBy8)
import Z80Core exposing (CoreChange(..), RareCoreChange(..), Z80Core)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagRegisters, add16, c_F53, testBit)
import Z80Mem exposing (getMem8)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), MainWithIndexRegisters, get_bc, get_de, set_xy)


type SingleEnvMainChange
    = SingleEnvNewHL16BitAdd IXIYHL Int Int
    | NewSPValue Int
    | SingleEnvLoadAIndirect (MainWithIndexRegisters -> Int)


singleEnvMainRegs : Dict Int ( MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegs =
    Dict.fromList
        [ ( 0x0A, ( ld_a_indirect_bc, SevenTStates ) )
        , ( 0x1A, ( ld_a_indirect_de, SevenTStates ) )
        , ( 0x33, ( inc_sp, SixTStates ) )
        , ( 0x39, ( add_hl_sp, ElevenTStates ) )
        , ( 0x3B, ( dec_sp, SixTStates ) )
        ]


singleEnvMainRegsIX : Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegsIX =
    Dict.fromList
        [ ( 0x39, ( add_ix_sp, FifteenTStates ) )
        ]


singleEnvMainRegsIY : Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegsIY =
    Dict.fromList
        [ ( 0x39, ( add_iy_sp, FifteenTStates ) )
        ]


applySingleEnvMainChange : CpuTimeCTime -> SingleEnvMainChange -> Z80ROM -> Z80Core -> CoreChange
applySingleEnvMainChange clockTime z80changeData rom48k z80 =
    case z80changeData of
        NewSPValue int ->
            SetStackPointer int |> RareChange

        SingleEnvNewHL16BitAdd ixiyhl hl sp ->
            let
                new_xy =
                    add16 hl sp z80.flags
            in
            ChangeMainAndFlags (z80.main |> set_xy new_xy.value ixiyhl) new_xy.flags

        SingleEnvLoadAIndirect function ->
            let
                v =
                    z80.main |> function

                flags =
                    z80.flags

                ( value, newTime ) =
                    z80.env |> getMem8 v clockTime rom48k
            in
            { flags | a = value } |> FlagsOnly


ld_a_indirect_bc : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_a_indirect_bc z80_main rom48k clockTime z80_env =
    -- case 0x0A: MP=(v=B<<8|C)+1; A=env.mem(v); time+=3; break;
    SingleEnvLoadAIndirect get_bc


ld_a_indirect_de : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_a_indirect_de z80_main rom48k clockTime z80_env =
    -- case 0x1A: MP=(v=D<<8|E)+1; A=env.mem(v); time+=3; break;
    SingleEnvLoadAIndirect get_de


add_hl_sp : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
add_hl_sp z80_main rom48k clockTime z80_env =
    --case 0x39: HL=add16(HL,SP); break;
    SingleEnvNewHL16BitAdd HL z80_main.hl z80_env.sp


add_ix_sp : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
add_ix_sp z80_main rom48k z80_env =
    --case 0x39: xy=add16(xy,SP); break;
    SingleEnvNewHL16BitAdd IX z80_main.ix z80_env.sp


add_iy_sp : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
add_iy_sp z80_main rom48k z80_env =
    --case 0x39: xy=add16(xy,SP); break;
    SingleEnvNewHL16BitAdd IY z80_main.iy z80_env.sp


inc_sp : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
inc_sp z80_main rom48k clockTime z80_env =
    -- case 0x33: SP=(char)(SP+1); time+=2; break;
    NewSPValue (Bitwise.and (z80_env.sp + 1) 0xFFFF)


dec_sp : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
dec_sp z80_main rom48k clockTime z80_env =
    -- case 0x3B: SP=(char)(SP-1); time+=2; break;
    NewSPValue (Bitwise.and (z80_env.sp - 1) 0xFFFF)
