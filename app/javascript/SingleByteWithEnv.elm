module SingleByteWithEnv exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import Z80Core exposing (Z80Core)
import Z80Env exposing (Z80Env)


type SingleByteEnvChange
    = NewSPValue Int


singleByteZ80Env : Dict Int ( Z80Env -> SingleByteEnvChange, InstructionDuration )
singleByteZ80Env =
    Dict.fromList
        [ ( 0x33, ( inc_sp, SixTStates ) )
        , ( 0x3B, ( dec_sp, SixTStates ) )
        ]


applyEnvChangeDelta : CpuTimeCTime -> SingleByteEnvChange -> Z80Core -> Z80Core
applyEnvChangeDelta cpu_time z80changeData z80 =
    case z80changeData of
        NewSPValue int ->
            let
                new_pc =
                    Bitwise.and (z80.pc + 1) 0xFFFF

                env =
                    z80.env
            in
            { z80
                | pc = new_pc
                , env = { env | sp = int }
            }


inc_sp : Z80Env -> SingleByteEnvChange
inc_sp z80_env =
    -- case 0x33: SP=(char)(SP+1); time+=2; break;
    --let
    --    new_sp =
    --        Bitwise.and (z80.env.sp + 1) 0xFFFF
    --in
    NewSPValue (Bitwise.and (z80_env.sp + 1) 0xFFFF)


dec_sp : Z80Env -> SingleByteEnvChange
dec_sp z80_env =
    -- case 0x3B: SP=(char)(SP-1); time+=2; break;
    --let
    --    new_sp =
    --        Bitwise.and (z80.env.sp - 1) 0xFFFF
    --in
    NewSPValue (Bitwise.and (z80_env.sp - 1) 0xFFFF)
