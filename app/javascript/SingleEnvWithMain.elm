module SingleEnvWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, CpuTimeCTime, CpuTimeSpAnd16BitValue, InstructionDuration(..), addDuration)
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..))
import Utils exposing (BitTest(..), shiftLeftBy8, shiftRightBy8)
import Z80Core exposing (Z80Core)
import Z80Env exposing (Z80Env, mem)
import Z80Flags exposing (FlagFunc(..), FlagRegisters, add16, c_F53, changeFlags, testBit)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), MainWithIndexRegisters, set_xy)


type EightBitMain
    = RegisterB
    | RegisterC
    | RegisterD
    | RegisterE


type SingleEnvMainChange
    = SingleEnvNewARegister Int CpuTimeCTime
    | SingleEnv8BitMain EightBitMain Int CpuTimeCTime
    | SingleEnvNewHLRegister Int CpuTimeCTime
    | IndirectBitTest BitTest Int
    | SingleEnvFlagFunc FlagFunc Int CpuTimeCTime
    | SingleEnvNewHL16BitAdd IXIYHL Int Int


singleEnvMainRegs : Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, PCIncrement, InstructionDuration )
singleEnvMainRegs =
    Dict.fromList
        [ ( 0x0A, ( ld_a_indirect_bc, IncrementByOne, SevenTStates ) )
        , ( 0x1A, ( ld_a_indirect_de, IncrementByOne, SevenTStates ) )
        , ( 0x39, ( add_hl_sp, IncrementByOne, ElevenTStates ) )
        , ( 0x46, ( ld_b_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0x4E, ( ld_c_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0x56, ( ld_d_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0x5E, ( ld_e_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0x66, ( ld_h_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0x6E, ( ld_l_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0x7E, ( ld_a_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0x86, ( add_a_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0x8E, ( adc_a_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0x96, ( sub_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0x9E, ( sbc_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0xA6, ( and_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0xAE, ( xor_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0xB6, ( or_indirect_hl, IncrementByOne, SevenTStates ) )
        , ( 0xBE, ( cp_indirect_hl, IncrementByOne, SevenTStates ) )
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


applySingleEnvMainChange : PCIncrement -> InstructionDuration -> SingleEnvMainChange -> Z80ROM -> Z80Core -> Z80Core
applySingleEnvMainChange pcInc duration z80changeData rom48k z80 =
    let
        env =
            z80.env

        env_1 =
            { env | time = env.time |> addDuration duration }

        new_pc =
            case pcInc of
                IncrementByOne ->
                    Bitwise.and (z80.pc + 1) 0xFFFF

                IncrementByTwo ->
                    Bitwise.and (z80.pc + 2) 0xFFFF

                PCIncrementByFour ->
                    Bitwise.and (z80.pc + 4) 0xFFFF
    in
    case z80changeData of
        SingleEnvNewARegister int cpuTimeCTime ->
            let
                flags =
                    z80.flags
            in
            { z80
                | pc = new_pc
                , flags = { flags | a = int }
                , env = { env_1 | time = cpuTimeCTime }
            }

        SingleEnv8BitMain eightBit int cpuTimeCTime ->
            let
                main =
                    z80.main

                main_1 =
                    case eightBit of
                        RegisterB ->
                            { main | b = int }

                        RegisterC ->
                            { main | c = int }

                        RegisterD ->
                            { main | d = int }

                        RegisterE ->
                            { main | e = int }
            in
            { z80
                | pc = new_pc
                , main = main_1
                , env = { env_1 | time = cpuTimeCTime }
            }

        SingleEnvNewHLRegister int cpuTimeCTime ->
            let
                main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { main | hl = int }
                , env = { env_1 | time = cpuTimeCTime }
            }

        IndirectBitTest bitTest mp_address ->
            -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
            let
                value =
                    z80.env |> mem mp_address z80.env.time rom48k

                new_flags =
                    z80.flags |> testBit bitTest value.value
            in
            { z80
                | pc = new_pc
                , env = { env_1 | time = value.time }
                , flags = { new_flags | ff = new_flags.ff |> Bitwise.and (Bitwise.complement c_F53) |> Bitwise.or (mp_address |> shiftRightBy8 |> Bitwise.and c_F53) }
            }

        SingleEnvFlagFunc flagFunc int cpuTimeCTime ->
            let
                flags =
                    z80.flags
            in
            { z80
                | pc = new_pc
                , flags = flags |> changeFlags flagFunc int
                , env = { env_1 | time = cpuTimeCTime }
            }

        SingleEnvNewHL16BitAdd ixiyhl hl sp ->
            let
                new_xy =
                    add16 hl sp z80.flags
            in
            { z80
                | pc = new_pc
                , env = env_1
                , flags = new_xy.flags
                , main = z80.main |> set_xy new_xy.value ixiyhl
            }


ld_a_indirect_bc : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_a_indirect_bc z80_main rom48k z80_env =
    -- case 0x0A: MP=(v=B<<8|C)+1; A=env.mem(v); time+=3; break;
    let
        v =
            Bitwise.or (shiftLeftBy8 z80_main.b) z80_main.c

        new_a =
            z80_env |> mem v z80_env.time rom48k
    in
    SingleEnvNewARegister new_a.value new_a.time


ld_a_indirect_de : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_a_indirect_de z80_main rom48k z80_env =
    -- case 0x1A: MP=(v=D<<8|E)+1; A=env.mem(v); time+=3; break;
    let
        addr =
            Bitwise.or (shiftLeftBy8 z80_main.d) z80_main.e

        new_a =
            z80_env |> mem addr z80_env.time rom48k
    in
    --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
    --CpuTimeWithFlags env_1 new_flags
    SingleEnvNewARegister new_a.value new_a.time


ld_b_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_b_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: B=env.mem(HL); time+=3; break;
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_b value.value
    SingleEnv8BitMain RegisterB value.value value.time


ld_c_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_c_indirect_hl z80_main rom48k z80_env =
    -- case 0x4E: C=env.mem(HL); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_c value.value
    --MainRegsWithPcAndCpuTime { main | c = value.value } value.pc value.time
    SingleEnv8BitMain RegisterC value.value value.time


ld_d_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_d_indirect_hl z80_main rom48k z80_env =
    -- case 0x56: D=env.mem(HL); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_d value.value
    --MainRegsWithPcAndCpuTime { main | d = value.value } value.pc value.time
    SingleEnv8BitMain RegisterD value.value value.time


ld_e_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_e_indirect_hl z80_main rom48k z80_env =
    -- case 0x5E: E=env.mem(HL); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_e value.value
    --MainRegsWithPcAndCpuTime { main | e = value.value } value.pc value.time
    SingleEnv8BitMain RegisterE value.value value.time


ld_h_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_h_indirect_hl z80_main rom48k z80_env =
    -- case 0x66: HL=HL&0xFF|env.mem(HL)<<8; time+=3; break;
    -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k

        new_hl =
            (z80_main.hl |> Bitwise.and 0xFF) |> Bitwise.or (value.value |> shiftLeftBy8)
    in
    SingleEnvNewHLRegister new_hl value.time


ld_l_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_l_indirect_hl z80_main rom48k z80_env =
    -- case 0x6E: HL=HL&0xFF00|env.mem(HL); time+=3; break;
    -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k

        new_hl =
            z80_main.hl |> Bitwise.and 0xFF00 |> Bitwise.or value.value
    in
    SingleEnvNewHLRegister new_hl value.time


ld_a_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_a_indirect_hl z80_main rom48k z80_env =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
    SingleEnvNewARegister value.value value.time


add_a_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
add_a_indirect_hl z80_main rom48k z80_env =
    -- case 0x86: add(env.mem(HL)); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleEnvFlagFunc AddA value.value value.time


adc_a_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
adc_a_indirect_hl z80_main rom48k z80_env =
    -- case 0x8E: adc(env.mem(HL)); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleEnvFlagFunc AdcA value.value value.time


sub_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
sub_indirect_hl z80_main rom48k z80_env =
    -- case 0x96: sub(env.mem(HL)); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleEnvFlagFunc SubA value.value value.time


sbc_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
sbc_indirect_hl z80_main rom48k z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleEnvFlagFunc SbcA value.value value.time


and_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
and_indirect_hl z80_main rom48k z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleEnvFlagFunc AndA value.value value.time


xor_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
xor_indirect_hl z80_main rom48k z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleEnvFlagFunc XorA value.value value.time


or_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
or_indirect_hl z80_main rom48k z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleEnvFlagFunc OrA value.value value.time


cp_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
cp_indirect_hl z80_main rom48k z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleEnvFlagFunc CpA value.value value.time


add_hl_sp : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
add_hl_sp z80_main rom48k z80_env =
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
