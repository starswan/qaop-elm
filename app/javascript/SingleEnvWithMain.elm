module SingleEnvWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import Utils exposing (BitTest, shiftLeftBy8, shiftRightBy8)
import Z80Core exposing (Z80Core)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagFunc(..), add16, c_F53, changeFlags, testBit)
import Z80Mem exposing (mem)
import Z80Registers exposing (CoreRegister(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), MainWithIndexRegisters, set_xy)


type SingleEnvMainChange
    = SingleEnvNewARegister Int CpuTimeCTime
    | SingleEnv8BitMain CoreRegister Int CpuTimeCTime
    | SingleEnvNewHLRegister Int CpuTimeCTime
    | IndirectBitTest BitTest Int
    | SingleEnvFlagFunc FlagFunc Int CpuTimeCTime
    | SingleEnvNewHL16BitAdd IXIYHL Int Int


singleEnvMainRegs : Dict Int ( MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegs =
    Dict.fromList
        [ ( 0x0A, ( ld_a_indirect_bc, SevenTStates ) )
        , ( 0x1A, ( ld_a_indirect_de, SevenTStates ) )
        , ( 0x39, ( add_hl_sp, ElevenTStates ) )
        , ( 0x46, ( ld_b_indirect_hl, SevenTStates ) )
        , ( 0x4E, ( ld_c_indirect_hl, SevenTStates ) )
        , ( 0x56, ( ld_d_indirect_hl, SevenTStates ) )
        , ( 0x5E, ( ld_e_indirect_hl, SevenTStates ) )
        , ( 0x66, ( ld_h_indirect_hl, SevenTStates ) )
        , ( 0x6E, ( ld_l_indirect_hl, SevenTStates ) )
        , ( 0x7E, ( ld_a_indirect_hl, SevenTStates ) )
        , ( 0x86, ( add_a_indirect_hl, SevenTStates ) )
        , ( 0x8E, ( adc_a_indirect_hl, SevenTStates ) )
        , ( 0x96, ( sub_indirect_hl, SevenTStates ) )
        , ( 0x9E, ( sbc_indirect_hl, SevenTStates ) )
        , ( 0xA6, ( and_indirect_hl, SevenTStates ) )
        , ( 0xAE, ( xor_indirect_hl, SevenTStates ) )
        , ( 0xB6, ( or_indirect_hl, SevenTStates ) )
        , ( 0xBE, ( cp_indirect_hl, SevenTStates ) )
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


applySingleEnvMainChange : CpuTimeCTime -> SingleEnvMainChange -> Z80ROM -> Z80Core -> Z80Core
applySingleEnvMainChange clockTime z80changeData rom48k z80 =
    case z80changeData of
        SingleEnvNewARegister int cpuTimeCTime ->
            let
                flags =
                    z80.flags
            in
            { z80 | flags = { flags | a = int } }

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
                | main = main_1
            }

        SingleEnvNewHLRegister int cpuTimeCTime ->
            let
                main =
                    z80.main
            in
            { z80
                | main = { main | hl = int }
            }

        IndirectBitTest bitTest mp_address ->
            -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
            let
                ( value_time, value_value ) =
                    z80.env |> mem mp_address clockTime rom48k

                new_flags =
                    z80.flags |> testBit bitTest value_value
            in
            { z80
                | flags = { new_flags | ff = new_flags.ff |> Bitwise.and (Bitwise.complement c_F53) |> Bitwise.or (mp_address |> shiftRightBy8 |> Bitwise.and c_F53) }
            }

        SingleEnvFlagFunc flagFunc int cpuTimeCTime ->
            let
                flags =
                    z80.flags
            in
            { z80
                | flags = flags |> changeFlags flagFunc int
            }

        SingleEnvNewHL16BitAdd ixiyhl hl sp ->
            let
                new_xy =
                    add16 hl sp z80.flags
            in
            { z80
                | flags = new_xy.flags
                , main = z80.main |> set_xy new_xy.value ixiyhl
            }


ld_a_indirect_bc : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_a_indirect_bc z80_main rom48k clockTime z80_env =
    -- case 0x0A: MP=(v=B<<8|C)+1; A=env.mem(v); time+=3; break;
    let
        v =
            Bitwise.or (shiftLeftBy8 z80_main.b) z80_main.c

        ( value_time, value_value ) =
            z80_env |> mem v clockTime rom48k
    in
    SingleEnvNewARegister value_value value_time


ld_a_indirect_de : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_a_indirect_de z80_main rom48k clockTime z80_env =
    -- case 0x1A: MP=(v=D<<8|E)+1; A=env.mem(v); time+=3; break;
    let
        addr =
            Bitwise.or (shiftLeftBy8 z80_main.d) z80_main.e

        ( value_time, value_value ) =
            z80_env |> mem addr clockTime rom48k
    in
    SingleEnvNewARegister value_value value_time


ld_b_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_b_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x46: B=env.mem(HL); time+=3; break;
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_b value_value
    SingleEnv8BitMain RegisterB value_value value_time


ld_c_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_c_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x4E: C=env.mem(HL); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_c value_value
    --MainRegsWithPcAndCpuTime { main | c = value_value } value.pc value_time
    SingleEnv8BitMain RegisterC value_value value_time


ld_d_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_d_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x56: D=env.mem(HL); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_d value_value
    --MainRegsWithPcAndCpuTime { main | d = value_value } value.pc value_time
    SingleEnv8BitMain RegisterD value_value value_time


ld_e_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_e_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x5E: E=env.mem(HL); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_e value_value
    --MainRegsWithPcAndCpuTime { main | e = value_value } value.pc value_time
    SingleEnv8BitMain RegisterE value_value value_time


ld_h_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_h_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x66: HL=HL&0xFF|env.mem(HL)<<8; time+=3; break;
    -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k

        new_hl =
            (z80_main.hl |> Bitwise.and 0xFF) |> Bitwise.or (value_value |> shiftLeftBy8)
    in
    SingleEnvNewHLRegister new_hl value_time


ld_l_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_l_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x6E: HL=HL&0xFF00|env.mem(HL); time+=3; break;
    -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k

        new_hl =
            z80_main.hl |> Bitwise.and 0xFF00 |> Bitwise.or value_value
    in
    SingleEnvNewHLRegister new_hl value_time


ld_a_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
ld_a_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value_time } } |> set_a value_value
    SingleEnvNewARegister value_value value_time


add_a_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
add_a_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x86: add(env.mem(HL)); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    SingleEnvFlagFunc AddA value_value value_time


adc_a_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
adc_a_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x8E: adc(env.mem(HL)); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    SingleEnvFlagFunc AdcA value_value value_time


sub_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
sub_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x96: sub(env.mem(HL)); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    SingleEnvFlagFunc SubA value_value value_time


sbc_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
sbc_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    SingleEnvFlagFunc SbcA value_value value_time


and_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
and_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    SingleEnvFlagFunc AndA value_value value_time


xor_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
xor_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    SingleEnvFlagFunc XorA value_value value_time


or_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
or_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    SingleEnvFlagFunc OrA value_value value_time


cp_indirect_hl : MainWithIndexRegisters -> Z80ROM -> CpuTimeCTime -> Z80Env -> SingleEnvMainChange
cp_indirect_hl z80_main rom48k clockTime z80_env =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    let
        ( value_time, value_value ) =
            z80_env |> mem z80_main.hl clockTime rom48k
    in
    SingleEnvFlagFunc CpA value_value value_time


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
