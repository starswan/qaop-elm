module SingleWith8BitParameter exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement, addCpuTimeTime)
import Dict exposing (Dict)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..))
import TransformTypes exposing (InstructionDuration(..))
import Utils exposing (byte, shiftLeftBy8)
import Z80Env exposing (Z80Env, mem)
import Z80Flags exposing (FlagRegisters, adc, sbc, z80_add, z80_and, z80_cp, z80_or, z80_sub, z80_xor)
import Z80Rom exposing (Z80ROM)
import Z80Transform exposing (ChangeEnvOperation(..), ChangeMainOperation(..), ChangeMemoryOperation(..), InstructionLength(..), Z80Operation(..), Z80Transform)
import Z80Types exposing (MainWithIndexRegisters, Z80)


singleWith8BitParam : Dict Int ( Int -> Single8BitChange, MediumPCIncrement )
singleWith8BitParam =
    Dict.fromList
        [ ( 0x06, ( ld_b_n, IncreaseByTwo ) )
        , ( 0x0E, ( ld_c_n, IncreaseByTwo ) )
        , ( 0x16, ( ld_d_n, IncreaseByTwo ) )
        , ( 0x1E, ( ld_e_n, IncreaseByTwo ) )
        ]


standardDouble : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, MediumPCIncrement )
standardDouble =
    Dict.fromList
        [ ( 0x10, ( djnz, IncreaseByTwo ) )
        , ( 0x36, ( ld_indirect_hl_n, IncreaseByTwo ) )
        , ( 0x26, ( ld_h_n, IncreaseByTwo ) )
        , ( 0x2E, ( ld_l_n, IncreaseByTwo ) )
        ]


ixDouble : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, MediumPCIncrement )
ixDouble =
    Dict.fromList
        [ ( 0xDD26, ( ld_ix_h_n, IncreaseByThree ) )
        , ( 0xDD2E, ( ld_ix_l_n, IncreaseByThree ) )
        , ( 0xDD34, ( inc_indirect_ix, IncreaseByThree ) )
        , ( 0xDD35, ( dec_indirect_ix, IncreaseByThree ) )
        , ( 0xDD46, ( ld_b_indirect_ix, IncreaseByThree ) )
        , ( 0xDD4E, ( ld_c_indirect_ix, IncreaseByThree ) )
        , ( 0xDD56, ( ld_d_indirect_ix, IncreaseByThree ) )
        , ( 0xDD5E, ( ld_e_indirect_ix, IncreaseByThree ) )
        , ( 0xDD66, ( \z80_main param -> NewHIndexedIndirect z80_main.ix param, IncreaseByThree ) )
        , ( 0xDD6E, ( \z80_main param -> NewLIndexedIndirect z80_main.ix param, IncreaseByThree ) )
        , ( 0xDD86, ( add_a_indirect_ix, IncreaseByThree ) )

        -- case 0x8E: adc(env.mem(HL)); time+=3; break;
        , ( 0xDD8E, ( \z80_main param -> AdcIndexedIndirect z80_main.ix param, IncreaseByThree ) )
        , ( 0xDD96, ( \z80_main param -> SubIndexedIndirect z80_main.ix param, IncreaseByThree ) )
        , ( 0xDD9E, ( \z80_main param -> SbcIndexedIndirect z80_main.ix param, IncreaseByThree ) )
        , ( 0xDDA6, ( \z80_main param -> AndIndexedIndirect z80_main.ix param, IncreaseByThree ) )
        , ( 0xDDAE, ( \z80_main param -> XorIndexedIndirect z80_main.ix param, IncreaseByThree ) )
        , ( 0xDDB6, ( \z80_main param -> OrIndexedIndirect z80_main.ix param, IncreaseByThree ) )
        , ( 0xDDBE, ( \z80_main param -> CpIndexedIndirect z80_main.ix param, IncreaseByThree ) )
        ]


iyDouble : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, MediumPCIncrement )
iyDouble =
    Dict.fromList
        [ ( 0xFD26, ( ld_iy_h_n, IncreaseByThree ) )
        , ( 0xFD2E, ( ld_iy_l_n, IncreaseByThree ) )
        , ( 0xFD34, ( inc_indirect_iy, IncreaseByThree ) )
        , ( 0xFD35, ( dec_indirect_iy, IncreaseByThree ) )
        , ( 0xFD46, ( ld_b_indirect_iy, IncreaseByThree ) )
        , ( 0xFD4E, ( ld_c_indirect_iy, IncreaseByThree ) )
        , ( 0xFD56, ( ld_d_indirect_iy, IncreaseByThree ) )
        , ( 0xFD5E, ( ld_e_indirect_iy, IncreaseByThree ) )
        , ( 0xFD66, ( \z80_main param -> NewHIndexedIndirect z80_main.iy param, IncreaseByThree ) )
        , ( 0xFD6E, ( \z80_main param -> NewLIndexedIndirect z80_main.iy param, IncreaseByThree ) )
        , ( 0xFD86, ( add_a_indirect_iy, IncreaseByThree ) )
        , ( 0xFD8E, ( adc_a_indirect_iy, IncreaseByThree ) )
        , ( 0xFD96, ( \z80_main param -> SubIndexedIndirect z80_main.iy param, IncreaseByThree ) )
        , ( 0xFD9E, ( \z80_main param -> SbcIndexedIndirect z80_main.iy param, IncreaseByThree ) )
        , ( 0xFDA6, ( \z80_main param -> AndIndexedIndirect z80_main.iy param, IncreaseByThree ) )
        , ( 0xFDAE, ( \z80_main param -> XorIndexedIndirect z80_main.iy param, IncreaseByThree ) )
        , ( 0xFDB6, ( \z80_main param -> OrIndexedIndirect z80_main.iy param, IncreaseByThree ) )
        , ( 0xFDBE, ( \z80_main param -> CpIndexedIndirect z80_main.iy param, IncreaseByThree ) )
        ]


cbDouble : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, MediumPCIncrement )
cbDouble =
    Dict.fromList
        []


maybeRelativeJump : Dict Int (Int -> FlagRegisters -> JumpChange)
maybeRelativeJump =
    Dict.fromList
        [ ( 0x18, jr_n )
        , ( 0x20, jr_nz_d )
        , ( 0x28, jr_z_d )
        , ( 0x30, jr_nc_d )
        , ( 0x38, jr_c_d )
        , ( 0x3E, ld_a_n )
        , ( 0xC6, add_a_n )
        , ( 0xCE, adc_n )
        , ( 0xD6, sub_n )
        , ( 0xDE, sbc_a_n )
        , ( 0xE6, and_n )
        , ( 0xEE, xor_n )
        , ( 0xF6, or_n )
        , ( 0xFE, cp_n )
        ]


type Single8BitChange
    = NewBRegister Int
    | NewCRegister Int
    | NewDRegister Int
    | NewERegister Int


type DoubleWithRegisterChange
    = RelativeJumpWithTimeOffset Single8BitChange (Maybe Int) Int
    | DoubleRegChangeStoreIndirect Int Int InstructionDuration
    | NewHLRegisterValue Int
    | NewIXRegisterValue Int
    | NewIYRegisterValue Int
    | NewBIndexedIndirect Int Int
    | NewCIndexedIndirect Int Int
    | NewDIndexedIndirect Int Int
    | NewEIndexedIndirect Int Int
    | NewHIndexedIndirect Int Int
    | NewLIndexedIndirect Int Int
    | IndexedIndirectIncrement Int Int
    | IndexedIndirectDecrement Int Int
    | AdcIndexedIndirect Int Int
    | AddIndexedIndirect Int Int
    | SubIndexedIndirect Int Int
    | SbcIndexedIndirect Int Int
    | AndIndexedIndirect Int Int
    | XorIndexedIndirect Int Int
    | OrIndexedIndirect Int Int
    | CpIndexedIndirect Int Int


type JumpChange
    = ActualJump Int
    | NoJump
    | FlagJump FlagRegisters


parseDoubleWithRegs : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, MediumPCIncrement ) -> CpuTimeCTime -> Int -> Z80ROM -> Z80 -> Maybe Z80Transform
parseDoubleWithRegs operationDict instrTime instrCode rom48k z80 =
    case operationDict |> Dict.get instrCode of
        Just ( f, pcInc ) ->
            let
                ( param, new_pc ) =
                    case pcInc of
                        IncreaseByTwo ->
                            ( mem (Bitwise.and (z80.pc + 1) 0xFFFF) instrTime rom48k z80.env.ram, TwoByteInstruction )

                        IncreaseByThree ->
                            ( mem (Bitwise.and (z80.pc + 2) 0xFFFF) instrTime rom48k z80.env.ram, ThreeByteInstruction )

                -- duplicate of code in imm8 - add 3 to the cpu_time
                cpu_time =
                    param.time |> addCpuTimeTime 3
            in
            case f z80.main param.value of
                RelativeJumpWithTimeOffset single8BitChange maybeInt timeOffset ->
                    let
                        pc =
                            case maybeInt of
                                Just jump ->
                                    let
                                        pc_new =
                                            case pcInc of
                                                IncreaseByTwo ->
                                                    z80.pc + 2

                                                IncreaseByThree ->
                                                    z80.pc + 3
                                    in
                                    --Bitwise.and (new_pc + jump) 0xFFFF
                                    JumpInstruction (pc_new + jump)

                                Nothing ->
                                    --Bitwise.and new_pc 0xFFFF
                                    new_pc

                        simpleChange =
                            case single8BitChange of
                                NewBRegister int ->
                                    ChangeBRegister int

                                NewCRegister int ->
                                    ChangeCRegister int

                                NewDRegister int ->
                                    ChangeDRegister int

                                NewERegister int ->
                                    ChangeERegister int
                    in
                    Just { pcIncrement = pc, time = cpu_time |> addCpuTimeTime timeOffset, timeIncrement = FourTStates, operation = ChangeMain simpleChange }

                DoubleRegChangeStoreIndirect addr value cpuTimeIncrement ->
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = cpuTimeIncrement, operation = ChangeEnv (Store8BitMemoryValue addr value) }

                NewHLRegisterValue int ->
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMain (ChangeHLRegister int) }

                NewIXRegisterValue int ->
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMain (ChangeIXRegister int) }

                NewIYRegisterValue int ->
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMain (ChangeIYRegister int) }

                IndexedIndirectIncrement addr offset ->
                    let
                        ram_addr =
                            (addr + byte offset |> Bitwise.and 0xFFFF) - 0x4000
                    in
                    if ram_addr >= 0 then
                        Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeMemory (IncrementMemory ram_addr) }

                    else
                        Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeNothing }

                IndexedIndirectDecrement addr offset ->
                    let
                        ram_addr =
                            (addr + byte offset |> Bitwise.and 0xFFFF) - 0x4000
                    in
                    if ram_addr >= 0 then
                        Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeMemory (DecrementMemory ram_addr) }

                    else
                        Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeNothing }

                NewBIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeMain (ChangeBRegister value.value) }

                NewCIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeMain (ChangeCRegister value.value) }

                NewDIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeMain (ChangeDRegister value.value) }

                NewEIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeMain (ChangeERegister value.value) }

                NewHIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeMain (ChangeHRegister value.value) }

                NewLIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeMain (ChangeLRegister value.value) }

                AdcIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeFlagRegisters (z80.flags |> adc value.value) }

                AddIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeFlagRegisters (z80.flags |> z80_add value.value) }

                SubIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeFlagRegisters (z80.flags |> z80_sub value.value) }

                SbcIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeFlagRegisters (z80.flags |> sbc value.value) }

                AndIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeFlagRegisters (z80.flags |> z80_and value.value) }

                XorIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeFlagRegisters (z80.flags |> z80_xor value.value) }

                OrIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeFlagRegisters (z80.flags |> z80_or value.value) }

                CpIndexedIndirect addr offset ->
                    let
                        address =
                            addr + byte offset |> Bitwise.and 0xFFFF

                        value =
                            mem address cpu_time rom48k z80.env.ram
                    in
                    Just { pcIncrement = new_pc, time = cpu_time, timeIncrement = FifteenTStates, operation = ChangeFlagRegisters (z80.flags |> z80_cp value.value) }

        Nothing ->
            Nothing


applySimple8BitDelta : MediumPCIncrement -> CpuTimeCTime -> Single8BitChange -> Z80Transform
applySimple8BitDelta pcInc cpu_time z80changeData =
    let
        new_pc =
            case pcInc of
                IncreaseByTwo ->
                    TwoByteInstruction

                --Bitwise.and (z80.pc + 2) 0xFFFF
                IncreaseByThree ->
                    ThreeByteInstruction

        --Bitwise.and (z80.pc + 3) 0xFFFF
        --main =
        --    z80.main |> applySimple8BitChange z80changeData
    in
    --{ z80 | pc = new_pc, main = main }
    case z80changeData of
        NewBRegister int ->
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMain (ChangeBRegister int) }

        --{ z80_main | b = int }
        NewCRegister int ->
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMain (ChangeCRegister int) }

        NewDRegister int ->
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMain (ChangeDRegister int) }

        NewERegister int ->
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMain (ChangeERegister int) }


ld_b_n : Int -> Single8BitChange
ld_b_n param =
    -- case 0x06: B=imm8(); break;
    --{ z80 | env = new_b.env, pc = new_b.pc }|> set_b new_b.value
    NewBRegister param


ld_c_n : Int -> Single8BitChange
ld_c_n param =
    -- case 0x0E: C=imm8(); break;
    --{ z80 | env = new_c.env, pc = new_c.pc, main = { z80_main | c = new_c.value } }
    NewCRegister param


ld_d_n : Int -> Single8BitChange
ld_d_n param =
    -- case 0x16: D=imm8(); break;
    NewDRegister param


ld_e_n : Int -> Single8BitChange
ld_e_n param =
    -- case 0x1E: E=imm8(); break;
    NewERegister param


ld_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_h_n z80_main param =
    -- case 0x26: HL=HL&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF) |> NewHLRegisterValue


ld_ix_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_ix_h_n z80_main param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.ix 0xFF) |> NewIXRegisterValue


ld_iy_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_iy_h_n z80_main param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.iy 0xFF) |> NewIYRegisterValue


ld_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_l_n z80_main param =
    -- case 0x2E: HL=HL&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.hl 0xFF00) |> NewHLRegisterValue


ld_ix_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_ix_l_n z80_main param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.ix 0xFF00) |> NewIXRegisterValue


ld_iy_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_iy_l_n z80_main param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.iy 0xFF00) |> NewIYRegisterValue


djnz : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
djnz z80_main param =
    --case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
    --if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
    --PC=(char)v;} break;
    let
        d =
            byte param

        b =
            Bitwise.and (z80_main.b - 1) 0xFF

        ( time, jump ) =
            if b /= 0 then
                ( 9, Just d )

            else
                ( 4, Nothing )
    in
    RelativeJumpWithTimeOffset (NewBRegister b) jump time


ld_indirect_hl_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_indirect_hl_n z80_main param =
    -- case 0x36: env.mem(HL,imm8()); time+=3; break;
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    DoubleRegChangeStoreIndirect z80_main.hl param SevenTStates


inc_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
inc_indirect_ix z80_main param =
    -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement z80_main.ix param


ld_b_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_b_indirect_ix z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    NewBIndexedIndirect z80_main.ix param


ld_c_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_c_indirect_ix z80_main param =
    --case 0x4E: C=env.mem(getd(xy)); time+=3; break;
    NewCIndexedIndirect z80_main.ix param


ld_d_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_d_indirect_ix z80_main param =
    --case 0x56: D=env.mem(getd(xy)); time+=3; break;
    NewDIndexedIndirect z80_main.ix param


ld_e_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_e_indirect_ix z80_main param =
    --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
    NewEIndexedIndirect z80_main.ix param


ld_b_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_b_indirect_iy z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    NewBIndexedIndirect z80_main.iy param


ld_c_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_c_indirect_iy z80_main param =
    --case 0x4E: C=env.mem(getd(xy)); time+=3; break;
    NewCIndexedIndirect z80_main.iy param


ld_d_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_d_indirect_iy z80_main param =
    --case 0x56: D=env.mem(getd(xy)); time+=3; break;
    NewDIndexedIndirect z80_main.iy param


ld_e_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_e_indirect_iy z80_main param =
    --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
    NewEIndexedIndirect z80_main.iy param


adc_a_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
adc_a_indirect_iy z80_main param =
    -- case 0x8E: adc(env.mem(HL)); time+=3; break;
    AdcIndexedIndirect z80_main.iy param


add_a_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
add_a_indirect_ix z80_main param =
    -- case 0x8E: adc(env.mem(HL)); time+=3; break;
    AddIndexedIndirect z80_main.ix param


add_a_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
add_a_indirect_iy z80_main param =
    -- case 0x8E: adc(env.mem(HL)); time+=3; break;
    AddIndexedIndirect z80_main.iy param


inc_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
inc_indirect_iy z80_main param =
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement z80_main.iy param


dec_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
dec_indirect_ix z80_main param =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement z80_main.ix param


dec_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
dec_indirect_iy z80_main param =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement z80_main.iy param


jr_n : Int -> FlagRegisters -> JumpChange
jr_n param _ =
    -- case 0x18: MP=PC=(char)(PC+1+(byte)env.mem(PC)); time+=8; break;
    -- This is just an inlined jr() call
    --z80 |> set_pc dest |> add_cpu_time 8
    ActualJump (byte param)


jr_nz_d : Int -> FlagRegisters -> JumpChange
jr_nz_d param z80_flags =
    -- case 0x20: if(Fr!=0) jr(); else imm8(); break;
    if z80_flags.fr /= 0 then
        ActualJump (byte param)

    else
        NoJump


jr_z_d : Int -> FlagRegisters -> JumpChange
jr_z_d param z80_flags =
    -- case 0x28: if(Fr==0) jr(); else imm8(); break;
    if z80_flags.fr == 0 then
        ActualJump (byte param)

    else
        NoJump


jr_nc_d : Int -> FlagRegisters -> JumpChange
jr_nc_d param z80_flags =
    -- case 0x30: if((Ff&0x100)==0) jr(); else imm8(); break;
    if Bitwise.and z80_flags.ff 0x0100 == 0 then
        ActualJump (byte param)

    else
        NoJump


jr_c_d : Int -> FlagRegisters -> JumpChange
jr_c_d param z80_flags =
    -- case 0x38: if((Ff&0x100)!=0) jr(); else imm8(); break;
    if Bitwise.and z80_flags.ff 0x0100 /= 0 then
        ActualJump (byte param)

    else
        NoJump


add_a_n : Int -> FlagRegisters -> JumpChange
add_a_n param z80_flags =
    -- case 0xC6: add(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        --env_1 = z80.env
        --z80_1 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
        flags =
            z80_flags |> z80_add param
    in
    --{ z80_1 | flags = flags }
    FlagJump flags


adc_n : Int -> FlagRegisters -> JumpChange
adc_n param z80_flags =
    -- case 0xCE: adc(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        flags =
            z80_flags |> adc param

        --env_1 = z80.env
    in
    --{z80 | pc = v.pc, env = { env_1 | time = v.time }, flags = flags }
    --FlagsWithPcAndTime flags v.pc v.time
    FlagJump flags


sub_n : Int -> FlagRegisters -> JumpChange
sub_n param z80_flags =
    -- case 0xD6: sub(imm8()); break;
    let
        --v = imm8 z80.pc z80.env.time rom48k z80.env.ram
        flags =
            z80_flags |> z80_sub param

        --env_1 = z80.env
    in
    --{ z80 | flags = flags, env = { env_1 | time = v.time }, pc = v.pc }
    --FlagsWithPcAndTime flags v.pc v.time
    FlagJump flags


sbc_a_n : Int -> FlagRegisters -> JumpChange
sbc_a_n param z80_flags =
    -- case 0xDE: sbc(imm8()); break;
    z80_flags |> sbc param |> FlagJump


and_n : Int -> FlagRegisters -> JumpChange
and_n param z80_flags =
    -- case 0xE6: and(imm8()); break;
    let
        --a =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        --
        --env_1 =
        --    z80.env
        --
        --z80_1 =
        --    { z80 | env = { env_1 | time = a.time }, pc = a.pc }
        flags =
            z80_flags |> z80_and param
    in
    --{ z80_1 | flags = flags }
    --FlagsWithPcAndTime flags a.pc a.time
    FlagJump flags


xor_n : Int -> FlagRegisters -> JumpChange
xor_n param z80_flags =
    -- case 0xEE: xor(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        --env_1 = z80.env
        --z80_1 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
        flags =
            z80_flags |> z80_xor param
    in
    --{ z80_1 | flags = flags }
    --FlagsWithPcAndTime flags v.pc v.time
    FlagJump flags


or_n : Int -> FlagRegisters -> JumpChange
or_n param z80_flags =
    -- case 0xF6: or(imm8()); break;
    let
        --a = imm8 z80.pc z80.env.time rom48k z80.env.ram
        --env_1 = z80.env
        --z80_1 = { z80 | env = { env_1 | time = a.time }, pc = a.pc }
        flags =
            z80_flags |> z80_or param
    in
    --{ z80_1 | flags = flags }
    --FlagsWithPcAndTime flags a.pc a.time
    FlagJump flags


cp_n : Int -> FlagRegisters -> JumpChange
cp_n param z80_flags =
    -- case 0xFE: cp(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        flags =
            z80_flags |> z80_cp param

        --env_1 =
        --    z80.env
    in
    --{ z80 | flags = flags, env = { env_1 | time = v.time }, pc = v.pc }
    FlagJump flags


ld_a_n : Int -> FlagRegisters -> JumpChange
ld_a_n param z80_flags =
    -- case 0x3E: A=imm8(); break;
    FlagJump { z80_flags | a = param }
