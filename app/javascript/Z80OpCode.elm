module Z80OpCode exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, InstructionDuration, addDuration)
import Dict
import DoubleWithRegisters exposing (applyDoubleWithRegistersDelta, doubleWithRegisters)
import GroupCB exposing (singleByteMainAndFlagRegistersCB, singleByteMainRegsCB, singleEnvMainRegsCB)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import SimpleFlagOps exposing (singleByteFlags, singleByteFlagsCB)
import SimpleSingleByte exposing (singleByteMainRegs)
import SingleByteWithEnv exposing (applyEnvChangeDelta, singleByteZ80Env)
import SingleEnvWithMain exposing (applySingleEnvMainChange, singleEnvMainRegs)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters)
import SingleNoParams exposing (applyNoParamsDelta, singleWithNoParam)
import SingleWith8BitParameter exposing (maybeRelativeJump, singleWith8BitParam)
import TripleByte exposing (tripleByteWith16BitParam)
import TripleWithFlags exposing (triple16bitJumps)
import TripleWithMain exposing (applyTripleMainChange, tripleMainRegs)
import Z80Core exposing (Z80Core)
import Z80Env exposing (m1, mem, mem16)
import Z80Execute exposing (DeltaWithChanges(..), applyFlagDelta, applyJumpChangeDelta, applyPureDelta, applyRegisterDelta, applySimple8BitDelta, applyTripleChangeDelta, applyTripleFlagChange)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


type Z80OpCode
    = TimeAndValue CpuTimeAndValue
    | CoreFunction (Z80Core -> Z80Core) PCIncrement InstructionDuration


fetchInstruction : Z80ROM -> Int -> Z80Core -> Z80OpCode
fetchInstruction rom48k r_register z80_core =
    let
        pc_value =
            --case romRoutineNames |> Dict.get z80.pc of
            --    Just name ->
            --        debugLog "fetch PC " name z80.pc
            --
            --    Nothing ->
            z80_core.pc

        ct =
            z80_core.env |> m1 pc_value (Bitwise.or z80_core.interrupts.ir (Bitwise.and r_register 0x7F)) rom48k z80_core.clockTime
    in
    case singleByteMainRegs |> Dict.get ct.value of
        Just ( mainRegFunc, duration ) ->
            CoreFunction
                (\core ->
                    core |> applyRegisterDelta IncrementByOne duration (mainRegFunc core.main) rom48k
                )
                IncrementByOne
                duration

        Nothing ->
            case singleByteFlags |> Dict.get ct.value of
                Just ( flagFunc, duration ) ->
                    CoreFunction
                        (\core ->
                            core |> applyFlagDelta IncrementByOne duration (flagFunc core.flags) rom48k
                        )
                        IncrementByOne
                        duration

                Nothing ->
                    case tripleByteWith16BitParam |> Dict.get ct.value of
                        Just ( f, duration ) ->
                            let
                                env =
                                    z80_core.env

                                newTime =
                                    ct.time |> addDuration duration

                                doubleParam =
                                    env |> mem16 (Bitwise.and (z80_core.pc + 1) 0xFFFF) rom48k newTime
                            in
                            CoreFunction
                                (\core ->
                                    core |> applyTripleChangeDelta rom48k TripleIncrementByThree doubleParam.time (f doubleParam.value16)
                                )
                                IncrementByThree
                                duration

                        Nothing ->
                            case maybeRelativeJump |> Dict.get ct.value of
                                Just ( f, duration ) ->
                                    let
                                        newTime =
                                            ct.time |> addDuration duration

                                        param =
                                            z80_core.env |> mem (Bitwise.and (z80_core.pc + 1) 0xFFFF) newTime rom48k
                                    in
                                    CoreFunction
                                        (\core ->
                                            core |> applyJumpChangeDelta param.time (f param.value z80_core.pc)
                                        )
                                        IncrementByTwo
                                        duration

                                Nothing ->
                                    case singleEnvMainRegs |> Dict.get ct.value of
                                        Just ( f, duration ) ->
                                            CoreFunction
                                                (\core ->
                                                    core |> applySingleEnvMainChange IncrementByOne duration (f z80_core.main rom48k ct.time z80_core.env) rom48k
                                                )
                                                IncrementByOne
                                                duration

                                        Nothing ->
                                            case triple16WithFlags |> Dict.get ct.value of
                                                Just ( f, duration ) ->
                                                    let
                                                        env =
                                                            z80_core.env

                                                        env_1 =
                                                            ct.time |> addDuration duration

                                                        doubleParam =
                                                            env |> mem16 (Bitwise.and (z80_core.pc + 1) 0xFFFF) rom48k env_1
                                                    in
                                                    CoreFunction
                                                        (\core ->
                                                            core |> applyTripleFlagChange doubleParam.time (f doubleParam.value16)
                                                        )
                                                        IncrementByThree
                                                        duration

                                                Nothing ->
                                                    case singleByteMainAndFlagRegisters |> Dict.get ct.value of
                                                        Just ( f, duration ) ->
                                                            CoreFunction
                                                                (\core ->
                                                                    core |> applyPureDelta IncrementByOne (ct.time |> addDuration duration) (f z80_core.main z80_core.flags)
                                                                )
                                                                IncrementByOne
                                                                duration

                                                        Nothing ->
                                                            case doubleWithRegisters |> Dict.get ct.value of
                                                                Just ( f, duration ) ->
                                                                    let
                                                                        time =
                                                                            ct.time |> addDuration duration

                                                                        param =
                                                                            z80_core.env |> mem (Bitwise.and (z80_core.pc + 1) 0xFFFF) time rom48k
                                                                    in
                                                                    CoreFunction
                                                                        (\core ->
                                                                            core |> applyDoubleWithRegistersDelta IncreaseByTwo param.time (f z80_core.main param.value) rom48k
                                                                        )
                                                                        IncrementByTwo
                                                                        duration

                                                                Nothing ->
                                                                    case tripleMainRegs |> Dict.get ct.value of
                                                                        Just ( f, duration ) ->
                                                                            let
                                                                                time =
                                                                                    ct.time |> addDuration duration

                                                                                env =
                                                                                    z80_core.env

                                                                                doubleParam =
                                                                                    env |> mem16 (Bitwise.and (z80_core.pc + 1) 0xFFFF) rom48k time
                                                                            in
                                                                            CoreFunction
                                                                                (\core ->
                                                                                    core |> applyTripleMainChange doubleParam.time TripleIncrementByThree (f doubleParam.value16 z80_core.main)
                                                                                )
                                                                                IncrementByThree
                                                                                duration

                                                                        Nothing ->
                                                                            case singleByteZ80Env |> Dict.get ct.value of
                                                                                Just ( f, duration ) ->
                                                                                    CoreFunction
                                                                                        (\core ->
                                                                                            core |> applyEnvChangeDelta (ct.time |> addDuration duration) (f z80_core.env)
                                                                                        )
                                                                                        IncrementByOne
                                                                                        duration

                                                                                Nothing ->
                                                                                    case singleWithNoParam |> Dict.get ct.value of
                                                                                        Just ( f, duration ) ->
                                                                                            CoreFunction (\core -> core |> applyNoParamsDelta (ct.time |> addDuration duration) f rom48k) IncrementByOne duration

                                                                                        Nothing ->
                                                                                            case singleWith8BitParam |> Dict.get ct.value of
                                                                                                Just ( f, duration ) ->
                                                                                                    let
                                                                                                        newTime =
                                                                                                            ct.time |> addDuration duration

                                                                                                        param =
                                                                                                            z80_core.env |> mem (Bitwise.and (z80_core.pc + 1) 0xFFFF) newTime rom48k
                                                                                                    in
                                                                                                    CoreFunction (\core -> core |> applySimple8BitDelta IncreaseByTwo param.time (f param.value) rom48k) IncrementByTwo duration

                                                                                                Nothing ->
                                                                                                    case ct.value of
                                                                                                        0xCB ->
                                                                                                            let
                                                                                                                param =
                                                                                                                    z80_core.env |> mem (Bitwise.and (z80_core.pc + 1) 0xFFFF) ct.time rom48k
                                                                                                            in
                                                                                                            --Special (BitManipCB param)
                                                                                                            z80_core |> fetchBitManipCB rom48k param

                                                                                                        _ ->
                                                                                                            TimeAndValue ct


fetchBitManipCB : Z80ROM -> CpuTimeAndValue -> Z80Core -> Z80OpCode
fetchBitManipCB rom48k param z80_core =
    let
        instrTime =
            param.time
    in
    case singleByteMainRegsCB |> Dict.get param.value of
        Just ( mainRegFunc, duration ) ->
            --RegisterChangeDelta IncrementByTwo duration (mainRegFunc z80_core.main)
            CoreFunction (\core -> core |> applyRegisterDelta IncrementByTwo duration (mainRegFunc z80_core.main) rom48k) IncrementByTwo duration

        Nothing ->
            case singleByteFlagsCB |> Dict.get param.value of
                Just ( flagFunc, duration ) ->
                    --FlagDelta IncrementByTwo duration (flagFunc z80_core.flags)
                    CoreFunction
                        (\core ->
                            core |> applyFlagDelta IncrementByTwo duration (flagFunc core.flags) rom48k
                        )
                        IncrementByTwo
                        duration

                Nothing ->
                    case singleEnvMainRegsCB |> Dict.get param.value of
                        Just ( f, duration ) ->
                            --MainWithEnvDelta IncrementByTwo duration (f z80_core.main rom48k z80_core.env)
                            CoreFunction
                                (\core ->
                                    core |> applySingleEnvMainChange IncrementByTwo duration (f z80_core.main rom48k z80_core.env) rom48k
                                )
                                IncrementByTwo
                                duration

                        Nothing ->
                            case singleByteMainAndFlagRegistersCB |> Dict.get param.value of
                                Just ( f, duration ) ->
                                    --PureDelta IncrementByTwo (instrTime |> addDuration duration) (f z80_core.main z80_core.flags)
                                    CoreFunction
                                        (\core ->
                                            core |> applyPureDelta IncrementByTwo (instrTime |> addDuration duration) (f z80_core.main z80_core.flags)
                                        )
                                        IncrementByOne
                                        duration

                                Nothing ->
                                    TimeAndValue param
