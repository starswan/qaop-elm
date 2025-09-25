module Z80OpCode exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, addDuration)
import Dict
import PCIncrement exposing (PCIncrement(..), TriplePCIncrement(..))
import SimpleFlagOps exposing (singleByteFlags)
import SimpleSingleByte exposing (singleByteMainRegs)
import SingleEnvWithMain exposing (applySingleEnvMainChange, singleEnvMainRegs)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters)
import SingleWith8BitParameter exposing (maybeRelativeJump)
import TripleByte exposing (tripleByteWith16BitParam)
import TripleWithFlags exposing (triple16WithFlags)
import Z80Core exposing (Z80Core)
import Z80Env exposing (m1, mem, mem16)
import Z80Execute exposing (DeltaWithChanges(..), applyFlagDelta, applyJumpChangeDelta, applyPureDelta, applyRegisterDelta, applyTripleChangeDelta, applyTripleFlagChange)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


type Z80OpCode
    = TimeAndValue CpuTimeAndValue
    | CoreFunction (PCIncrement -> Z80Core -> Z80Core) PCIncrement


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
            CoreFunction (\pcInc core -> core |> applyRegisterDelta pcInc duration (mainRegFunc core.main) rom48k) IncrementByOne

        Nothing ->
            case singleByteFlags |> Dict.get ct.value of
                Just ( flagFunc, duration ) ->
                    CoreFunction (\pcInc core -> core |> applyFlagDelta pcInc duration (flagFunc core.flags) rom48k) IncrementByOne

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
                            CoreFunction (\pcInc core -> core |> applyTripleChangeDelta rom48k pcInc doubleParam.time (f doubleParam.value16)) PCIncrementByThree

                        Nothing ->
                            case maybeRelativeJump |> Dict.get ct.value of
                                Just ( f, duration ) ->
                                    let
                                        newTime =
                                            ct.time |> addDuration duration

                                        param =
                                            z80_core.env |> mem (Bitwise.and (z80_core.pc + 1) 0xFFFF) newTime rom48k
                                    in
                                    CoreFunction (\_ core -> core |> applyJumpChangeDelta param.time (f param.value z80_core.flags)) IncrementByTwo

                                Nothing ->
                                    case singleEnvMainRegs |> Dict.get ct.value of
                                        Just ( f, duration ) ->
                                            CoreFunction (\inc core -> core |> applySingleEnvMainChange inc duration (f z80_core.main rom48k ct.time z80_core.env) rom48k) IncrementByOne

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
                                                    CoreFunction (\_ core -> core |> applyTripleFlagChange doubleParam.time (f doubleParam.value16 z80_core.flags)) PCIncrementByThree

                                                Nothing ->
                                                    case singleByteMainAndFlagRegisters |> Dict.get ct.value of
                                                        Just ( f, duration ) ->
                                                            CoreFunction (\pcInc core -> core |> applyPureDelta pcInc (ct.time |> addDuration duration) (f z80_core.main z80_core.flags)) IncrementByOne

                                                        Nothing ->
                                                            TimeAndValue ct
