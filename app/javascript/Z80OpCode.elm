module Z80OpCode exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, addDuration)
import Dict
import PCIncrement exposing (PCIncrement(..), TriplePCIncrement(..))
import SimpleFlagOps exposing (singleByteFlags)
import SimpleSingleByte exposing (singleByteMainRegs)
import TripleByte exposing (tripleByteWith16BitParam)
import Z80Core exposing (Z80Core)
import Z80Env exposing (m1, mem16)
import Z80Execute exposing (DeltaWithChanges(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


type Z80OpCode
    = TimeAndValue CpuTimeAndValue
    | CoreToDeltaChanges (Z80Core -> DeltaWithChanges)


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
            --DirectExecute (applyRegisterDelta IncrementByOne duration (mainRegFunc core.main) rom48k)
            CoreToDeltaChanges (\core -> RegisterChangeDelta IncrementByOne duration (mainRegFunc core.main))

        Nothing ->
            case singleByteFlags |> Dict.get ct.value of
                Just ( flagFunc, duration ) ->
                    CoreToDeltaChanges (\core -> FlagDelta IncrementByOne duration (flagFunc core.flags))

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
                            CoreToDeltaChanges (\core -> Triple16ParamDelta doubleParam.time IncrementByThree (f doubleParam.value16))

                        Nothing ->
                            TimeAndValue ct
