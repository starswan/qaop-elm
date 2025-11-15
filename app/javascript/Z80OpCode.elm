module Z80OpCode exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, InstructionDuration, addDuration, reset_cpu_time)
import Dict
import DoubleWithRegisters exposing (applyDoubleWithRegistersDelta, doubleWithRegisters, doubleWithRegistersIY)
import GroupED exposing (singleByteMainAndFlagsED, singleByteMainRegsED)
import Maybe.Extra exposing (oneOf)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import SimpleFlagOps exposing (singleByteFlags)
import SimpleSingleByte exposing (singleByteMainRegs, singleByteMainRegsFD)
import SingleEnvWithMain exposing (applySingleEnvMainChange, singleEnvMainRegs)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters)
import SingleNoParams exposing (applyNoParamsDelta, applyRstDelta, singleNoParamCalls, singleWithNoParam)
import SingleWith8BitParameter exposing (maybeRelativeJump, singleWith8BitParam)
import TripleByte exposing (tripleByteWith16BitParam)
import TripleWithFlags exposing (triple16bitJumps)
import Z80Core exposing (Z80Core)
import Z80Env exposing (Z80Env)
import Z80Execute exposing (applyEdRegisterDelta, applyFlagDelta, applyJumpChangeDelta, applyPureDelta, applyRegisterDelta, applySimple8BitDelta, applyTripleChangeDelta, applyTripleFlagChange)
import Z80Mem exposing (m1, mem, mem16)
import Z80Rom exposing (CompiledZ80ROM, CpuInstruction, Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


fetchInstruction : CompiledZ80ROM -> Int -> Z80Core -> CpuInstruction
fetchInstruction rom48k r_register z80_core =
    let
        pc_value =
            --case romRoutineNames |> Dict.get z80.pc of
            --    Just name ->
            --        debugLog "fetch PC " name z80.pc
            --
            --    Nothing ->
            --if
            --    ([ 0x11E2, 0x11E3, 0x11E5, 0x11E6, 0x11E7, 0x11E9, 0x11EA, 0x11EC, 0x11ED, 0x11DC, 0x11DE, 0x11DF, 0x11E0 ] |> List.member z80_core.pc)
            --        || ([ 0x0E54, 0x0E59, 0x0E4D, 0x0E5E, 0x0E57, 0x0E5C, 0x0E62, 0x0E5B ] |> List.member z80_core.pc)
            --then
            --    z80_core.pc
            --
            --else
            --    debugLog "m1" (subName z80_core.pc) z80_core.pc
            z80_core.pc
    in
    z80_core.env |> m1 pc_value (Bitwise.or z80_core.interrupts.ir (Bitwise.and r_register 0x7F)) rom48k z80_core.clockTime


lengthAndDuration : Int -> Z80ROM -> Z80Env -> Maybe ( PCIncrement, InstructionDuration, InstructionDuration -> Z80ROM -> Z80Core -> Z80Core )
lengthAndDuration pc rom48k z80env =
    let
        clockTime =
            reset_cpu_time

        opcode =
            z80env
                |> mem pc clockTime rom48k
                |> .value
    in
    if opcode == 0xFD then
        let
            param =
                z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) clockTime rom48k
        in
        param.value
            |> oneOf
                [ \fdinstruction ->
                    singleByteMainRegsFD
                        |> Dict.get fdinstruction
                        |> Maybe.map
                            (\( mainRegFunc, duration ) ->
                                ( IncrementByTwo, duration, \dur z80rom z80core -> z80core |> applyRegisterDelta IncrementByTwo dur (mainRegFunc z80core.main) z80rom )
                            )
                , \fdinstruction ->
                    doubleWithRegistersIY
                        |> Dict.get fdinstruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    doubleParam =
                                        z80env |> mem (Bitwise.and (pc + 2) 0xFFFF) clockTime rom48k
                                in
                                ( IncrementByThree
                                , duration
                                , \dur z80rom z80core ->
                                    z80core
                                        |> applyDoubleWithRegistersDelta IncreaseByThree (z80core.clockTime |> addDuration dur) (f z80core.main doubleParam.value) z80rom
                                )
                            )
                ]

    else
        opcode
            |> oneOf
                [ \instruction ->
                    singleWithNoParam
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( noParamChange, duration ) ->
                                ( IncrementByOne, duration, \dur z80rom z80core -> z80core |> applyNoParamsDelta (z80core.clockTime |> addDuration dur) noParamChange z80rom )
                            )
                , \instruction ->
                    singleNoParamCalls
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( noParamChange, duration ) ->
                                ( IncrementByOne, duration, \dur z80rom z80core -> z80core |> applyRstDelta (z80core.clockTime |> addDuration dur) noParamChange z80rom )
                            )
                , \instruction ->
                    tripleByteWith16BitParam
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    doubleParam =
                                        z80env |> mem16 (Bitwise.and (pc + 1) 0xFFFF) rom48k clockTime
                                in
                                ( IncrementByThree
                                , duration
                                , \dur z80rom z80core ->
                                    z80core |> applyTripleChangeDelta z80rom TripleIncrementByThree (z80core.clockTime |> addDuration dur) (f doubleParam.value16)
                                )
                            )
                , \instruction ->
                    singleByteMainAndFlagRegisters
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                ( IncrementByOne, duration, \dur z80rom z80core -> z80core |> applyPureDelta IncrementByOne (z80core.clockTime |> addDuration dur) (f z80core.main z80core.flags) )
                            )
                , \instruction ->
                    singleByteMainRegs
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( mainRegFunc, duration ) ->
                                ( IncrementByOne
                                , duration
                                , \dur z80rom z80core ->
                                    z80core |> applyRegisterDelta IncrementByOne dur (mainRegFunc z80core.main) z80rom
                                )
                            )
                , \instruction ->
                    singleWith8BitParam
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    param =
                                        z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) clockTime rom48k
                                in
                                ( IncrementByTwo, duration, \dur z80rom z80core -> z80core |> applySimple8BitDelta IncreaseByTwo (z80core.clockTime |> addDuration dur) (f param.value) z80rom )
                            )
                , \instruction ->
                    singleByteFlags
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( flagFunc, duration ) ->
                                ( IncrementByOne, duration, \dur z80rom z80core -> z80core |> applyFlagDelta IncrementByOne dur (flagFunc z80core.flags) z80rom )
                            )

                -- These not supported by tests (yet)
                , \instruction ->
                    doubleWithRegisters
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    param =
                                        z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) clockTime rom48k
                                in
                                ( IncrementByTwo
                                , duration
                                , \dur z80rom z80core ->
                                    z80core
                                        |> applyDoubleWithRegistersDelta IncreaseByTwo (z80core.clockTime |> addDuration dur) (f z80core.main param.value) z80rom
                                )
                            )
                , \instruction ->
                    triple16bitJumps
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    doubleParam =
                                        z80env |> mem16 (Bitwise.and (pc + 1) 0xFFFF) rom48k clockTime
                                in
                                ( IncrementByThree, duration, \dur z80rom z80core -> z80core |> applyTripleFlagChange (z80core.clockTime |> addDuration dur) (f doubleParam.value16) )
                            )
                , \instruction ->
                    maybeRelativeJump
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    param =
                                        z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) clockTime rom48k
                                in
                                ( IncrementByTwo, duration, \dur z80rom z80core -> z80core |> applyJumpChangeDelta (z80core.clockTime |> addDuration dur) (f param.value pc) )
                            )
                , \instruction ->
                    singleEnvMainRegs
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                ( IncrementByOne
                                , duration
                                , \dur z80rom z80core ->
                                    z80core |> applySingleEnvMainChange IncrementByOne dur (f z80core.main z80rom (z80core.clockTime |> addDuration dur) z80core.env) z80rom
                                )
                            )
                , \instruction ->
                    singleByteMainRegsED
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                ( IncrementByTwo
                                , duration
                                , \dur z80rom z80core ->
                                    z80core |> applyEdRegisterDelta IncrementByTwo dur (f z80core.main) z80rom
                                )
                            )
                , \instruction ->
                    singleByteMainAndFlagsED
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, pcInc, duration ) ->
                                ( pcInc
                                , duration
                                , \dur z80rom z80core ->
                                    z80core |> applyPureDelta pcInc (z80core.clockTime |> addDuration dur) (f z80core.main z80core.flags)
                                )
                            )
                ]
