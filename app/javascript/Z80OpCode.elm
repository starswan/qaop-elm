module Z80OpCode exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, InstructionDuration, addDuration, reset_cpu_time)
import Dict
import DoubleWithRegisters exposing (applyDoubleWithRegistersDelta, doubleWithRegisters, doubleWithRegistersIY)
import Maybe.Extra exposing (oneOf)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import SimpleFlagOps exposing (singleByteFlags)
import SimpleSingleByte exposing (singleByteMainRegs, singleByteMainRegsFD)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters)
import SingleNoParams exposing (applyNoParamsDelta, applyRstDelta, singleNoParamCalls, singleWithNoParam)
import SingleWith8BitParameter exposing (maybeRelativeJump, singleWith8BitParam)
import TripleByte exposing (tripleByteWith16BitParam)
import TripleWithFlags exposing (triple16bitJumps)
import Z80Core exposing (Z80Core)
import Z80Env exposing (Z80Env)
import Z80Execute exposing (applyFlagDelta, applyJumpChangeDelta, applyPureDelta, applyRegisterDelta, applySimple8BitDelta, applyTripleChangeDelta, applyTripleFlagChange)
import Z80Mem exposing (m1, mem, mem16)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


fetchInstruction : Z80ROM -> Int -> Z80Core -> CpuTimeAndValue
fetchInstruction rom48k r_register z80_core =
    let
        pc_value =
            --case romRoutineNames |> Dict.get z80.pc of
            --    Just name ->
            --        debugLog "fetch PC " name z80.pc
            --
            --    Nothing ->
            z80_core.pc
    in
    z80_core.env |> m1 pc_value (Bitwise.or z80_core.interrupts.ir (Bitwise.and r_register 0x7F)) rom48k z80_core.clockTime


lengthAndDuration : Int -> Z80ROM -> Z80Env -> Maybe ( PCIncrement, InstructionDuration, InstructionDuration -> Z80Core -> Z80Core )
lengthAndDuration pc rom48k z80env =
    let
        opcode =
            z80env
                |> m1 pc 0 rom48k reset_cpu_time
                |> .value
    in
    if opcode == 0xFD then
        let
            param =
                z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) reset_cpu_time rom48k
        in
        param.value
            |> oneOf
                [ \fdinstruction ->
                    singleByteMainRegsFD
                        |> Dict.get fdinstruction
                        |> Maybe.map
                            (\( mainRegFunc, duration ) ->
                                ( IncrementByTwo, duration, \dur z80core -> z80core |> applyRegisterDelta IncrementByTwo dur (mainRegFunc z80core.main) rom48k )
                            )
                , \fdinstruction ->
                    doubleWithRegistersIY
                        |> Dict.get fdinstruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    doubleParam =
                                        z80env |> mem (Bitwise.and (pc + 2) 0xFFFF) reset_cpu_time rom48k
                                in
                                ( IncrementByThree
                                , duration
                                , \dur z80core ->
                                    z80core
                                        |> applyDoubleWithRegistersDelta IncreaseByThree (z80core.clockTime |> addDuration dur) (f z80core.main doubleParam.value) rom48k
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
                                ( IncrementByOne, duration, \dur z80core -> z80core |> applyNoParamsDelta (z80core.clockTime |> addDuration dur) noParamChange rom48k )
                            )
                , \instruction ->
                    singleNoParamCalls
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( noParamChange, duration ) ->
                                ( IncrementByOne, duration, \dur z80core -> z80core |> applyRstDelta (z80core.clockTime |> addDuration dur) noParamChange rom48k )
                            )
                , \instruction ->
                    tripleByteWith16BitParam
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    doubleParam =
                                        z80env |> mem16 (Bitwise.and (pc + 1) 0xFFFF) rom48k reset_cpu_time
                                in
                                ( IncrementByThree
                                , duration
                                , \dur z80core ->
                                    z80core |> applyTripleChangeDelta rom48k TripleIncrementByThree (z80core.clockTime |> addDuration dur) (f doubleParam.value16)
                                )
                            )
                , \instruction ->
                    singleByteMainAndFlagRegisters
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                ( IncrementByOne, duration, \dur z80core -> z80core |> applyPureDelta IncrementByOne (z80core.clockTime |> addDuration dur) (f z80core.main z80core.flags) )
                            )
                , \instruction ->
                    singleByteMainRegs
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( mainRegFunc, duration ) ->
                                ( IncrementByOne
                                , duration
                                , \dur z80core ->
                                    z80core |> applyRegisterDelta IncrementByOne dur (mainRegFunc z80core.main) rom48k
                                )
                            )
                , \instruction ->
                    singleWith8BitParam
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    param =
                                        z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) reset_cpu_time rom48k
                                in
                                ( IncrementByTwo, duration, \dur z80core -> z80core |> applySimple8BitDelta IncreaseByTwo (z80core.clockTime |> addDuration dur) (f param.value) rom48k )
                            )
                , \instruction ->
                    singleByteFlags
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( flagFunc, duration ) ->
                                ( IncrementByOne, duration, \dur z80core -> z80core |> applyFlagDelta IncrementByOne dur (flagFunc z80core.flags) rom48k )
                            )

                -- These not supported by tests (yet)
                , \instruction ->
                    doubleWithRegisters
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    param =
                                        z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) reset_cpu_time rom48k
                                in
                                ( IncrementByTwo
                                , duration
                                , \dur z80core ->
                                    z80core
                                        |> applyDoubleWithRegistersDelta IncreaseByTwo (z80core.clockTime |> addDuration dur) (f z80core.main param.value) rom48k
                                )
                            )
                , \instruction ->
                    triple16bitJumps
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    doubleParam =
                                        z80env |> mem16 (Bitwise.and (pc + 1) 0xFFFF) rom48k reset_cpu_time
                                in
                                ( IncrementByThree, duration, \dur z80core -> z80core |> applyTripleFlagChange (z80core.clockTime |> addDuration dur) (f doubleParam.value16) )
                            )
                , \instruction ->
                    maybeRelativeJump
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    param =
                                        z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) reset_cpu_time rom48k
                                in
                                ( IncrementByTwo, duration, \dur z80core -> z80core |> applyJumpChangeDelta (z80core.clockTime |> addDuration dur) (f param.value pc) )
                            )
                ]
