module Z80OpCode exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, CpuTimeCTime, InstructionDuration, reset_cpu_time)
import Dict
import DoubleWithRegisters exposing (applyDoubleWithRegistersDelta, doubleWithRegisters, doubleWithRegistersIX, doubleWithRegistersIY)
import GroupCBIXIY exposing (singleByteMainRegsIYCB, singleEnvMainRegsIYCB)
import GroupED exposing (singleByteMainAndFlagsED, singleByteMainRegsED)
import Maybe.Extra exposing (oneOf)
import PCIncrement exposing (PCIncrement(..))
import SimpleFlagOps exposing (singleByteFlags)
import SimpleSingleByte exposing (singleByteMainRegs, singleByteMainRegsDD, singleByteMainRegsFD)
import SingleEnvWithMain exposing (applySingleEnvMainChange, singleEnvMainRegs)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters)
import SingleNoParams exposing (applyNoParamsDelta, applyRstDelta, singleNoParamCalls, singleWithNoParam)
import SingleWith8BitParameter exposing (maybeRelativeJump, singleWith8BitParam)
import TripleByte exposing (tripleByteWith16BitParam, tripleByteWith16BitParamDD, tripleByteWith16BitParamFD)
import TripleWithFlags exposing (triple16bitJumps)
import Z80Core exposing (CoreChange(..), Z80Core)
import Z80Env exposing (Z80Env)
import Z80Execute exposing (applyEdRegisterDelta, applyFlagDelta, applyJumpChangeDelta, applyPureDelta, applyRegisterDelta, applySimple8BitDelta, applyTripleChangeDelta, applyTripleFlagChange)
import Z80Mem exposing (m1, mem, mem16)
import Z80Rom exposing (CompiledZ80ROM, CpuInstruction, Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


fetchInstruction : Int -> CompiledZ80ROM -> CpuTimeCTime -> Int -> Z80Core -> CpuInstruction
fetchInstruction pc_value rom48k clockTime r_register z80_core =
    --let
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
    --in
    z80_core.env |> m1 pc_value (Bitwise.or z80_core.interrupts.ir (Bitwise.and r_register 0x7F)) rom48k clockTime


lengthAndDuration : Int -> Z80ROM -> Z80Env -> Maybe ( PCIncrement, InstructionDuration, CpuTimeCTime -> Z80ROM -> Z80Core -> CoreChange )
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
                z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) clockTime rom48k |> .value
        in
        param
            |> oneOf
                [ \fdinstruction ->
                    singleByteMainRegsFD
                        |> Dict.get fdinstruction
                        |> Maybe.map
                            (\( mainRegFunc, duration ) ->
                                ( IncrementByTwo, duration, \cpuClock z80rom z80core -> z80core |> applyRegisterDelta cpuClock (mainRegFunc z80core.main) z80rom )
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
                                , \cpuClock z80rom z80core ->
                                    z80core
                                        |> applyDoubleWithRegistersDelta cpuClock (f z80core.main doubleParam.value) z80rom
                                        |> CoreOnly
                                )
                            )
                , \fdinstruction ->
                    tripleByteWith16BitParamFD
                        |> Dict.get fdinstruction
                        |> Maybe.map
                            (\( mainRegFunc, duration ) ->
                                let
                                    doubleParam =
                                        z80env |> mem16 (Bitwise.and (pc + 2) 0xFFFF) rom48k clockTime
                                in
                                ( IncrementByFour, duration, \cpuClock z80rom z80core -> z80core |> applyTripleChangeDelta z80rom cpuClock (mainRegFunc doubleParam.value16) )
                            )
                , \fdinstruction ->
                    if fdinstruction == 0xCB then
                        let
                            iycboffset =
                                z80env |> mem (Bitwise.and (pc + 2) 0xFFFF) clockTime rom48k

                            iycbparam =
                                z80env |> mem (Bitwise.and (pc + 3) 0xFFFF) clockTime rom48k
                        in
                        ( iycbparam.value, iycboffset.value )
                            |> oneOf
                                [ \( cbparam, cboffset ) ->
                                    singleByteMainRegsIYCB
                                        |> Dict.get cbparam
                                        |> Maybe.map
                                            (\( mainRegFunc, duration ) ->
                                                ( IncrementByFour
                                                , duration
                                                , \cpuClock z80rom z80core ->
                                                    z80core |> applyRegisterDelta cpuClock (mainRegFunc cboffset z80core.main) z80rom
                                                )
                                            )
                                , \( cbparam, cboffset ) ->
                                    singleEnvMainRegsIYCB
                                        |> Dict.get cbparam
                                        |> Maybe.map
                                            (\( f, duration ) ->
                                                ( IncrementByFour
                                                , duration
                                                , \cpuClock z80rom z80_core ->
                                                    z80_core |> applySingleEnvMainChange cpuClock (f z80_core.main cboffset rom48k z80_core.env) z80rom
                                                )
                                            )
                                ]

                    else
                        Nothing
                ]

    else if opcode == 0xDD then
        let
            param =
                z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) clockTime rom48k |> .value
        in
        param
            |> oneOf
                [ \fdinstruction ->
                    singleByteMainRegsDD
                        |> Dict.get fdinstruction
                        |> Maybe.map
                            (\( mainRegFunc, duration ) ->
                                ( IncrementByTwo, duration, \cpuClock z80rom z80core -> z80core |> applyRegisterDelta cpuClock (mainRegFunc z80core.main) z80rom )
                            )
                , \fdinstruction ->
                    doubleWithRegistersIX
                        |> Dict.get fdinstruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                let
                                    doubleParam =
                                        z80env |> mem (Bitwise.and (pc + 2) 0xFFFF) clockTime rom48k
                                in
                                ( IncrementByThree
                                , duration
                                , \cpuClock z80rom z80core ->
                                    z80core
                                        |> applyDoubleWithRegistersDelta cpuClock (f z80core.main doubleParam.value) z80rom
                                        |> CoreOnly
                                )
                            )
                , \fdinstruction ->
                    tripleByteWith16BitParamDD
                        |> Dict.get fdinstruction
                        |> Maybe.map
                            (\( mainRegFunc, duration ) ->
                                let
                                    doubleParam =
                                        z80env |> mem16 (Bitwise.and (pc + 2) 0xFFFF) rom48k clockTime
                                in
                                ( IncrementByFour, duration, \cpuClock z80rom z80core -> z80core |> applyTripleChangeDelta z80rom cpuClock (mainRegFunc doubleParam.value16) )
                            )
                ]

    else if opcode == 0xED then
        let
            edQualifier =
                z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) clockTime rom48k |> .value
        in
        edQualifier
            |> oneOf
                [ \instruction ->
                    singleByteMainRegsED
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                ( IncrementByTwo
                                , duration
                                , \cpuClock z80rom z80core ->
                                    z80core |> applyEdRegisterDelta cpuClock f z80rom
                                )
                            )
                , \instruction ->
                    singleByteMainAndFlagsED
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, pcInc, duration ) ->
                                ( pcInc
                                , duration
                                , \cpuClock _ z80core ->
                                    z80core |> applyPureDelta cpuClock (f z80core.main z80core.flags) |> CoreOnly
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
                                ( IncrementByOne, duration, \cpuClock z80rom z80core -> z80core |> applyNoParamsDelta cpuClock noParamChange z80rom )
                            )
                , \instruction ->
                    singleNoParamCalls
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( noParamChange, duration ) ->
                                ( IncrementByOne, duration, \cpuClock _ z80core -> z80core |> applyRstDelta cpuClock noParamChange )
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
                                , \cpuClock z80rom z80core ->
                                    z80core |> applyTripleChangeDelta z80rom cpuClock (f doubleParam.value16)
                                )
                            )
                , \instruction ->
                    singleByteMainAndFlagRegisters
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                ( IncrementByOne, duration, \cpuClock _ z80core -> z80core |> applyPureDelta cpuClock (f z80core.main z80core.flags) |> CoreOnly )
                            )
                , \instruction ->
                    singleByteMainRegs
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( mainRegFunc, duration ) ->
                                ( IncrementByOne
                                , duration
                                , \cpuClock z80rom z80core ->
                                    z80core |> applyRegisterDelta cpuClock (mainRegFunc z80core.main) z80rom
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
                                ( IncrementByTwo, duration, \cpuClock z80rom z80core -> z80core |> applySimple8BitDelta cpuClock (f param.value) z80rom |> CoreOnly )
                            )
                , \instruction ->
                    singleByteFlags
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( flagFunc, duration ) ->
                                ( IncrementByOne, duration, \cpuClock z80rom z80core -> z80core |> applyFlagDelta cpuClock (flagFunc z80core.flags) z80rom )
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
                                , \cpuClock z80rom z80core ->
                                    z80core
                                        |> applyDoubleWithRegistersDelta cpuClock (f param.value) z80rom
                                        |> CoreOnly
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
                                ( IncrementByThree, duration, \_ _ z80core -> z80core |> applyTripleFlagChange (f doubleParam.value16) )
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
                                ( IncrementByTwo, duration, \_ _ z80core -> z80core |> applyJumpChangeDelta (f param.value) )
                            )
                , \instruction ->
                    singleEnvMainRegs
                        |> Dict.get instruction
                        |> Maybe.map
                            (\( f, duration ) ->
                                ( IncrementByOne
                                , duration
                                , \cpuClock z80rom z80core ->
                                    z80core |> applySingleEnvMainChange cpuClock (f z80core.main z80rom cpuClock z80core.env) z80rom
                                )
                            )
                ]
