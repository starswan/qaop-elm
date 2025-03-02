module Z80Parser exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTime)
import Dict
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement)
import SingleWith8BitParameter exposing (doubleWithRegisters, maybeRelativeJump)
import TripleByte exposing (tripleByteWith16BitParam)
import TripleWithFlags exposing (triple16WithFlags)
import TripleWithMain exposing (TripleMainChange, applyTripleMainChange, tripleMainRegs)
import Z80Address exposing (addIndexOffset, incrementBy1, incrementBy2, toInt)
import Z80Env exposing (mem, mem16)
import Z80Execute exposing (DeltaWithChanges(..), applyTripleChangeDelta, applyTripleFlagChange)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (Z80)


parseTripleMain : Int -> Z80ROM -> PCIncrement -> Z80 -> Maybe Z80
parseTripleMain instrCode rom48k paramOffset z80 =
    case tripleMainRegs |> Dict.get instrCode of
        Just ( f, pcInc ) ->
            let
                --doubleParam =
                --    z80.env |> mem16 ((z80.pc |> addIndexOffset paramOffset) |> toInt) rom48k
                doubleParam =
                    case paramOffset of
                        IncrementByOne ->
                            z80.env |> mem16 ((z80.pc |> incrementBy1) |> toInt) rom48k

                        IncrementByTwo ->
                            z80.env |> mem16 ((z80.pc |> incrementBy2) |> toInt) rom48k
            in
            -- duplicate of code in imm16 - add 6 to the cpu_time
            Just (z80 |> applyTripleMainChange (doubleParam.time |> addCpuTimeTime 6) pcInc (f doubleParam.value z80.main))

        Nothing ->
            Nothing



--| TripleFlagDelta CpuTimeCTime TripleWithFlagsChange
--    TripleFlagDelta cpuTimeCTime tripleWithFlagsChange ->
--        z80 |> applyTripleFlagChange cpuTimeCTime tripleWithFlagsChange


parseTriple16Flags : Int -> Z80ROM -> PCIncrement -> Z80 -> Maybe Z80
parseTriple16Flags instrCode rom48k paramOffset z80 =
    case triple16WithFlags |> Dict.get instrCode of
        Just f ->
            let
                --doubleParam =
                --    z80.env |> mem16 ((z80.pc |> addIndexOffset paramOffset) |> toInt) rom48k
                doubleParam =
                    case paramOffset of
                        IncrementByOne ->
                            z80.env |> mem16 ((z80.pc |> incrementBy1) |> toInt) rom48k

                        IncrementByTwo ->
                            z80.env |> mem16 ((z80.pc |> incrementBy2) |> toInt) rom48k
            in
            -- duplicate of code in imm16 - add 6 to the cpu_time
            Just (z80 |> applyTripleFlagChange (doubleParam.time |> addCpuTimeTime 6) (f doubleParam.value z80.flags))

        Nothing ->
            Nothing


parseTriple16Param : Int -> Z80ROM -> PCIncrement -> Z80 -> Maybe Z80
parseTriple16Param instrCode rom48k paramOffset z80 =
    case tripleByteWith16BitParam |> Dict.get instrCode of
        Just ( f, pcInc ) ->
            let
                --doubleParam =
                --    z80.env |> mem16 ((z80.pc |> addIndexOffset paramOffset) |> toInt) rom48k
                doubleParam =
                    case paramOffset of
                        IncrementByOne ->
                            z80.env |> mem16 ((z80.pc |> incrementBy1) |> toInt) rom48k

                        IncrementByTwo ->
                            z80.env |> mem16 ((z80.pc |> incrementBy2) |> toInt) rom48k
            in
            -- duplicate of code in imm16 - add 6 to the cpu_time
            --Just (TripleChangeDelta pcInc (doubleParam.time |> addCpuTimeTime 6) (f doubleParam.value))
            Just (z80 |> applyTripleChangeDelta rom48k pcInc (doubleParam.time |> addCpuTimeTime 6) (f doubleParam.value))

        Nothing ->
            Nothing


parseRelativeJump : Int -> Z80ROM -> CpuTimeCTime -> Z80 -> Maybe DeltaWithChanges
parseRelativeJump instrCode rom48k instrTime z80 =
    case maybeRelativeJump |> Dict.get instrCode of
        Just f ->
            let
                param =
                    mem (z80.pc |> incrementBy1 |> toInt) instrTime rom48k z80.env.ram
            in
            -- duplicate of code in imm8 - add 3 to the cpu_time
            Just (JumpChangeDelta (param.time |> addCpuTimeTime 3) (f param.value z80.flags))

        Nothing ->
            Nothing


parseDoubleWithRegs : Int -> Z80ROM -> CpuTimeCTime -> Z80 -> Maybe DeltaWithChanges
parseDoubleWithRegs instrCode rom48k instrTime z80 =
    case doubleWithRegisters |> Dict.get instrCode of
        Just ( f, pcInc ) ->
            let
                param =
                    case pcInc of
                        IncreaseByTwo ->
                            mem (z80.pc |> incrementBy1 |> toInt) instrTime rom48k z80.env.ram

                        IncreaseByThree ->
                            mem (z80.pc |> incrementBy2 |> toInt) instrTime rom48k z80.env.ram
            in
            -- duplicate of code in imm8 - add 3 to the cpu_time
            Just (DoubleWithRegistersDelta pcInc (param.time |> addCpuTimeTime 3) (f z80.main param.value))

        Nothing ->
            Nothing
