module Z80OpCode exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, CpuTimeCTime, InstructionDuration)
import Dict exposing (Dict)
import RegisterChange exposing (RegisterFlagChange(..))
import SimpleFlagOps exposing (singleByteFlags, singleByteFlagsDD, singleByteFlagsFD)
import SimpleSingleByte exposing (singleByteMainRegs, singleByteMainRegsDD, singleByteMainRegsFD)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters, singleByteMainAndFlagRegistersIX, singleByteMainAndFlagRegistersIY)
import SingleNoParams exposing (singleNoParamCalls, singleWithNoParam, singleWithNoParamDD, singleWithNoParamFD)
import Z80Core exposing (Z80Core)
import Z80Mem exposing (m1)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


singleByteMainFlagsRegs : Dict Int ( RegisterFlagChange, InstructionDuration )
singleByteMainFlagsRegs =
    singleByteMainRegs
        |> Dict.union singleByteFlags
        |> Dict.union singleWithNoParam
        |> Dict.union singleNoParamCalls
        |> Dict.union (singleByteMainAndFlagRegisters |> Dict.map (\_ ( f, duration ) -> ( RegisterZ80Change f, duration )))


singleByteMainFlagsRegsIY : Dict Int ( RegisterFlagChange, InstructionDuration )
singleByteMainFlagsRegsIY =
    singleByteFlagsFD
        |> Dict.union singleByteMainRegsFD
        |> Dict.union singleWithNoParamFD
        |> Dict.union (singleByteMainAndFlagRegistersIY |> Dict.map (\_ ( f, duration ) -> ( RegisterZ80Change f, duration )))


singleByteMainFlagsRegsIX : Dict Int ( RegisterFlagChange, InstructionDuration )
singleByteMainFlagsRegsIX =
    singleByteFlagsDD
        |> Dict.union singleByteMainRegsDD
        |> Dict.union singleWithNoParamDD
        |> Dict.union (singleByteMainAndFlagRegistersIX |> Dict.map (\_ ( f, duration ) -> ( RegisterZ80Change f, duration )))


fetchInstruction : Int -> Z80ROM -> CpuTimeCTime -> Int -> Z80Core -> CpuTimeAndValue
fetchInstruction pc_value rom48k clockTime r_register z80_core =
    --let
    --pc_value =
    --case romRoutineNames |> Dict.get z80.pc of
    --    Just name ->
    --        debugLog "fetch PC " name z80.pc
    --
    --    Nothing ->
    --z80_core.pc
    --in
    z80_core.env |> m1 pc_value (Bitwise.or z80_core.interrupts.ir (Bitwise.and r_register 0x7F)) rom48k clockTime
