module Z80OpCode exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, CpuTimeCTime, InstructionDuration)
import Dict exposing (Dict)
import RegisterChange exposing (RegisterFlagChange)
import SimpleFlagOps exposing (singleByteFlags)
import SimpleSingleByte exposing (singleByteMainRegs)
import Z80Core exposing (Z80Core)
import Z80Mem exposing (m1)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


singleByteMainFlagsRegs : Dict Int ( RegisterFlagChange, InstructionDuration )
singleByteMainFlagsRegs =
    singleByteMainRegs |> Dict.union singleByteFlags


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
