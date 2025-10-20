module Z80OpCode exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, InstructionDuration)
import Z80Core exposing (Z80Core)
import Z80Mem exposing (m1)
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
