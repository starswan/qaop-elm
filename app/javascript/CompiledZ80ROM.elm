module CompiledZ80ROM exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration)
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement)
import Z80Core exposing (CoreChange, Z80Core)
import Z80Debug exposing (debugTodo)
import Z80Types exposing (Z80ROM)


type alias CompiledZ80ROM =
    { z80rom : Z80ROM
    , compiled : Dict Int ( CpuTimeCTime -> Z80ROM -> Z80Core -> CoreChange, InstructionDuration, PCIncrement )

    --, compiled : Dict Int CpuInstruction
    }


type CpuInstruction
    = UncompiledOpcode Int CpuTimeCTime
    | Z80Compiled (CpuTimeCTime -> Z80ROM -> Z80Core -> CoreChange) InstructionDuration PCIncrement


getROMInstruction : Int -> CpuTimeCTime -> CompiledZ80ROM -> CpuInstruction
getROMInstruction addr clockTime z80rom =
    case z80rom.compiled |> Dict.get addr of
        Just ( f, duration, length ) ->
            Z80Compiled f duration length

        --Just inst ->
        --    inst
        Nothing ->
            case Dict.get addr z80rom.z80rom.rom48k of
                Just a ->
                    UncompiledOpcode a clockTime

                Nothing ->
                    debugTodo "getROMValue" (String.fromInt addr) UncompiledOpcode -1 clockTime
