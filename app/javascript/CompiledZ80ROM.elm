module CompiledZ80ROM exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration)
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement)
import Z80Core exposing (CoreChange, Z80Core)
import Z80Rom exposing (Z80ROM, getROMValue)


type alias CompiledInstruction =
    { function : CpuTimeCTime -> Z80ROM -> Z80Core -> CoreChange
    , duration : InstructionDuration
    , length : PCIncrement
    }


type alias CompiledZ80ROM =
    { z80rom : Z80ROM
    , compiled : Dict Int CompiledInstruction
    }


type CpuInstruction
    = UncompiledOpcode Int
    | Z80Compiled CompiledInstruction


getROMInstruction : Int -> CompiledZ80ROM -> CpuInstruction
getROMInstruction addr z80rom =
    case z80rom.compiled |> Dict.get addr of
        Just compiled ->
            Z80Compiled compiled

        Nothing ->
            let
                a =
                    getROMValue addr z80rom.z80rom
            in
            UncompiledOpcode a
