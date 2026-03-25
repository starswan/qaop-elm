module JumpChange exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime)
import SingleWith8BitParameter exposing (JumpChange(..))
import TripleWithFlags exposing (TripleWithFlagsChange(..))
import Utils exposing (shiftLeftBy8)
import Z80Core exposing (CoreChange(..), Z80Core)
import Z80Env exposing (setMem)


applyJumpChangeDelta : CpuTimeCTime -> JumpChange -> Z80Core -> CoreChange
applyJumpChangeDelta cpu_time z80changeData z80 =
    case z80changeData of
        ActualJumpOffset offset ->
            JumpWithOffset offset

        ConditionalJumpOffset offset shortDelay function ->
            if z80.flags |> function then
                JumpOffsetWithDelay offset shortDelay

            else
                NoCore

        DJNZOffset offset shortDelay ->
            let
                --case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
                --if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
                --PC=(char)v;} break;
                b =
                    Bitwise.and (z80.main.b - 1) 0xFF

                main =
                    z80.main
            in
            if b /= 0 then
                { main | b = b } |> MainWithOffsetAndDelay offset shortDelay

            else
                { main | b = b } |> MainOnly

        RegChangeStoreIndirect addr_f value ->
            let
                addr =
                    z80.main |> addr_f

                ( env_1, newTime ) =
                    z80.env |> setMem addr value cpu_time
            in
            { z80 | env = env_1 } |> CoreOnly

        SimpleNewHValue param ->
            let
                main =
                    z80.main

                int =
                    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and main.hl 0xFF)
            in
            { main | hl = int } |> MainOnly

        SimpleNewLValue param ->
            let
                main =
                    z80.main

                int =
                    Bitwise.or param (Bitwise.and main.hl 0xFF00)
            in
            { main | hl = int } |> MainOnly


applyTripleFlagChange : TripleWithFlagsChange -> Z80Core -> CoreChange
applyTripleFlagChange z80changeData z80 =
    case z80changeData of
        Conditional16BitJump int function ->
            if z80.flags |> function then
                JumpOnlyPC int

            else
                z80 |> CoreOnly

        Conditional16BitCall address shortdelay function ->
            if z80.flags |> function then
                CallWithPCAndDelay address shortdelay

            else
                z80 |> CoreOnly

        CallImmediate int ->
            CallWithPC int

        NewPCRegister int ->
            JumpOnlyPC int
