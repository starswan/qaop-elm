module Z80OpCode exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, InstructionDuration, addDuration)
import Dict
import DoubleWithRegisters exposing (applyDoubleWithRegistersDelta, doubleWithRegisters)
import GroupCB exposing (singleByteMainAndFlagRegistersCB, singleByteMainRegsCB, singleEnvMainRegsCB)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import SimpleFlagOps exposing (singleByteFlags, singleByteFlagsCB)
import SimpleSingleByte exposing (singleByteMainRegs)
import SingleByteWithEnv exposing (applyEnvChangeDelta, singleByteZ80Env)
import SingleEnvWithMain exposing (applySingleEnvMainChange, singleEnvMainRegs)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters)
import SingleNoParams exposing (applyNoParamsDelta, singleWithNoParam)
import SingleWith8BitParameter exposing (maybeRelativeJump, singleWith8BitParam)
import TripleByte exposing (tripleByteWith16BitParam)
import TripleWithFlags exposing (triple16WithFlags)
import TripleWithMain exposing (applyTripleMainChange, tripleMainRegs)
import Z80Core exposing (Z80Core)
import Z80Env exposing (m1, mem, mem16)
import Z80Execute exposing (DeltaWithChanges(..), applyFlagDelta, applyJumpChangeDelta, applyPureDelta, applyRegisterDelta, applySimple8BitDelta, applyTripleChangeDelta, applyTripleFlagChange)
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
