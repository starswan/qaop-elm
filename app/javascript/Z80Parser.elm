module Z80Parser exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTime)
import Dict
import PCIncrement exposing (MediumPCIncrement(..), TriplePCIncrement)
import SingleWith8BitParameter exposing (JumpChange(..), applySimple8BitDelta, doubleWithRegisters, maybeRelativeJump, singleWith8BitParam)
import Z80Env exposing (mem)
import Z80Execute exposing (DeltaWithChanges(..))
import Z80Rom exposing (Z80ROM)
import Z80Transform exposing (ChangeEnvOperation(..), InstructionDuration(..), InstructionLength(..), Z80Operation(..), Z80Transform)
import Z80Types exposing (Z80)


parseRelativeJump : Int -> Z80ROM -> CpuTimeCTime -> Z80 -> Maybe Z80Transform
parseRelativeJump instrCode rom48k instrTime z80 =
    case maybeRelativeJump |> Dict.get instrCode of
        Just f ->
            let
                param =
                    mem (Bitwise.and (z80.pc + 1) 0xFFFF) instrTime rom48k z80.env.ram
            in
            case f param.value z80.flags of
                ActualJump jump ->
                    Just
                        { pcIncrement = JumpInstruction (Bitwise.and (z80.pc + 2 + jump) 0xFFFF)
                        , time = instrTime
                        , timeIncrement = EightTStates
                        , operation = ChangeEnv DoNothing
                        }

                NoJump ->
                    Just
                        { pcIncrement = TwoByteInstruction
                        , time = instrTime
                        , timeIncrement = EightTStates
                        , operation = ChangeEnv DoNothing
                        }

                FlagJump flagRegisters ->
                    Just
                        { pcIncrement = TwoByteInstruction
                        , time = instrTime
                        , timeIncrement = ZeroTStates
                        , operation = ChangeFlagRegisters flagRegisters
                        }

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
                            mem (Bitwise.and (z80.pc + 1) 0xFFFF) instrTime rom48k z80.env.ram

                        IncreaseByThree ->
                            mem (Bitwise.and (z80.pc + 2) 0xFFFF) instrTime rom48k z80.env.ram
            in
            -- duplicate of code in imm8 - add 3 to the cpu_time
            Just (DoubleWithRegistersDelta pcInc (param.time |> addCpuTimeTime 3) (f z80.main param.value))

        Nothing ->
            Nothing


parseSingleByteWithParam : CpuTimeCTime -> Int -> Z80ROM -> Z80 -> Maybe Z80Transform
parseSingleByteWithParam ctime instr_code rom48k z80 =
    case singleWith8BitParam |> Dict.get instr_code of
        Just ( f, pcInc ) ->
            let
                param =
                    case pcInc of
                        IncreaseByTwo ->
                            mem (Bitwise.and (z80.pc + 1) 0xFFFF) ctime rom48k z80.env.ram

                        IncreaseByThree ->
                            mem (Bitwise.and (z80.pc + 2) 0xFFFF) ctime rom48k z80.env.ram

                -- duplicate of code in imm8 - add 3 to the cpu_time
                x =
                    z80 |> applySimple8BitDelta pcInc (param.time |> addCpuTimeTime 3) (f param.value)
            in
            Just x

        Nothing ->
            Nothing
