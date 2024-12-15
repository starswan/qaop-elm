module Z80Transform exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTimeInc, cpuTimeIncrement10, cpuTimeIncrement4, cpuTimeIncrement6, cpuTimeIncrement8, increment7)
import Z80Env exposing (setMem16, z80_push)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (Z80, set_bc_main, set_de_main)


type InstructionLength
    = OneByteInstruction
    | TwoByteInstruction
    | ThreeByteInstruction
    | FourByteInstruction
    | JumpInstruction Int


type InstructionDuration
    = ZeroTStates
    | FourTStates
    | SixTStates
    | SevenTStates
    | EightTStates
    | TenTStates


type ChangeEnvOperation
    = Store16BitMemoryValue Int Int
    | DoNothing
    | ChangeSPRegister Int
    | PushValue Int


type ChangeMainOperation
    = ChangeBCRegister Int
    | ChangeBRegister Int
    | ChangeCRegister Int
    | ChangeDRegister Int
    | ChangeERegister Int
    | ChangeDERegister Int
    | ChangeHLRegister Int
    | ChangeIXRegister Int
    | ChangeIYRegister Int


type Z80Operation
    = ChangeEnv ChangeEnvOperation
    | ChangeMain ChangeMainOperation
    | ChangeFlagRegisters FlagRegisters


type alias Z80Transform =
    { pcIncrement : InstructionLength
    , time : CpuTimeCTime
    , timeIncrement : InstructionDuration
    , operation : Z80Operation
    }


executeTransform : Z80Transform -> Z80 -> Z80
executeTransform z80Transform z80 =
    let
        interrupts =
            z80.interrupts

        main =
            z80.main

        env =
            z80.env

        new_pc =
            case z80Transform.pcIncrement of
                OneByteInstruction ->
                    z80.pc + 1

                TwoByteInstruction ->
                    z80.pc + 2

                ThreeByteInstruction ->
                    z80.pc + 3

                FourByteInstruction ->
                    z80.pc + 4

                JumpInstruction pc_value ->
                    pc_value

        new_time =
            case z80Transform.timeIncrement of
                FourTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement4

                SixTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement6

                SevenTStates ->
                    z80Transform.time |> addCpuTimeTimeInc increment7

                ZeroTStates ->
                    z80Transform.time

                EightTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement8

                TenTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement10

    in
    case z80Transform.operation of
        ChangeEnv envChange ->
            let
                env1 =
                    case envChange of
                        Store16BitMemoryValue addr value ->
                            z80.env |> setMem16 addr value

                        ChangeSPRegister int ->
                            { env | sp = int }

                        DoNothing ->
                            z80.env

                        PushValue value ->
                            z80.env |> z80_push value
            in
            { z80 | pc = new_pc |> Bitwise.and 0xFFFF, env = { env1 | time = new_time }, interrupts = { interrupts | r = interrupts.r + 1 } }

        ChangeMain mainChange ->
            let
                main_1 =
                    case mainChange of
                        ChangeBCRegister int ->
                            main |> set_bc_main int

                        ChangeDERegister int ->
                            main |> set_de_main int

                        ChangeHLRegister int ->
                            { main | hl = int }

                        ChangeIXRegister int ->
                            { main | ix = int }

                        ChangeIYRegister int ->
                            { main | iy = int }

                        ChangeBRegister int ->
                            { main | b = int }

                        ChangeCRegister int ->
                            { main | c = int }

                        ChangeDRegister int ->
                            { main | d = int }

                        ChangeERegister int ->
                            { main | e = int }
            in
            { z80 | main = main_1, pc = new_pc |> Bitwise.and 0xFFFF, env = { env | time = new_time }, interrupts = { interrupts | r = interrupts.r + 1 } }

        ChangeFlagRegisters flagRegisters ->
            { z80 | flags = flagRegisters, pc = new_pc |> Bitwise.and 0xFFFF, env = { env | time = new_time }, interrupts = { interrupts | r = interrupts.r + 1 } }
