module Z80Transform exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement, addCpuTimeTimeInc, cpuTimeIncrement10, cpuTimeIncrement11, cpuTimeIncrement15, cpuTimeIncrement4, cpuTimeIncrement5, cpuTimeIncrement6, cpuTimeIncrement8, increment7)
import TransformTypes exposing (InstructionDuration(..))
import Utils exposing (shiftLeftBy8, shiftRightBy8)
import Z80Env exposing (Z80Env, addCpuTimeEnvInc, mem, setMem, setMem16, setRam, z80_push)
import Z80Flags exposing (FlagRegisters, IntWithFlags, adc, dec, inc, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7)
import Z80Ram exposing (getRamValue, setRamValue)
import Z80Types exposing (MainWithIndexRegisters, Z80, set_bc_main, set_de_main)


type InstructionLength
    = OneByteInstruction
    | TwoByteInstruction
    | ThreeByteInstruction
    | FourByteInstruction
    | JumpInstruction Int


type ChangeEnvOperation
    = Store16BitMemoryValue Int Int
    | Store8BitMemoryValue Int Int
    | ChangeSPRegister Int
    | PushValue Int


type ChangeEnvWithFlagsOperation
    = ApplyShifter0 Int
    | ApplyShifter1 Int
    | ApplyShifter2 Int
    | ApplyShifter3 Int
    | ApplyShifter4 Int
    | ApplyShifter5 Int
    | ApplyShifter6 Int
    | ApplyShifter7 Int


type ChangeMemoryOperation
    = IncrementMemory Int
    | DecrementMemory Int


type ChangeMainOperation
    = ChangeBCRegister Int
    | ChangeRegisterBAndC Int Int
    | ChangeRegisterDAndE Int Int
    | ChangeMainDEAndHL Int Int
    | ChangeBRegister Int
    | ChangeCRegister Int
    | ChangeDRegister Int
    | ChangeERegister Int
    | ChangeHRegister Int
    | ChangeIXHRegister Int
    | ChangeIXLRegister Int
    | ChangeIYHRegister Int
    | ChangeIYLRegister Int
    | ChangeLRegister Int
    | ChangeDERegister Int
    | ChangeHLRegister Int
    | ChangeIXRegister Int
    | ChangeIYRegister Int
    | ChangeMainRegisters MainWithIndexRegisters


type ChangeMainFlagsOperation
    = ChangeMainFlagsHL Int
    | ChangeMainFlagsIX Int
    | ChangeMainFlagsIY Int
    | ChangeMainFlagsB Int
    | ChangeMainFlagsC Int
    | ChangeMainFlagsD Int
    | ChangeMainFlagsE Int


type Z80Operation
    = ChangeEnv ChangeEnvOperation
    | ChangeEnvWithFlags ChangeEnvWithFlagsOperation
    | ChangeMain ChangeMainOperation
    | ChangeMemory ChangeMemoryOperation
    | ChangeMainWithFlags ChangeMainFlagsOperation FlagRegisters
    | ChangeFlagRegisters FlagRegisters
    | ChangeNothing


type alias Z80Transform =
    { pcIncrement : InstructionLength
    , time : CpuTimeCTime
    , timeIncrement : InstructionDuration
    , operation : Z80Operation
    }


executeTransform : Z80Transform -> Z80 -> Z80
executeTransform z80Transform z80 =
    let
        new_pc =
            case z80Transform.pcIncrement of
                OneByteInstruction ->
                    z80.pc + 1 |> Bitwise.and 0xFFFF

                TwoByteInstruction ->
                    z80.pc + 2 |> Bitwise.and 0xFFFF

                ThreeByteInstruction ->
                    z80.pc + 3 |> Bitwise.and 0xFFFF

                FourByteInstruction ->
                    z80.pc + 4 |> Bitwise.and 0xFFFF

                JumpInstruction pc_value ->
                    pc_value

        new_time =
            case z80Transform.timeIncrement of
                FourTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement4

                FiveTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement5

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

                FifteenTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement15

                ElevenTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement11
    in
    case z80Transform.operation of
        ChangeEnv envChange ->
            let
                env =
                    z80.env

                env1 =
                    case envChange of
                        Store16BitMemoryValue addr value ->
                            env |> setMem16 addr value

                        ChangeSPRegister int ->
                            { env | sp = int }

                        PushValue value ->
                            env |> z80_push value

                        Store8BitMemoryValue addr value ->
                            env |> setMem addr value
            in
            { z80 | pc = new_pc, env = { env1 | time = new_time } }

        ChangeEnvWithFlags envChange ->
            let
                ( env1, flags ) =
                    case envChange of
                        ApplyShifter0 addr ->
                            z80 |> applyShifter shifter0 addr

                        ApplyShifter1 addr ->
                            z80 |> applyShifter shifter1 addr

                        ApplyShifter2 addr ->
                            z80 |> applyShifter shifter2 addr

                        ApplyShifter3 addr ->
                            z80 |> applyShifter shifter3 addr

                        ApplyShifter4 addr ->
                            z80 |> applyShifter shifter4 addr

                        ApplyShifter5 addr ->
                            z80 |> applyShifter shifter5 addr

                        ApplyShifter6 addr ->
                            z80 |> applyShifter shifter6 addr

                        ApplyShifter7 addr ->
                            z80 |> applyShifter shifter7 addr
            in
            { z80 | pc = new_pc, env = { env1 | time = new_time }, flags = flags }

        ChangeMain mainChange ->
            let
                main =
                    z80.main

                env =
                    z80.env

                main_1 =
                    case mainChange of
                        ChangeBCRegister int ->
                            main |> set_bc_main int

                        ChangeRegisterBAndC b_value c_value ->
                            { main | b = b_value, c = c_value }

                        ChangeRegisterDAndE d_value e_value ->
                            { main | d = d_value, e = e_value }

                        ChangeMainDEAndHL de_value hl_value ->
                            { main | hl = hl_value } |> set_de_main de_value

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

                        ChangeHRegister int ->
                            let
                                l =
                                    main.hl |> Bitwise.and 0xFF

                                h =
                                    int |> shiftLeftBy8
                            in
                            { main | hl = h |> Bitwise.or l }

                        ChangeIXHRegister int ->
                            let
                                l =
                                    main.ix |> Bitwise.and 0xFF

                                h =
                                    int |> shiftLeftBy8
                            in
                            { main | ix = h |> Bitwise.or l }

                        ChangeIYHRegister int ->
                            let
                                l =
                                    main.iy |> Bitwise.and 0xFF

                                h =
                                    int |> shiftLeftBy8
                            in
                            { main | iy = h |> Bitwise.or l }

                        ChangeLRegister int ->
                            let
                                h =
                                    main.hl |> Bitwise.and 0xFF00
                            in
                            { main | hl = h |> Bitwise.or int }

                        ChangeIXLRegister int ->
                            let
                                h =
                                    main.ix |> Bitwise.and 0xFF00
                            in
                            { main | ix = h |> Bitwise.or int }

                        ChangeIYLRegister int ->
                            let
                                h =
                                    main.iy |> Bitwise.and 0xFF00
                            in
                            { main | iy = h |> Bitwise.or int }

                        ChangeMainRegisters mainWithIndexRegisters ->
                            mainWithIndexRegisters
            in
            { z80 | main = main_1, pc = new_pc, env = { env | time = new_time } }

        ChangeFlagRegisters flagRegisters ->
            let
                env =
                    z80.env
            in
            { z80 | flags = flagRegisters, pc = new_pc, env = { env | time = new_time } }

        ChangeMainWithFlags mainFlagChange flagRegisters ->
            let
                main =
                    z80.main

                env =
                    z80.env

                main_1 =
                    case mainFlagChange of
                        ChangeMainFlagsHL int ->
                            { main | hl = int }

                        ChangeMainFlagsIX int ->
                            { main | ix = int }

                        ChangeMainFlagsIY int ->
                            { main | iy = int }

                        ChangeMainFlagsB int ->
                            { main | b = int }

                        ChangeMainFlagsC int ->
                            { main | c = int }

                        ChangeMainFlagsD int ->
                            { main | d = int }

                        ChangeMainFlagsE int ->
                            { main | e = int }
            in
            { z80 | main = main_1, flags = flagRegisters, pc = new_pc, env = { env | time = new_time } }

        ChangeMemory changeMemoryOperation ->
            let
                env =
                    z80.env

                ( env1, flags ) =
                    case changeMemoryOperation of
                        IncrementMemory ramAddr ->
                            let
                                value =
                                    env.ram |> getRamValue ramAddr

                                valueWithFlags =
                                    z80.flags |> inc value
                            in
                            ( { env | ram = env.ram |> setRamValue ramAddr valueWithFlags.value }, valueWithFlags.flags )

                        DecrementMemory ramAddr ->
                            let
                                value =
                                    env.ram |> getRamValue ramAddr

                                valueWithFlags =
                                    z80.flags |> dec value
                            in
                            ( { env | ram = env.ram |> setRamValue ramAddr valueWithFlags.value }, valueWithFlags.flags )
            in
            { z80 | flags = flags, pc = new_pc, env = { env1 | time = new_time } }

        ChangeNothing ->
            let
                env =
                    z80.env
            in
            { z80 | pc = new_pc, env = { env | time = new_time } }


applyShifter : (Int -> FlagRegisters -> IntWithFlags) -> Int -> Z80 -> ( Z80Env, FlagRegisters )
applyShifter shifterFunc ramAddr z80 =
    let
        value =
            z80.env.ram |> getRamValue ramAddr

        result =
            z80.flags |> shifterFunc value

        env =
            z80.env

        --interrupts =
        --    z80.interrupts
        --env_1 =
        --    { env | time = value.time }
        env_2 =
            { env | ram = env.ram |> setRamValue ramAddr result.value }
    in
    --{ z80 | pc = new_pc, env = env_2, interrupts = { interrupts | r = interrupts.r + 1 } }
    ( env_2, result.flags )
