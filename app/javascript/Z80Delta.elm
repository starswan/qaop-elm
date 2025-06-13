module Z80Delta exposing (..)

import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeCTime, CpuTimeIncrement, InstructionDuration, addCpuTimeTime)
import Utils exposing (toHexString2)
import Z80Core exposing (Z80, Z80Core, add_cpu_time, set408bitHL)
import Z80Debug exposing (debugTodo)
import Z80Env exposing (Z80Env, addCpuTimeEnv, setMem, setMem16, z80_push)
import Z80Flags exposing (FlagRegisters, f_szh0n0p)
import Z80Types exposing (IXIYHL(..), InterruptRegisters, MainRegisters, MainWithIndexRegisters, set_bc_main)


type Z80Delta
    = WholeCore Z80Core
    | MainRegsWithPcAndCpuTime MainWithIndexRegisters Int CpuTimeCTime
      --| FlagsWithPCMainAndTime FlagRegisters Int MainWithIndexRegisters CpuTimeIncrement
    | FlagsWithMainAndTime FlagRegisters MainWithIndexRegisters Int
    | FlagsWithPCMainAndCpuTime FlagRegisters Int MainWithIndexRegisters CpuTimeCTime
    | FlagRegs FlagRegisters
    | FlagRegsWithPc FlagRegisters Int
      --| MainRegs MainWithIndexRegisters
    | MainRegsWithPc MainWithIndexRegisters Int
    | CpuTimeWithFlags CpuTimeCTime FlagRegisters
    | EnvWithFlagsAndPc Z80Env FlagRegisters Int
    | CpuTimeWithFlagsAndPc CpuTimeCTime FlagRegisters Int
    | MainRegsWithEnv MainWithIndexRegisters Z80Env
    | SpAndCpuTimeWithPc Int Int Int
    | EnvWithPc Z80Env Int
    | CpuTimeWithSpAndPc CpuTimeCTime Int Int
    | OnlyPc Int
    | FlagsWithPcAndTime FlagRegisters Int CpuTimeCTime
    | InterruptsWithCpuTime InterruptRegisters CpuTimeCTime
    | SetImValue Int
    | MainRegsWithSpPcAndTime MainWithIndexRegisters Int Int CpuTimeCTime
      --| MainRegsWithEnvAndPc MainWithIndexRegisters Z80Env Int
      --| OnlyTime CpuTimeCTime
      --| MainRegsWithAltRegs MainWithIndexRegisters MainRegisters
    | OnlyPush Int
    | PushWithPc Int Int
    | PushWithCpuTimeAndPc Int CpuTimeCTime Int
    | PushWithMainSpCpuTimeAndPc Int MainWithIndexRegisters Int CpuTimeCTime Int
    | PushWithMainSpCpuTime Int MainWithIndexRegisters Int CpuTimeCTime
    | SetMem8WithTime Int Int Int
    | SetMem16WithTimeAndPc Int Int Int Int
    | SetMem8WithCpuTimeIncrementAndPc Int Int CpuTimeCTime Int Int
      --| PcTimeSet408Bit Int CpuTimeCTime Int Int
    | Fszh0n0pTimeDeltaSet408Bit Int Int Int
    | FlagsWithPcEnvAndCpuTime FlagRegisters Int Z80Env Int
    | UnknownIntValue String Int
    | HLBCWithFlagsAndPc Int Int FlagRegisters Int


type alias DeltaWithChangesData =
    { delta : Z80Delta
    , interrupts : InterruptRegisters
    , pc : Int
    , time : CpuTimeCTime
    }


applyDeltaWithChanges : DeltaWithChangesData -> Z80Core -> Z80Core
applyDeltaWithChanges z80delta z80 =
    let
        z80_env =
            z80.env
    in
    case z80delta.delta of
        WholeCore just_z80 ->
            just_z80

        HLBCWithFlagsAndPc hl bc flags pc ->
            let
                main =
                    z80.main |> set_bc_main bc
            in
            { z80 | pc = pc, flags = flags, main = { main | hl = hl }, env = { z80_env | time = z80delta.time } }

        MainRegsWithPcAndCpuTime mainRegisters pc cpu_time ->
            { z80 | pc = pc, env = { z80_env | time = cpu_time }, main = mainRegisters, interrupts = z80delta.interrupts }

        FlagRegs flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        FlagRegsWithPc flagRegisters pc ->
            { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        CpuTimeWithFlags time flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { z80_env | time = time }, interrupts = z80delta.interrupts }

        EnvWithPc z80Env programCounter ->
            { z80 | env = z80Env, pc = programCounter, interrupts = z80delta.interrupts }

        --FlagsWithPCMainAndTime flagRegisters pc mainWithIndexRegisters cpu_time ->
        --    { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = z80delta.time |> addCpuTimeTimeInc cpu_time }, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }
        FlagsWithMainAndTime flagRegisters mainWithIndexRegisters cpu_time ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { z80_env | time = z80delta.time |> addCpuTimeTime cpu_time }, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        SpAndCpuTimeWithPc sp cpu_time pc ->
            { z80 | pc = pc, env = { z80_env | time = z80delta.time |> addCpuTimeTime cpu_time, sp = sp }, interrupts = z80delta.interrupts }

        EnvWithFlagsAndPc z80Env flagRegisters pc ->
            { z80 | flags = flagRegisters, pc = pc, env = z80Env, interrupts = z80delta.interrupts }

        CpuTimeWithFlagsAndPc cpu_time flagRegisters pc ->
            { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = cpu_time }, interrupts = z80delta.interrupts }

        --MainRegs mainWithIndexRegisters ->
        --    { z80 | main = mainWithIndexRegisters, pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }
        MainRegsWithPc mainWithIndexRegisters pc ->
            { z80 | main = mainWithIndexRegisters, pc = pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        OnlyPc pc ->
            { z80 | pc = pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        FlagsWithPcAndTime flags pc time ->
            { z80 | pc = pc, flags = flags, env = { z80_env | time = time }, interrupts = z80delta.interrupts }

        CpuTimeWithSpAndPc time sp pc ->
            { z80 | pc = pc, env = { z80_env | time = time, sp = sp }, interrupts = z80delta.interrupts }

        InterruptsWithCpuTime interruptRegisters cpuTimeCTime ->
            { z80 | pc = z80delta.pc, env = { z80_env | time = cpuTimeCTime }, interrupts = interruptRegisters }

        SetImValue imvalue ->
            let
                interruptRegisters =
                    z80.interrupts
            in
            { z80 | pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = { interruptRegisters | iM = imvalue } }

        MainRegsWithSpPcAndTime main sp pc time ->
            { z80 | main = main, pc = pc, env = { z80_env | sp = sp, time = time }, interrupts = z80delta.interrupts }

        MainRegsWithEnv mainRegisters z80Env ->
            { z80 | env = z80Env, pc = z80delta.pc, main = mainRegisters, interrupts = z80delta.interrupts }

        FlagsWithPCMainAndCpuTime flagRegisters pc mainWithIndexRegisters time ->
            { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = time }, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        OnlyPush value ->
            { z80 | pc = z80delta.pc, env = { z80_env | time = z80delta.time } |> z80_push value, interrupts = z80delta.interrupts }

        PushWithPc value pc ->
            let
                env =
                    z80.env
            in
            { z80 | pc = pc, env = { env | time = z80delta.time } |> z80_push value, interrupts = z80delta.interrupts }

        PushWithCpuTimeAndPc value time pc ->
            { z80 | pc = pc, env = { z80_env | time = time } |> z80_push value, interrupts = z80delta.interrupts }

        SetMem8WithTime addr value time ->
            { z80 | pc = z80delta.pc, env = z80.env |> setMem addr value |> addCpuTimeEnv time, interrupts = z80delta.interrupts }

        SetMem16WithTimeAndPc addr value time pc ->
            { z80 | pc = pc, env = z80.env |> setMem16 addr value |> addCpuTimeEnv time, interrupts = z80delta.interrupts }

        SetMem8WithCpuTimeIncrementAndPc addr value cpuTimeCTime time pc ->
            { z80 | pc = pc, env = { z80_env | time = cpuTimeCTime } |> setMem addr value |> addCpuTimeEnv time, interrupts = z80delta.interrupts }

        --PcTimeSet408Bit pc cpuTimeCTime caseval result ->
        --    let
        --        ( main, flags, env ) =
        --            set408bitHL caseval result ( z80.main, z80.flags, { z80_env | time = cpuTimeCTime } )
        --    in
        --    --{ z80 | pc = pc, env = { z80_env | time = cpuTimeCTime } } |> set408bitHL caseval result
        --    { z80 | pc = pc, env = env, flags = flags, main = main }
        Fszh0n0pTimeDeltaSet408Bit timeDelta caseval result ->
            let
                ( main, flags, env ) =
                    set408bitHL caseval result ( z80.main, z80.flags, z80_env )

                --z80_1 =
                --    z80 |> set408bitHL caseval result
            in
            --{ z80_1 | flags = z80_1.flags |> f_szh0n0p result } |> add_cpu_time timeDelta
            { z80 | flags = flags |> f_szh0n0p result, env = env, main = main } |> add_cpu_time timeDelta

        PushWithMainSpCpuTimeAndPc value mainWithIndexRegisters sp time pc ->
            let
                env =
                    z80.env
            in
            { z80 | main = mainWithIndexRegisters, pc = pc, env = { env | time = time, sp = sp } |> z80_push value, interrupts = z80delta.interrupts }

        PushWithMainSpCpuTime value mainWithIndexRegisters sp time ->
            let
                env =
                    z80.env
            in
            { z80 | main = mainWithIndexRegisters, pc = z80delta.pc, env = { env | time = time, sp = sp } |> z80_push value, interrupts = z80delta.interrupts }

        FlagsWithPcEnvAndCpuTime flagRegisters pc z80Env int ->
            { z80 | flags = flagRegisters, pc = pc, env = z80Env |> addCpuTimeEnv int, interrupts = z80delta.interrupts }

        UnknownIntValue string int ->
            debugTodo string (int |> toHexString2) z80
