module Z80Delta exposing (..)

import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeCTime, CpuTimeIncrement, addCpuTimeTime)
import Z80Env exposing (Z80Env, addCpuTimeEnv, setMem, setMem16, z80_push)
import Z80Flags exposing (FlagRegisters, f_szh0n0p)
import Z80Types exposing (IXIYHL(..), InterruptRegisters, MainRegisters, MainWithIndexRegisters, Z80, add_cpu_time, set408bit)


type Z80Delta
    = Whole Z80
      -- 2 in GroupED
    | MainRegsWithPcAndCpuTime MainWithIndexRegisters Int CpuTimeCTime
      -- 1 in GroupED
    | FlagsWithMainAndTime FlagRegisters MainWithIndexRegisters Int
      -- 1 in GroupED
    | FlagsWithPCMainAndCpuTime FlagRegisters Int MainWithIndexRegisters CpuTimeCTime
      -- lots of these left
    | FlagRegs FlagRegisters
      -- 1 left in Group0xE0
    | MainRegs MainWithIndexRegisters
      --| MainRegsWithPc MainWithIndexRegisters Int
      -- 1 in GroupE0, 1 in GroupED
    | CpuTimeWithFlagsAndPc CpuTimeCTime FlagRegisters Int
      --1 left in GroupE0 EX (SP), HL
    | MainRegsWithEnv MainWithIndexRegisters Z80Env
      -- 1 left in GroupF0 LD SP, HL
    | SpAndCpuTimeWithPc Int Int Int
      -- 1 in GroupED, 1 in GroupE0
    | EnvWithPc Z80Env Int
      -- 1 in GroupED
    | CpuTimeWithSpAndPc CpuTimeCTime Int Int
      -- 1 in GroupE0
    | OnlyPc Int
      -- lots of these left
    | FlagsWithPcAndTime FlagRegisters Int CpuTimeCTime
      -- 1 in GroupED
    | InterruptsWithCpuTime InterruptRegisters CpuTimeCTime
    | SetImValue Int
      -- 2 in GroupE0
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
      -- 2 in GroupE0
      --| MainRegsWithEnvAndPc MainWithIndexRegisters Z80Env Int
      -- 1 left in Group70
    | SetMem8WithCpuTimeIncrementAndPc Int Int CpuTimeCTime Int Int
      -- 2 in GroupCB
    | PcTimeSet408Bit Int CpuTimeCTime Int Int
      -- 1 in GroupED
    | Fszh0n0pTimeDeltaSet408Bit Int Int Int


type alias DeltaWithChangesData =
    { delta : Z80Delta
    , interrupts : InterruptRegisters
    , pc : Int
    , time : CpuTimeCTime
    }


applyDeltaWithChanges : DeltaWithChangesData -> Z80 -> Z80
applyDeltaWithChanges z80delta z80 =
    let
        z80_env =
            z80.env
    in
    case z80delta.delta of
        Whole just_z80 ->
            just_z80

        MainRegsWithPcAndCpuTime mainRegisters pc cpu_time ->
            { z80 | pc = pc, env = { z80_env | time = cpu_time }, main = mainRegisters, interrupts = z80delta.interrupts }

        FlagRegs flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        EnvWithPc z80Env programCounter ->
            { z80 | env = z80Env, pc = programCounter, interrupts = z80delta.interrupts }

        FlagsWithMainAndTime flagRegisters mainWithIndexRegisters cpu_time ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { z80_env | time = z80delta.time |> addCpuTimeTime cpu_time }, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        SpAndCpuTimeWithPc sp cpu_time pc ->
            { z80 | pc = pc, env = { z80_env | time = z80delta.time |> addCpuTimeTime cpu_time, sp = sp }, interrupts = z80delta.interrupts }

        CpuTimeWithFlagsAndPc cpu_time flagRegisters pc ->
            { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = cpu_time }, interrupts = z80delta.interrupts }

        MainRegs mainWithIndexRegisters ->
            { z80 | main = mainWithIndexRegisters, pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        --MainRegsWithPc mainWithIndexRegisters pc ->
        --    { z80 | main = mainWithIndexRegisters, pc = pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }
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

        PcTimeSet408Bit pc cpuTimeCTime caseval result ->
            { z80 | pc = pc, env = { z80_env | time = cpuTimeCTime } } |> set408bit caseval result HL

        Fszh0n0pTimeDeltaSet408Bit timeDelta caseval result ->
            let
                z80_1 =
                    z80 |> set408bit caseval result HL
            in
            { z80_1 | flags = z80_1.flags |> f_szh0n0p result } |> add_cpu_time timeDelta

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



--delta_noop : Z80ROM -> Z80 -> Z80Delta
--delta_noop _ _ =
--    NoChange
--jp_delta : Bool -> Z80ROM -> Z80 -> Z80Delta
--jp_delta y rom48k z80 =
--    let
--        result =
--            z80 |> jp y rom48k
--    in
--    CpuTimeWithPc result.time result.pc
--jp : Bool -> Z80ROM -> Z80 -> CpuTimeAndPc
--jp y rom48k z80 =
--    let
--        a =
--            z80 |> imm16 rom48k
--    in
--    if y then
--        CpuTimeAndPc a.time a.value
--
--    else
--        CpuTimeAndPc a.time a.pc
--rst : Int -> Z80 -> (Int, Int)
--rst c z80 =
--z80 |> push z80.pc |> set_pc (c - 199)
--let
--    pushed =
--        z80.env |> z80_push z80.pc
--in
--(z80.pc, (c - 199))
--rst_delta : Int -> Z80 -> Z80Delta
--rst_delta value z80 =
--    --z80 |> rst_z80 0xC7
--    let
--        --result =
--        --    z80 |> rst value
--        result = (z80.pc, (value - 199))
--    in
--    --EnvWithPc result.env result.pc
--    PushWithPc (result |> Tuple.first) (result |> Tuple.second)
