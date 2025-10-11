module Z80Delta exposing (..)

import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeCTime, CpuTimeIncrement, InstructionDuration, addCpuTimeTime)
import Utils exposing (toHexString2)
import Z80Core exposing (Z80, Z80Core)
import Z80Debug exposing (debugTodo)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (InterruptRegisters, MainRegisters, MainWithIndexRegisters, set_bc_main)


type
    Z80Delta
    -- only used by LDIR
    = WholeCore Z80Core
    | MainRegsWithPcAndCpuTime MainWithIndexRegisters Int CpuTimeCTime
    | FlagsWithPCMainAndCpuTime FlagRegisters Int MainWithIndexRegisters CpuTimeCTime
    | CpuTimeWithFlagsAndPc CpuTimeCTime FlagRegisters Int
    | EnvWithPcAndTime Z80Env Int CpuTimeCTime
      -- only used by ED78
    | CpuTimeWithSpAndPc CpuTimeCTime Int Int
    | InterruptsWithCpuTime InterruptRegisters CpuTimeCTime
    | MainRegsWithCpuTime MainWithIndexRegisters CpuTimeCTime
      -- only used by RLD
    | FlagsWithPcEnvAndCpuTime FlagRegisters Int Z80Env Int
    | UnknownIntValue String Int
      -- only used by CPIR
    | HLBCWithFlagsAndPc Int Int FlagRegisters Int
    | NewAValue Int
    | NoOp


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
            { z80 | pc = pc, flags = flags, main = { main | hl = hl }, env = z80_env, clockTime = z80delta.time }

        MainRegsWithPcAndCpuTime mainRegisters pc cpu_time ->
            { z80 | pc = pc, env = z80_env, clockTime = cpu_time, main = mainRegisters, interrupts = z80delta.interrupts }

        EnvWithPcAndTime z80Env programCounter clockTime ->
            { z80 | env = z80Env, pc = programCounter, interrupts = z80delta.interrupts, clockTime = clockTime }

        CpuTimeWithFlagsAndPc cpu_time flagRegisters pc ->
            { z80 | flags = flagRegisters, pc = pc, env = z80_env, clockTime = cpu_time, interrupts = z80delta.interrupts }

        CpuTimeWithSpAndPc cpu_time sp pc ->
            { z80 | pc = pc, env = { z80_env | sp = sp }, clockTime = cpu_time, interrupts = z80delta.interrupts }

        InterruptsWithCpuTime interruptRegisters cpuTimeCTime ->
            { z80 | pc = z80delta.pc, env = z80_env, clockTime = cpuTimeCTime, interrupts = interruptRegisters }

        FlagsWithPCMainAndCpuTime flagRegisters pc mainWithIndexRegisters time ->
            { z80 | flags = flagRegisters, pc = pc, env = z80_env, clockTime = time, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        FlagsWithPcEnvAndCpuTime flagRegisters pc z80Env int ->
            { z80 | flags = flagRegisters, pc = pc, env = z80Env, clockTime = z80.clockTime |> addCpuTimeTime int, interrupts = z80delta.interrupts }

        UnknownIntValue string int ->
            debugTodo string (int |> toHexString2) z80

        NoOp ->
            { z80 | pc = z80delta.pc, env = z80_env, clockTime = z80delta.time, interrupts = z80delta.interrupts }

        NewAValue int ->
            let
                flags =
                    z80.flags
            in
            { z80 | pc = z80delta.pc, flags = { flags | a = int }, env = z80_env, clockTime = z80delta.time, interrupts = z80delta.interrupts }

        MainRegsWithCpuTime mainWithIndexRegisters cpuTimeCTime ->
            { z80 | pc = z80delta.pc, env = z80_env, clockTime = cpuTimeCTime, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }
