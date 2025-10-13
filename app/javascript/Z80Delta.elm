module Z80Delta exposing (..)

import Utils exposing (toHexString2)
import Z80Core exposing (CoreChange(..), Z80Core)
import Z80Debug exposing (debugTodo)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (InterruptRegisters, MainRegisters, MainWithIndexRegisters)


type
    Z80Delta
    -- only used by LDIR
    --= WholeCore Z80Core
    = MainRegsWithPcAndCpuTime MainWithIndexRegisters Int
    | FlagsWithPCMainAndCpuTime FlagRegisters Int MainWithIndexRegisters
    | CpuTimeWithFlagsAndPc FlagRegisters Int
    | EnvWithPcAndTime Z80Env Int
      -- only used by ED78
    | CpuTimeWithSpAndPc Int Int
    | InterruptsWithCpuTime InterruptRegisters
      --| MainRegsWithCpuTime MainWithIndexRegisters CpuTimeCTime
      -- only used by RLD
    | FlagsWithPcEnvAndCpuTime FlagRegisters Int Z80Env
    | UnknownIntValue String Int
      -- only used by CPIR
      --| HLBCWithFlagsAndPc Int Int FlagRegisters Int
      --| NewAValue Int
    | NoOp


type alias DeltaWithChangesData =
    { delta : Z80Delta
    , interrupts : InterruptRegisters
    , pc : Int

    --, time : CpuTimeCTime
    }


applyDeltaWithChanges : DeltaWithChangesData -> Z80Core -> CoreChange
applyDeltaWithChanges z80delta z80 =
    let
        z80_env =
            z80.env
    in
    case z80delta.delta of
        --WholeCore just_z80 ->
        --    just_z80 |> CoreOnly
        --HLBCWithFlagsAndPc hl bc flags pc ->
        --    let
        --        main =
        --            z80.main |> set_bc_main bc
        --    in
        --    { z80 | flags = flags, main = { main | hl = hl }, env = z80_env }
        MainRegsWithPcAndCpuTime mainRegisters pc ->
            { z80 | env = z80_env, main = mainRegisters, interrupts = z80delta.interrupts } |> CoreWithPC pc

        EnvWithPcAndTime z80Env programCounter ->
            { z80 | env = z80Env, interrupts = z80delta.interrupts } |> CoreWithPC programCounter

        CpuTimeWithFlagsAndPc flagRegisters pc ->
            { z80 | flags = flagRegisters, env = z80_env, interrupts = z80delta.interrupts } |> CoreWithPC pc

        CpuTimeWithSpAndPc sp pc ->
            { z80 | env = { z80_env | sp = sp }, interrupts = z80delta.interrupts } |> CoreWithPC pc

        InterruptsWithCpuTime interruptRegisters ->
            { z80 | env = z80_env, interrupts = interruptRegisters } |> CoreOnly

        FlagsWithPCMainAndCpuTime flagRegisters pc mainWithIndexRegisters ->
            { z80 | flags = flagRegisters, env = z80_env, main = mainWithIndexRegisters, interrupts = z80delta.interrupts } |> CoreWithPC pc

        FlagsWithPcEnvAndCpuTime flagRegisters pc z80Env ->
            { z80 | flags = flagRegisters, env = z80Env, interrupts = z80delta.interrupts } |> CoreWithPC pc

        UnknownIntValue string int ->
            debugTodo string (int |> toHexString2) z80 |> CoreOnly

        --MainRegsWithCpuTime mainWithIndexRegisters cpuTimeCTime ->
        --    { z80 | pc = z80delta.pc, env = z80_env, clockTime = cpuTimeCTime, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }
        --NewAValue int ->
        --    let
        --        flags =
        --            z80.flags
        --    in
        --    { z80 | pc = z80delta.pc, flags = { flags | a = int }, env = z80_env, clockTime = z80delta.time, interrupts = z80delta.interrupts }
        NoOp ->
            { z80 | env = z80_env, interrupts = z80delta.interrupts } |> CoreOnly
