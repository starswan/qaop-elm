module Compiler exposing (..)

import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..), addDuration, reset_cpu_time)
import Dict exposing (Dict)
import Maybe.Extra exposing (oneOf)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import Set exposing (Set)
import SimpleFlagOps exposing (singleByteFlags)
import SimpleSingleByte exposing (singleByteMainRegs)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters)
import SingleNoParams exposing (applyNoParamsDelta, singleWithNoParam)
import SingleWith8BitParameter exposing (JumpChange(..), maybeRelativeJump, singleWith8BitParam)
import TripleByte exposing (tripleByteWith16BitParam)
import TripleWithFlags exposing (TripleWithFlagsChange(..), triple16bitJumps)
import Z80Core exposing (Z80Core)
import Z80Env exposing (Z80Env, m1, mem, mem16)
import Z80Execute exposing (applyFlagDelta, applyJumpChangeDelta, applyPureDelta, applyRegisterDelta, applySimple8BitDelta, applyTripleChangeDelta, applyTripleFlagChange)
import Z80Rom exposing (Z80ROM)


lengthAndDuration : Int -> Z80ROM -> Z80Env -> Maybe ( PCIncrement, InstructionDuration, InstructionDuration -> Z80Core -> Z80Core )
lengthAndDuration pc rom48k z80env =
    z80env
        |> m1 pc 0 rom48k reset_cpu_time
        |> .value
        |> oneOf
            [ \instruction ->
                singleWithNoParam
                    |> Dict.get instruction
                    |> Maybe.map
                        (\( noParamChange, duration ) ->
                            ( IncrementByOne, duration, \dur z80core -> z80core |> applyNoParamsDelta (z80core.clockTime |> addDuration dur) noParamChange rom48k )
                        )
            , \instruction ->
                tripleByteWith16BitParam
                    |> Dict.get instruction
                    |> Maybe.map
                        (\( f, duration ) ->
                            let
                                doubleParam =
                                    z80env |> mem16 (Bitwise.and (pc + 1) 0xFFFF) rom48k reset_cpu_time
                            in
                            ( IncrementByThree
                            , duration
                            , \dur z80core ->
                                z80core |> applyTripleChangeDelta rom48k TripleIncrementByThree (z80core.clockTime |> addDuration dur) (f doubleParam.value16)
                            )
                        )
            , \instruction ->
                singleByteMainAndFlagRegisters
                    |> Dict.get instruction
                    |> Maybe.map
                        (\( f, duration ) ->
                            ( IncrementByOne, duration, \dur z80core -> z80core |> applyPureDelta IncrementByOne (z80core.clockTime |> addDuration dur) (f z80core.main z80core.flags) )
                        )
            , \instruction ->
                singleByteMainRegs
                    |> Dict.get instruction
                    |> Maybe.map
                        (\( mainRegFunc, duration ) ->
                            ( IncrementByOne
                            , duration
                            , \dur z80core ->
                                z80core |> applyRegisterDelta IncrementByOne dur (mainRegFunc z80core.main) rom48k
                            )
                        )
            , \instruction ->
                singleWith8BitParam
                    |> Dict.get instruction
                    |> Maybe.map
                        (\( f, duration ) ->
                            let
                                param =
                                    z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) reset_cpu_time rom48k
                            in
                            ( IncrementByTwo, duration, \dur z80core -> z80core |> applySimple8BitDelta IncreaseByTwo (z80core.clockTime |> addDuration dur) (f param.value) rom48k )
                        )
            , \instruction ->
                singleByteFlags
                    |> Dict.get instruction
                    |> Maybe.map
                        (\( flagFunc, duration ) ->
                            ( IncrementByOne, duration, \dur z80core -> z80core |> applyFlagDelta IncrementByOne dur (flagFunc z80core.flags) rom48k )
                        )

            -- These 2 not supported by tests (yet)
            , \instruction ->
                triple16bitJumps
                    |> Dict.get instruction
                    |> Maybe.map
                        (\( f, duration ) ->
                            let
                                doubleParam =
                                    z80env |> mem16 (Bitwise.and (pc + 1) 0xFFFF) rom48k reset_cpu_time
                            in
                            ( IncrementByThree, duration, \dur z80core -> z80core |> applyTripleFlagChange (z80core.clockTime |> addDuration dur) (f doubleParam.value16) )
                        )
            , \instruction ->
                maybeRelativeJump
                    |> Dict.get instruction
                    |> Maybe.map
                        (\( f, duration ) ->
                            let
                                param =
                                    z80env |> mem (Bitwise.and (pc + 1) 0xFFFF) reset_cpu_time rom48k
                            in
                            ( IncrementByTwo, duration, \dur z80core -> z80core |> applyJumpChangeDelta (z80core.clockTime |> addDuration dur) (f param.value pc) )
                        )
            ]


type CompilerState
    = Running
    | Skipping Int
    | JumpTo Int
    | Stopping


type alias CompileRunning =
    { seen : Set Int

    -- Dict of instruction to function with time and size
    , compiled : Dict Int (Z80Core -> Z80Core)
    , state : CompilerState
    }


compileRunning : Z80ROM -> Z80Env -> Int -> Int -> CompileRunning -> CompileRunning
compileRunning rom48k z80env key value input =
    case input.state of
        Stopping ->
            input

        Skipping int ->
            let
                state =
                    if int > 1 then
                        Skipping (int - 1)

                    else
                        Running
            in
            { input | state = state, seen = input.seen |> Set.insert key }

        JumpTo int ->
            if key < int then
                input

            else
                { input | state = Running }

        Running ->
            let
                clockTime =
                    reset_cpu_time

                running =
                    if [ 0xC9, 0xE9 ] |> List.member value then
                        -- C9 == RET , E9 = JP (HL)
                        { input | state = Stopping }

                    else
                        let
                            param =
                                z80env |> mem (Bitwise.and (key + 1) 0xFFFF) clockTime rom48k

                            maybeJump =
                                maybeRelativeJump |> Dict.get value |> Maybe.map Tuple.first |> Maybe.map (\f -> f param.value key)
                        in
                        case maybeJump of
                            Just relJump ->
                                let
                                    address =
                                        case relJump of
                                            ActualJump addr ->
                                                addr

                                            ConditionalJump int _ _ ->
                                                int

                                            DJNZ int _ ->
                                                int
                                in
                                if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                    { input | state = Stopping }

                                else
                                    case relJump of
                                        ActualJump addr ->
                                            { input | state = JumpTo address }

                                        ConditionalJump int delay f ->
                                            rom48k.rom48k |> Dict.foldl (compileRunning rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo address }

                                        DJNZ int delay ->
                                            rom48k.rom48k |> Dict.foldl (compileRunning rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo address }

                            Nothing ->
                                let
                                    jumpAddress =
                                        z80env |> mem16 (key + 1) rom48k clockTime |> .value16

                                    maybeLongJump =
                                        triple16bitJumps |> Dict.get value |> Maybe.map Tuple.first |> Maybe.map (\f -> f jumpAddress)
                                in
                                case maybeLongJump of
                                    Just aLongJump ->
                                        --let
                                        --    longAddr =
                                        --        case aLongJump of
                                        --            Conditional16BitJump int function ->
                                        --                int
                                        --
                                        --            Conditional16BitCall int shortDelay function ->
                                        --                int
                                        --
                                        --            CallImmediate int ->
                                        --                int
                                        --
                                        --            NewPCRegister int ->
                                        --                int
                                        --in
                                        if (input.seen |> Set.member key) || (input.compiled |> Dict.member key) then
                                            { input | state = Stopping }

                                        else
                                            case aLongJump of
                                                Conditional16BitJump int function ->
                                                    rom48k.rom48k |> Dict.foldl (compileRunning rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo jumpAddress }

                                                Conditional16BitCall int shortDelay function ->
                                                    rom48k.rom48k |> Dict.foldl (compileRunning rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo jumpAddress }

                                                CallImmediate int ->
                                                    rom48k.rom48k |> Dict.foldl (compileRunning rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo jumpAddress }

                                                NewPCRegister int ->
                                                    { input | state = JumpTo jumpAddress }

                                    --else if jumpAddress > key then
                                    --case aLongJump of
                                    --    Conditional16BitJump int function ->
                                    --        rom48k.rom48k |> Dict.foldl (compileRunning rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo jumpAddress }
                                    --
                                    --    Conditional16BitCall int shortDelay function ->
                                    --        rom48k.rom48k |> Dict.foldl (compileRunning rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo jumpAddress }
                                    --
                                    --    CallImmediate int ->
                                    --        rom48k.rom48k |> Dict.foldl (compileRunning rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo jumpAddress }
                                    --
                                    --    NewPCRegister int ->
                                    --        if jumpAddress > key then
                                    --            rom48k.rom48k |> Dict.foldl (compileRunning rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo jumpAddress }
                                    --
                                    --        else
                                    --            { input | state = Stopping }
                                    --else
                                    --    rom48k.rom48k |> Dict.foldl (compileRunning rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo jumpAddress }
                                    -- all conditional jumps are recursed
                                    -- all calls are recursed.
                                    Nothing ->
                                        input
            in
            case lengthAndDuration value rom48k z80env of
                Just ( length, duration, f ) ->
                    let
                        state =
                            case length of
                                IncrementByOne ->
                                    Running

                                IncrementByTwo ->
                                    Skipping 1

                                IncrementByThree ->
                                    Skipping 2

                                IncrementByFour ->
                                    Skipping 3
                    in
                    { running | compiled = running.compiled |> Dict.insert key (f duration), state = state }

                Nothing ->
                    { running | seen = running.seen |> Set.insert key }


compileRom : Z80ROM -> Z80Env -> Dict Int (Z80Core -> Z80Core)
compileRom rom48k z80env =
    let
        y =
            rom48k.rom48k
                |> Dict.foldl (compileRunning rom48k z80env) { seen = Set.empty, compiled = Dict.empty, state = Running }
    in
    y.compiled
