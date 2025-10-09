module Compiler exposing (..)

import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..), reset_cpu_time)
import Dict exposing (Dict)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import Set exposing (Set)
import SingleEnvWithMain exposing (singleEnvMainRegs)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegistersIY)
import SingleNoParams exposing (RstChange(..), singleNoParamCalls)
import SingleWith8BitParameter exposing (JumpChange(..), maybeRelativeJump)
import TripleWithFlags exposing (TripleWithFlagsChange(..), triple16bitJumps)
import TripleWithMain exposing (tripleMainRegsIY)
import Utils exposing (toHexString, toHexString2, toPlainHexString2)
import Z80Core exposing (Z80Core)
import Z80Debug exposing (debugLog, debugTodo)
import Z80Env exposing (Z80Env)
import Z80Mem exposing (mem, mem16)
import Z80OpCode exposing (lengthAndDuration)
import Z80Rom exposing (Z80ROM, subName)


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


compileRunning : Int -> Z80ROM -> Z80Env -> Int -> Int -> CompileRunning -> CompileRunning
compileRunning nesting rom48k z80env key value input =
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
            if key < int - 1 then
                input

            else
                { input | state = Running }

        Running ->
            let
                clockTime =
                    reset_cpu_time

                running =
                    if (input.seen |> Set.member key) || (input.compiled |> Dict.member key) then
                        { input | state = Stopping }

                    else if [ 0xC9, 0xE9 ] |> List.member value then
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
                                case relJump of
                                    ActualJump address ->
                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                            { input | state = Stopping }

                                        else
                                            { input | state = JumpTo address }

                                    ConditionalJump address _ _ ->
                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                            input

                                        else
                                            let
                                                state =
                                                    input.state

                                                x =
                                                    rom48k.rom48k |> Dict.foldl (compileRunning (nesting + 1) rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo address }
                                            in
                                            { x | state = state }

                                    DJNZ address _ ->
                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                            input

                                        else
                                            let
                                                state =
                                                    input.state

                                                x =
                                                    rom48k.rom48k |> Dict.foldl (compileRunning (nesting + 1) rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo address }
                                            in
                                            { x | state = state }

                            Nothing ->
                                case singleNoParamCalls |> Dict.get value of
                                    Just ( f, _ ) ->
                                        case f of
                                            Rst rstValue ->
                                                if (input.seen |> Set.member rstValue) || (input.compiled |> Dict.member rstValue) then
                                                    input

                                                else
                                                    let
                                                        state =
                                                            input.state

                                                        x =
                                                            rom48k.rom48k |> Dict.foldl (compileRunning (nesting + 1) rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo rstValue }
                                                    in
                                                    { x | state = state }

                                    Nothing ->
                                        let
                                            address =
                                                z80env |> mem16 (key + 1) rom48k clockTime |> .value16

                                            maybeLongJump =
                                                triple16bitJumps |> Dict.get value |> Maybe.map Tuple.first |> Maybe.map (\f -> f address)
                                        in
                                        case maybeLongJump of
                                            Just aLongJump ->
                                                case aLongJump of
                                                    NewPCRegister _ ->
                                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                                            { input | state = Stopping }

                                                        else
                                                            { input | state = JumpTo address }

                                                    Conditional16BitJump _ _ ->
                                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                                            input

                                                        else
                                                            let
                                                                state =
                                                                    input.state

                                                                x =
                                                                    rom48k.rom48k |> Dict.foldl (compileRunning (nesting + 1) rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo address }
                                                            in
                                                            { x | state = state }

                                                    Conditional16BitCall _ _ _ ->
                                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                                            input

                                                        else
                                                            let
                                                                state =
                                                                    input.state

                                                                x =
                                                                    rom48k.rom48k |> Dict.foldl (compileRunning (nesting + 1) rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo address }
                                                            in
                                                            { x | state = state }

                                                    CallImmediate _ ->
                                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                                            input

                                                        else
                                                            let
                                                                state =
                                                                    input.state

                                                                x =
                                                                    rom48k.rom48k |> Dict.foldl (compileRunning (nesting + 1) rom48k z80env) { seen = input.seen, compiled = input.compiled, state = JumpTo address }
                                                            in
                                                            { x | state = state }

                                            Nothing ->
                                                input
            in
            case running.state of
                Skipping _ ->
                    running

                JumpTo _ ->
                    running

                Stopping ->
                    running

                Running ->
                    case lengthAndDuration key rom48k z80env of
                        Just ( length, duration, f ) ->
                            let
                                state =
                                    case length of
                                        IncrementByOne ->
                                            -- RST08 takes an inline parameter and doesn't return via the stack.
                                            if key == 0xCF then
                                                Stopping

                                            else
                                                Running

                                        IncrementByTwo ->
                                            Skipping 1

                                        IncrementByThree ->
                                            Skipping 2

                                        IncrementByFour ->
                                            Skipping 3

                                keyVal =
                                    debugLog ((nesting |> String.fromInt) ++ " Addr " ++ (key |> subName)) (value |> toHexString2) key
                            in
                            { running | compiled = running.compiled |> Dict.insert keyVal (f duration), state = state }

                        Nothing ->
                            -- single byte unavailable (e.g. DI/EI etc) 33 INC SP, 3B DEC SP or singleMain main e.g. LD A, (BC) 0x08 - EX AF, AF' 0x76 = HALT
                            if ([ 0xF3, 0xFB, 0xD9, 0x33, 0x3B, 0x08, 0x76 ] |> List.member value) || (singleEnvMainRegs |> Dict.member value) then
                                let
                                    keyVal =
                                        debugLog ((nesting |> String.fromInt) ++ " Addr " ++ (key |> subName) ++ " DI/EI mains") (value |> toHexString2) key
                                in
                                { running | seen = running.seen |> Set.insert keyVal }

                            else if [ 0xCB ] |> List.member value then
                                -- 2 byte unavailable
                                let
                                    keyVal =
                                        debugLog ((nesting |> String.fromInt) ++ " Addr " ++ (key |> subName) ++ " CB 2 bytes") (value |> toHexString2) key
                                in
                                { running | seen = running.seen |> Set.insert keyVal, state = Skipping 1 }

                            else if value == 0xED then
                                let
                                    nextValue =
                                        z80env |> mem (Bitwise.and (key + 1) 0xFFFF) clockTime rom48k |> .value

                                    skipCount =
                                        if [ 0x43, 0x53, 0x63, 0x73, 0x4B, 0x5B, 0x6B, 0x7B ] |> List.member nextValue then
                                            2

                                        else
                                            1

                                    keyVal =
                                        debugLog ((nesting |> String.fromInt) ++ " Addr " ++ (key |> subName) ++ " ED " ++ (1 + skipCount |> String.fromInt) ++ " bytes") (value |> toHexString2) key
                                in
                                -- ED - mostly 2 bytes, but occasionally 3
                                { running | seen = running.seen |> Set.insert keyVal, state = Skipping skipCount }

                            else if [ 0xFD, 0xDD ] |> List.member value then
                                let
                                    nextValue =
                                        z80env |> mem (Bitwise.and (key + 1) 0xFFFF) clockTime rom48k |> .value
                                in
                                if [ 0xCB ] |> List.member nextValue then
                                    -- FDCB always 4 bytes
                                    let
                                        keyVal =
                                            debugLog ((nesting |> String.fromInt) ++ " Addr " ++ (key |> subName) ++ " FDCB 4") (value |> toHexString2) key
                                    in
                                    { running | seen = running.seen |> Set.insert keyVal, state = Skipping 3 }

                                else if [ 0xE9 ] |> List.member nextValue then
                                    -- JP (IX) or JP (IY)
                                    let
                                        keyVal =
                                            debugLog ((nesting |> String.fromInt) ++ " Addr " ++ (key |> subName) ++ " JP (IX/IY)") (value |> toHexString2) key
                                    in
                                    { running | seen = running.seen |> Set.insert keyVal, state = Stopping }

                                else if [ 0x21, 0x2A ] |> List.member nextValue then
                                    -- 3 byte unavailable
                                    let
                                        keyVal =
                                            debugLog ((nesting |> String.fromInt) ++ " Addr " ++ (key |> subName)) (value |> toHexString2) key
                                    in
                                    { running | seen = running.seen |> Set.insert keyVal, state = Skipping 2 }

                                else if singleByteMainAndFlagRegistersIY |> Dict.member nextValue then
                                    -- 2 byte e.g FD09
                                    let
                                        keyVal =
                                            debugLog ((nesting |> String.fromInt) ++ " Addr " ++ (key |> subName) ++ " 2 byte DD/FD") (value |> toHexString2) key
                                    in
                                    { running | seen = running.seen |> Set.insert keyVal, state = Skipping 1 }

                                else
                                    case tripleMainRegsIY |> Dict.get nextValue of
                                        Just ( _, inc, _ ) ->
                                            let
                                                keyVal =
                                                    debugLog ((nesting |> String.fromInt) ++ " Addr " ++ (key |> subName)) (value |> toHexString2) key

                                                length =
                                                    case inc of
                                                        TripleIncrementByThree ->
                                                            2

                                                        TripleIncrementByFour ->
                                                            3
                                            in
                                            { running | seen = running.seen |> Set.insert keyVal, state = Skipping length }

                                        Nothing ->
                                            debugTodo "no length" ((key |> toHexString) ++ " " ++ (value |> toPlainHexString2) ++ (nextValue |> toPlainHexString2)) { running | seen = running.seen |> Set.insert key }

                            else
                                debugTodo "no length" ((key |> toHexString) ++ " " ++ (value |> toHexString2)) { running | seen = running.seen |> Set.insert key }


compileRom : Z80ROM -> Z80Env -> Dict Int (Z80Core -> Z80Core)
compileRom rom48k z80env =
    let
        y =
            rom48k.rom48k
                |> Dict.foldl (compileRunning 0 rom48k z80env) { seen = Set.empty, compiled = Dict.empty, state = Running }

        z =
            debugLog "Sizes" ((y.compiled |> Dict.size |> String.fromInt) ++ " " ++ (y.seen |> Set.size |> String.fromInt)) (y.compiled |> Dict.size) + (y.seen |> Set.size)
    in
    ( y.compiled, z ) |> Tuple.first
