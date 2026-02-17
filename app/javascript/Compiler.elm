module Compiler exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..), reset_cpu_time)
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..), TriplePCIncrement(..))
import Set exposing (Set)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegistersIY)
import SingleNoParams exposing (RstChange(..), singleNoParamCalls)
import SingleWith8BitParameter exposing (JumpChange(..), maybeRelativeJump)
import TripleWithFlags exposing (TripleWithFlagsChange(..), triple16bitJumps)
import TripleWithMain exposing (tripleMainRegsIY)
import Utils exposing (toHexString, toHexString2, toPlainHexString2)
import Z80Core exposing (CoreChange, Z80Core)
import Z80Debug exposing (debugLog, debugTodo)
import Z80Env exposing (Z80Env)
import Z80Execute exposing (applyJumpChangeDelta, applyTripleFlagChange)
import Z80Mem exposing (mem, mem16)
import Z80OpCode exposing (lengthAndDuration)
import Z80Rom exposing (Z80ROM, subName)


type CompilerState
    = Running
    | Skipping Int
    | SkipUntil38Sentinel
    | JumpTo Int
    | Stopping


type alias CompileRunning =
    { seen : Set Int

    -- Dict of instruction to function with time and size
    , compiled : Dict Int ( CpuTimeCTime -> Z80ROM -> Z80Core -> CoreChange, InstructionDuration, PCIncrement )
    , state : CompilerState
    }


compilerInit : CompileRunning
compilerInit =
    { seen = Set.empty, compiled = Dict.empty, state = Running }


compileRunning : Int -> Z80ROM -> Z80Env -> Int -> Int -> CompileRunning -> CompileRunning
compileRunning nesting rom48k z80env key value input =
    case input.state of
        Stopping ->
            input

        SkipUntil38Sentinel ->
            if value == 0x38 then
                { input | state = Running }

            else
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

                    else if [ 0xCF, 0xC9, 0xE9 ] |> List.member value then
                        -- C9 == RET , E9 = JP (HL)
                        -- RST08(0xCF) and RST28(0xEF) take an inline parameter
                        -- RST08 doesn't return via the stack, RST28 returns after a sentinel 0x38
                        { input | state = Stopping }

                    else if value == 0xEF then
                        -- RST28 returns after a sentinel 0x38
                        { input | state = SkipUntil38Sentinel }

                    else
                        let
                            param =
                                z80env |> mem (Bitwise.and (key + 1) 0xFFFF) clockTime rom48k

                            maybeJump =
                                maybeRelativeJump |> Dict.get value |> Maybe.map (\( f, duration ) -> ( f param.value, duration ))
                        in
                        case maybeJump of
                            Just ( relJump, duration ) ->
                                let
                                    dictVal =
                                        ( \_ _ z80core -> z80core |> applyJumpChangeDelta relJump, duration, IncrementByTwo )
                                in
                                case relJump of
                                    ActualJumpOffset offset ->
                                        let
                                            address =
                                                key + offset
                                        in
                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                            { input | state = Stopping }

                                        else
                                            { input | state = JumpTo address, compiled = input.compiled |> Dict.insert key dictVal }

                                    ConditionalJumpOffset offset _ _ ->
                                        let
                                            address =
                                                key + offset
                                        in
                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                            input

                                        else
                                            let
                                                state =
                                                    input.state

                                                newState =
                                                    { seen = input.seen, compiled = input.compiled |> Dict.insert key dictVal, state = JumpTo address }

                                                x =
                                                    rom48k.rom48k |> Dict.foldl (compileRunning (nesting + 1) rom48k z80env) newState
                                            in
                                            { x | state = state }

                                    DJNZOffset offset _ ->
                                        let
                                            address =
                                                key + offset
                                        in
                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                            input

                                        else
                                            let
                                                state =
                                                    input.state

                                                newState =
                                                    { seen = input.seen, compiled = input.compiled |> Dict.insert key dictVal, state = JumpTo address }

                                                x =
                                                    rom48k.rom48k |> Dict.foldl (compileRunning (nesting + 1) rom48k z80env) newState
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
                                                triple16bitJumps |> Dict.get value |> Maybe.map (\( f, duration ) -> ( f address, duration ))
                                        in
                                        case maybeLongJump of
                                            Just ( aLongJump, duration ) ->
                                                let
                                                    dictVal =
                                                        ( \_ _ z80core -> z80core |> applyTripleFlagChange aLongJump, duration, IncrementByThree )
                                                in
                                                case aLongJump of
                                                    NewPCRegister _ ->
                                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                                            { input | state = Stopping }

                                                        else
                                                            { input | state = JumpTo address, compiled = input.compiled |> Dict.insert key dictVal }

                                                    Conditional16BitJump _ _ ->
                                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                                            input

                                                        else
                                                            let
                                                                state =
                                                                    input.state

                                                                newState =
                                                                    { seen = input.seen, compiled = input.compiled |> Dict.insert key dictVal, state = JumpTo address }

                                                                x =
                                                                    rom48k.rom48k |> Dict.foldl (compileRunning (nesting + 1) rom48k z80env) newState
                                                            in
                                                            { x | state = state }

                                                    Conditional16BitCall _ _ _ ->
                                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                                            input

                                                        else
                                                            let
                                                                state =
                                                                    input.state

                                                                newState =
                                                                    { seen = input.seen, compiled = input.compiled |> Dict.insert key dictVal, state = JumpTo address }

                                                                x =
                                                                    rom48k.rom48k |> Dict.foldl (compileRunning (nesting + 1) rom48k z80env) newState
                                                            in
                                                            { x | state = state }

                                                    CallImmediate _ ->
                                                        if (input.seen |> Set.member address) || (input.compiled |> Dict.member address) then
                                                            input

                                                        else
                                                            let
                                                                state =
                                                                    input.state

                                                                newState =
                                                                    { seen = input.seen, compiled = input.compiled |> Dict.insert key dictVal, state = JumpTo address }

                                                                x =
                                                                    rom48k.rom48k |> Dict.foldl (compileRunning (nesting + 1) rom48k z80env) newState
                                                            in
                                                            { x | state = state }

                                            Nothing ->
                                                input
            in
            case running.state of
                Skipping _ ->
                    running

                SkipUntil38Sentinel ->
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

                                --keyVal =
                                --    debugLog ((nesting |> String.fromInt) ++ " compiled " ++ (key |> subName)) (value |> toHexString2) key
                            in
                            { running | compiled = running.compiled |> Dict.insert key ( f, duration, length ), state = state }

                        Nothing ->
                            -- single byte unavailable (e.g. DI/EI etc) 33 INC SP, 3B DEC SP or  - EX AF, AF' 0x76 = HALT D9 = EXX
                            if [ 0xF3, 0xFB, 0xD9, 0x33, 0x3B, 0x08, 0x76 ] |> List.member value then
                                --let
                                --    keyVal =
                                --        debugLog ((nesting |> String.fromInt) ++ " DI/EI INC SP etc " ++ (key |> subName)) (value |> toHexString2) key
                                --in
                                { running | seen = running.seen |> Set.insert key }

                            else if [ 0xCB ] |> List.member value then
                                -- 2 byte unavailable
                                let
                                    nextValue =
                                        z80env |> mem (Bitwise.and (key + 1) 0xFFFF) clockTime rom48k |> .value

                                    keyVal =
                                        debugLog ((nesting |> String.fromInt) ++ " CB 2 bytes " ++ (key |> subName)) (nextValue |> toHexString2) key
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
                                        debugLog ((nesting |> String.fromInt) ++ " ED " ++ (key |> subName) ++ " " ++ (1 + skipCount |> String.fromInt) ++ " bytes") (nextValue |> toHexString2) key
                                in
                                -- ED - mostly 2 bytes, but occasionally 3
                                { running | seen = running.seen |> Set.insert keyVal, state = Skipping skipCount }

                            else if [ 0xFD, 0xDD ] |> List.member value then
                                let
                                    nextValue =
                                        z80env |> mem (Bitwise.and (key + 1) 0xFFFF) clockTime rom48k |> .value
                                in
                                if nextValue == 0xCB then
                                    -- FDCB always 4 bytes
                                    let
                                        fdcbparam =
                                            z80env |> mem16 (key + 2) rom48k clockTime |> .value16

                                        keyVal =
                                            debugLog ((nesting |> String.fromInt) ++ " IXCB 4 " ++ (key |> subName) ++ " " ++ (value |> toHexString2) ++ "CB") (fdcbparam |> toHexString) key
                                    in
                                    { running | seen = running.seen |> Set.insert keyVal, state = Skipping 3 }

                                else if nextValue == 0xE9 then
                                    -- JP (IX) or JP (IY)
                                    let
                                        keyVal =
                                            debugLog ((nesting |> String.fromInt) ++ " JP (IX/IY) " ++ (key |> subName)) (value |> toHexString2) key
                                    in
                                    { running | seen = running.seen |> Set.insert keyVal, state = Stopping }

                                else if singleByteMainAndFlagRegistersIY |> Dict.member nextValue then
                                    -- 2 byte e.g FD09
                                    let
                                        keyVal =
                                            debugLog ((nesting |> String.fromInt) ++ " Addr I " ++ (key |> subName) ++ " 2 byte DD/FD") (value |> toHexString2) key
                                    in
                                    { running | seen = running.seen |> Set.insert keyVal, state = Skipping 1 }

                                else
                                    case tripleMainRegsIY |> Dict.get nextValue of
                                        Just ( _, inc, _ ) ->
                                            let
                                                --keyVal =
                                                --    debugLog ((nesting |> String.fromInt) ++ " Addr J " ++ (key |> subName)) (nextValue |> toHexString2) key
                                                length =
                                                    case inc of
                                                        TripleIncrementByThree ->
                                                            2

                                                        TripleIncrementByFour ->
                                                            3
                                            in
                                            { running | seen = running.seen |> Set.insert key, state = Skipping length }

                                        Nothing ->
                                            debugTodo "no length" ((key |> toHexString) ++ " " ++ (value |> toPlainHexString2) ++ (nextValue |> toPlainHexString2)) { running | seen = running.seen |> Set.insert key }

                            else
                                debugTodo "no length" ((key |> toHexString) ++ " " ++ (value |> toHexString2)) { running | seen = running.seen |> Set.insert key }


entryPoints =
    -- Compiled : "5452 Seen 3101"
    [ 0, 0x10, 0x20, 0x38, 0x02BF ]
        ++ [ 0x1CF0, 0x1D03, 0x1DAB, 0x1DED, 0x1E27, 0x1E42, 0x1E4F, 0x1E5F, 0x1E67, 0x1E7A, 0x1E80, 0x1EA1 ]
        ++ [ 0x1EAC, 0x1EED, 0x1F23, 0x1F3A, 0x1F60, 0x1FC9, 0x2089, 0x2294, 0x22DC, 0x2320, 0x2382, 0x2C02 ]
        ++ [ 0x25B3, 0x25E8, 0x268D, 0x25AF, 0x25F5, 0x25F8, 0x2627, 0x2634, 0x268D, 0x2668, 0x2672 ]
        ++ [ 0x368F, 0x343C, 0x33A1, 0x300F, 0x30CA, 0x31AF, 0x3851, 0x351B, 0x3524, 0x353B ]
        ++ [ 0x37DA, 0x3833, 0x3843, 0x37E2, 0x3713, 0x36C4, 0x36AF, 0x384A, 0x3492, 0x346A ]
        ++ [ 0x353B, 0x3014, 0x352D, 0x353B, 0x359C, 0x35DE, 0x34BC, 0x3645, 0x346E, 0x3669, 0x3674, 0x37B5, 0x37AA ]


compileRom : Z80ROM -> Z80Env -> Dict Int ( CpuTimeCTime -> Z80ROM -> Z80Core -> CoreChange, InstructionDuration, PCIncrement )
compileRom rom48k z80env =
    let
        result =
            entryPoints
                |> List.foldl
                    (\jump accum ->
                        rom48k.rom48k |> Dict.foldl (compileRunning 0 rom48k z80env) { accum | state = JumpTo jump }
                    )
                    compilerInit

        compiledResult =
            debugLog "Compiled " ((result.compiled |> Dict.size |> String.fromInt) ++ " Seen " ++ (result.seen |> Set.size |> String.fromInt)) result.compiled
    in
    compiledResult
