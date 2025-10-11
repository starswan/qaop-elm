module Compiler exposing (..)

import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..), addDuration, reset_cpu_time)
import Dict exposing (Dict)
import Maybe.Extra as MaybeExtra exposing (combine, oneOf)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import Set exposing (Set)
import SimpleSingleByte exposing (singleByteMainRegs)
import SingleMainWithFlags exposing (singleByteMainAndFlagRegisters)
import SingleNoParams exposing (applyNoParamsDelta, singleWithNoParam)
import SingleWith8BitParameter exposing (singleWith8BitParam)
import TripleByte exposing (tripleByteWith16BitParam)
import Utils exposing (shiftLeftBy8)
import Z80Core exposing (Z80Core)
import Z80Env exposing (Z80Env, m1, mem, mem16)
import Z80Execute exposing (applyPureDelta, applyRegisterDelta, applySimple8BitDelta, applyTripleChangeDelta)
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
                                z80core |> applyRegisterDelta IncrementByOne (z80core.clockTime |> addDuration dur) (mainRegFunc z80core.main) rom48k
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
                            ( IncrementByTwo, duration, \dur z80core -> z80core |> applySimple8BitDelta IncreaseByTwo (z80core.clockTime |> addDuration dur) (f param.value) )
                        )
            ]


type alias CompileRunning =
    { seen : Set Int

    -- Dict of instruction to function with time and size
    , compiled : Dict Int (Z80Core -> Z80Core)
    , skip : Int
    , stop : Bool
    }


compileRunning : Z80ROM -> Z80Env -> Int -> Int -> CompileRunning -> CompileRunning
compileRunning rom48k z80env key value input =
    if (input.seen |> Set.member key) || (input.compiled |> Dict.member key) || input.stop then
        input

    else if input.skip > 0 then
        { input | skip = input.skip - 1, seen = input.seen |> Set.insert key }

    else
        let
            running =
                if [ 0xC9, 0xE9 ] |> List.member value then
                    -- C9 == RET , E9 = JP (HL)
                    { input | stop = True }

                else if value == 0x18 then
                    -- 18 == JR
                    rom48k.rom48k
                        |> Dict.get (key + 1)
                        |> Maybe.map
                            (\jr ->
                                if jr < 0x80 then
                                    { input | skip = 1 + jr }

                                else
                                    { input | stop = True }
                            )
                        |> Maybe.withDefault input

                else if value == 0xC3 then
                    -- C3 == JP
                    rom48k.rom48k
                        |> Dict.get (key + 1)
                        |> Maybe.map
                            (\jp_low ->
                                rom48k.rom48k
                                    |> Dict.get (key + 2)
                                    |> Maybe.map (\jp_high -> (jp_low |> shiftLeftBy8) + jp_high)
                                    |> Maybe.map
                                        (\addr ->
                                            if addr > key then
                                                { input | skip = addr - key }

                                            else
                                                { input | stop = True }
                                        )
                            )
                        -- convert Maybe Maybe y into Maybe y
                        |> MaybeExtra.join
                        |> Maybe.withDefault input
                    -- all conditional jumps are recursed
                    -- all calls are recursed.
                    --else if [ 0x18, 0xC3, 0xCD, 0xC7, 0xCF, 0xD7, 0xDF, 0xE7, 0xEF, 0xF7, 0xFF ] |> List.member value then
                    --    -- 0x18 JR, 0xC3 JP, 0xCD CALL, 0xC7 - 0xFF RST 00 - RST 38
                    --    let
                    --        call =
                    --            rom48k.rom48k |> Dict.filter (\k v -> k > target)
                    --                |> Dict.foldl (compileRunning rom48k z80env) { seen = input.seen, compiled = input.compiled, skip = 0, stop = False }
                    --    in
                    --    { input | seen = call.seen, compiled = call.compiled }

                else
                    input
        in
        case lengthAndDuration value rom48k z80env of
            Just ( length, duration, f ) ->
                let
                    skip =
                        case length of
                            IncrementByOne ->
                                0

                            IncrementByTwo ->
                                1

                            IncrementByThree ->
                                2

                            IncrementByFour ->
                                3
                in
                { running | compiled = running.compiled |> Dict.insert key (f duration), skip = running.skp + skip }

            Nothing ->
                { running | seen = running.seen |> Set.insert key }


compileRom : Z80ROM -> Z80Env -> Dict Int (Z80Core -> Z80Core)
compileRom rom48k z80env =
    let
        y =
            rom48k.rom48k
                |> Dict.foldl (compileRunning rom48k z80env) { seen = Set.empty, compiled = Dict.empty, skip = 0, stop = False }
    in
    y.compiled
