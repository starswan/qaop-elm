module SingleNoParams exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..))
import Dict exposing (Dict)
import Z80Core exposing (Z80, Z80Core, set_iff)
import Z80Env exposing (z80_pop, z80_push)
import Z80Flags exposing (set_af)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (set_bc_main, set_de_main)


type NoParamChange
    = NoOp
    | DisableInterrupts
    | EnableInterrupts
    | PopBC
    | PopDE
    | PopHL
    | PopIX
    | PopIY
    | PopAF
      --| Exx
      --| ExAfAfDash
    | Rst00
    | Rst08
    | Rst10
    | Rst18
    | Rst20
    | Rst28
    | Rst30
    | Rst38
    | Ret


singleWithNoParam : Dict Int ( NoParamChange, InstructionDuration )
singleWithNoParam =
    Dict.fromList
        [ ( 0x00, ( NoOp, FourTStates ) )

        --, ( 0x08, ( ExAfAfDash, FourTStates ) )
        -- case 0x40: break;
        , ( 0x40, ( NoOp, FourTStates ) )

        -- case 0x49: break;
        , ( 0x49, ( NoOp, FourTStates ) )

        -- case 0x52: break;
        , ( 0x52, ( NoOp, FourTStates ) )

        -- case 0x5B: break;
        , ( 0x5B, ( NoOp, FourTStates ) )

        -- case 0x64: break;
        , ( 0x64, ( NoOp, FourTStates ) )

        -- case 0x6D: break;
        , ( 0x6D, ( NoOp, FourTStates ) )

        -- case 0x7F: break;
        , ( 0x7F, ( NoOp, FourTStates ) )
        , ( 0xC1, ( PopBC, TenTStates ) )
        , ( 0xC7, ( Rst00, FourTStates ) )
        , ( 0xC9, ( Ret, TenTStates ) )
        , ( 0xCF, ( Rst08, ElevenTStates ) )
        , ( 0xD1, ( PopDE, TenTStates ) )
        , ( 0xD7, ( Rst10, ElevenTStates ) )

        -- case 0xD9: exx(); break;
        --, ( 0xD9, ( Exx, FourTStates ) )
        , ( 0xDF, ( Rst18, ElevenTStates ) )
        , ( 0xE1, ( PopHL, TenTStates ) )
        , ( 0xE7, ( Rst20, ElevenTStates ) )
        , ( 0xEF, ( Rst28, ElevenTStates ) )
        , ( 0xF1, ( PopAF, TenTStates ) )
        , ( 0xF3, ( DisableInterrupts, FourTStates ) )
        , ( 0xF7, ( Rst30, ElevenTStates ) )
        , ( 0xFB, ( EnableInterrupts, FourTStates ) )
        , ( 0xFF, ( Rst38, ElevenTStates ) )
        ]


singleWithNoParamDD : Dict Int ( NoParamChange, InstructionDuration )
singleWithNoParamDD =
    Dict.fromList
        [ ( 0xE1, ( PopIX, FourteenTStates ) )
        ]


singleWithNoParamFD : Dict Int ( NoParamChange, InstructionDuration )
singleWithNoParamFD =
    Dict.fromList
        [ ( 0xE1, ( PopIY, FourteenTStates ) )
        ]


applyNoParamsDelta : CpuTimeCTime -> NoParamChange -> Z80ROM -> Z80Core -> Z80Core
applyNoParamsDelta cpu_time z80changeData rom48k z80 =
    let
        old_env =
            z80.env
    in
    case z80changeData of
        NoOp ->
            { z80
                | pc = Bitwise.and (z80.pc + 1) 0xFFFF
                , env = { old_env | time = cpu_time }
                , r = z80.r + 1
            }

        PopBC ->
            let
                env1 =
                    { old_env | time = cpu_time }

                v =
                    env1 |> z80_pop rom48k

                --env = z80.env
                --z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
                --x = debug_log "pop_bc" (v.value |> toHexString) Nothing
            in
            { z80
                | pc = Bitwise.and (z80.pc + 1) 0xFFFF
                , main = z80.main |> set_bc_main v.value16
                , env = { env1 | time = v.time, sp = v.sp }
                , r = z80.r + 1
            }

        PopHL ->
            let
                env1 =
                    { old_env | time = cpu_time }

                v =
                    env1 |> z80_pop rom48k

                main =
                    z80.main
            in
            { z80
                | pc = Bitwise.and (z80.pc + 1) 0xFFFF
                , main = { main | hl = v.value16 }
                , env = { env1 | time = v.time, sp = v.sp }
                , r = z80.r + 1
            }

        PopIX ->
            let
                env1 =
                    { old_env | time = cpu_time }

                v =
                    env1 |> z80_pop rom48k

                main =
                    z80.main
            in
            { z80
                | pc = Bitwise.and (z80.pc + 2) 0xFFFF
                , main = { main | ix = v.value16 }
                , env = { env1 | time = v.time, sp = v.sp }
                , r = z80.r + 1
            }

        PopIY ->
            let
                env1 =
                    { old_env | time = cpu_time }

                v =
                    env1 |> z80_pop rom48k

                main =
                    z80.main
            in
            { z80
                | pc = Bitwise.and (z80.pc + 2) 0xFFFF
                , main = { main | iy = v.value16 }
                , env = { env1 | time = v.time, sp = v.sp }
                , r = z80.r + 1
            }

        DisableInterrupts ->
            -- case 0xF3: IFF=0; break;
            let
                ints =
                    z80 |> set_iff 0
            in
            { z80
                | pc = Bitwise.and (z80.pc + 1) 0xFFFF
                , interrupts = ints
                , r = z80.r + 1
            }

        EnableInterrupts ->
            -- case 0xFB: IFF=3; break;
            let
                ints =
                    z80 |> set_iff 3
            in
            { z80
                | pc = Bitwise.and (z80.pc + 1) 0xFFFF
                , interrupts = ints
                , r = z80.r + 1
            }

        --ExAfAfDash ->
        --    -- case 0x08: ex_af(); break;
        --    let
        --        new_z80 =
        --            z80 |> ex_af
        --    in
        --    { new_z80
        --        | pc = pc
        --        , r = z80.r + 1
        --    }
        --Exx ->
        --    -- case 0xD9: exx(); break;
        --    let
        --        main =
        --            z80.main
        --
        --        alt =
        --            z80.alt_main
        --    in
        --    { z80
        --        | pc = pc
        --        , r = z80.r + 1
        --        , main = { main | b = alt.b, c = alt.c, d = alt.d, e = alt.e, hl = alt.hl }
        --        , alt_main = { alt | b = main.b, c = main.c, d = main.d, e = main.e, hl = main.hl }
        --    }
        PopAF ->
            -- case 0xF1: af(pop()); break;
            let
                env1 =
                    { old_env | time = cpu_time }

                v =
                    env1 |> z80_pop rom48k
            in
            { z80
                | pc = Bitwise.and (z80.pc + 1) 0xFFFF
                , flags = set_af v.value16
                , env = { env1 | time = v.time, sp = v.sp }
                , r = z80.r + 1
            }

        PopDE ->
            -- case 0xD1: v=pop(); D=v>>>8; E=v&0xFF; break;
            let
                env1 =
                    { old_env | time = cpu_time }

                v =
                    env1 |> z80_pop rom48k
            in
            { z80
                | pc = Bitwise.and (z80.pc + 1) 0xFFFF
                , main = z80.main |> set_de_main v.value16
                , env = { env1 | time = v.time, sp = v.sp }
                , r = z80.r + 1
            }

        Rst00 ->
            --case 0xC7:push(PC); PC=c-199; break;
            z80 |> rst 0xC7 cpu_time

        Rst08 ->
            --case 0xCF:push(PC); PC=c-199; break;
            z80 |> rst 0xCF cpu_time

        Rst10 ->
            --case 0xD7:push(PC); PC=c-199; break;
            z80 |> rst 0xD7 cpu_time

        Rst18 ->
            --case 0xDF:push(PC); PC=c-199; break;
            z80 |> rst 0xDF cpu_time

        Rst20 ->
            --case 0xE7:push(PC); PC=c-199; break;
            z80 |> rst 0xE7 cpu_time

        Rst28 ->
            --case 0xEF:push(PC); PC=c-199; break;
            z80 |> rst 0xEF cpu_time

        Rst30 ->
            --case 0xF7:push(PC); PC=c-199; break;
            z80 |> rst 0xF7 cpu_time

        Rst38 ->
            --case 0xFF:push(PC); PC=c-199; break;
            z80 |> rst 0xFF cpu_time

        Ret ->
            --ret : Z80ROM -> Z80 -> Z80Delta
            --ret rom48k z80 =
            -- case 0xC9: MP=PC=pop(); break;
            let
                a =
                    z80.env |> z80_pop rom48k

                --b = debug_log "ret" (a.value |> subName) Nothing
                --env = z80.env
                --    CpuTimeWithSpAndPc a.time a.sp a.value
            in
            { z80
                | env = { old_env | time = a.time, sp = a.sp }
                , pc = a.value16
                , r = z80.r + 1
            }


rst : Int -> CpuTimeCTime -> Z80Core -> Z80Core
rst value cpu_time z80 =
    let
        --interrupts =
        --    z80.interrupts
        old_env =
            z80.env

        pc =
            Bitwise.and (z80.pc + 1) 0xFFFF
    in
    { z80
        | pc = value - 199
        , env = { old_env | time = cpu_time } |> z80_push pc
        , r = z80.r + 1
    }


ex_af : Z80 -> Z80
ex_af z80 =
    -- called by Spectrum loader
    let
        core =
            z80.core

        core_1 =
            { core | flags = z80.alt_flags }
    in
    { z80 | core = core_1, alt_flags = core.flags }


exx : Z80 -> Z80
exx z80 =
    -- case 0xD9: exx(); break;
    let
        core =
            z80.core

        main =
            core.main

        alt =
            z80.alt_main

        new_core =
            { core | main = { main | b = alt.b, c = alt.c, d = alt.d, e = alt.e, hl = alt.hl } }
    in
    { z80 | core = new_core, alt_main = { alt | b = main.b, c = main.c, d = main.d, e = main.e, hl = main.hl } }
