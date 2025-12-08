module SingleNoParams exposing (..)

import Bitwise exposing (shiftRightBy)
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..), c_TIME_LIMIT)
import Dict exposing (Dict)
import Z80Core exposing (Z80, Z80Core, add_cpu_time)
import Z80Debug exposing (debugLog)
import Z80Env exposing (z80_pop, z80_push)
import Z80Flags exposing (set_af)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (set_bc_main, set_de_main)


type NoParamChange
    = NoOp
      --| DisableInterrupts
      --| EnableInterrupts
    | PopBC
    | PopDE
    | PopHL
    | PopIX
    | PopIY
    | PopAF
      --| Exx
      --| ExAfAfDash
      --| Rst00
      --| Rst08
      --| Rst10
      --| Rst18
      --| Rst20
      --| Rst28
      --| Rst30
      --| Rst38
    | Ret


type RstChange
    = Rst Int


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
        , ( 0xC9, ( Ret, TenTStates ) )
        , ( 0xD1, ( PopDE, TenTStates ) )

        -- case 0xD9: exx(); break;
        --, ( 0xD9, ( Exx, FourTStates ) )
        , ( 0xE1, ( PopHL, TenTStates ) )
        , ( 0xF1, ( PopAF, TenTStates ) )

        --, ( 0xFB, ( EnableInterrupts, FourTStates ) )
        ]


singleNoParamCalls : Dict Int ( RstChange, InstructionDuration )
singleNoParamCalls =
    Dict.fromList
        [ ( 0xC7, ( Rst 0x00, ElevenTStates ) )
        , ( 0xCF, ( Rst 0x08, ElevenTStates ) )
        , ( 0xD7, ( Rst 0x10, ElevenTStates ) )
        , ( 0xDF, ( Rst 0x18, ElevenTStates ) )
        , ( 0xE7, ( Rst 0x20, ElevenTStates ) )
        , ( 0xEF, ( Rst 0x28, ElevenTStates ) )
        , ( 0xF7, ( Rst 0x30, ElevenTStates ) )
        , ( 0xFF, ( Rst 0x38, ElevenTStates ) )
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
                , clockTime = cpu_time
            }

        PopBC ->
            let
                v =
                    old_env |> z80_pop rom48k cpu_time

                --env = z80.env
                --z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
                --x = debug_log "pop_bc" (v.value |> toHexString) Nothing
            in
            { z80
                | pc = Bitwise.and (z80.pc + 1) 0xFFFF
                , main = z80.main |> set_bc_main v.value16
                , clockTime = v.time
                , env = { old_env | sp = v.sp }
            }

        PopHL ->
            let
                v =
                    old_env |> z80_pop rom48k cpu_time

                main =
                    z80.main
            in
            { z80
                | pc = Bitwise.and (z80.pc + 1) 0xFFFF
                , main = { main | hl = v.value16 }
                , clockTime = v.time
                , env = { old_env | sp = v.sp }
            }

        PopIX ->
            let
                v =
                    old_env |> z80_pop rom48k cpu_time

                main =
                    z80.main
            in
            { z80
                | pc = Bitwise.and (z80.pc + 2) 0xFFFF
                , main = { main | ix = v.value16 }
                , clockTime = v.time
                , env = { old_env | sp = v.sp }
            }

        PopIY ->
            let
                v =
                    old_env |> z80_pop rom48k cpu_time

                main =
                    z80.main
            in
            { z80
                | pc = Bitwise.and (z80.pc + 2) 0xFFFF
                , main = { main | iy = v.value16 }
                , clockTime = v.time
                , env = { old_env | sp = v.sp }
            }

        --DisableInterrupts ->
        --    -- case 0xF3: IFF=0; break;
        --    let
        --        ints =
        --            z80 |> set_iff 0
        --    in
        --    { z80
        --        | pc = Bitwise.and (z80.pc + 1) 0xFFFF
        --        , interrupts = ints
        --    }
        --
        --EnableInterrupts ->
        --    -- case 0xFB: IFF=3; break;
        --    let
        --        ints =
        --            z80 |> set_iff 3
        --    in
        --    { z80
        --        | pc = Bitwise.and (z80.pc + 1) 0xFFFF
        --        , interrupts = ints
        --    }
        PopAF ->
            -- case 0xF1: af(pop()); break;
            let
                v =
                    old_env |> z80_pop rom48k cpu_time
            in
            { z80
                | pc = Bitwise.and (z80.pc + 1) 0xFFFF
                , flags = set_af v.value16
                , clockTime = v.time
                , env = { old_env | sp = v.sp }
            }

        PopDE ->
            -- case 0xD1: v=pop(); D=v>>>8; E=v&0xFF; break;
            let
                v =
                    old_env |> z80_pop rom48k cpu_time
            in
            { z80
                | pc = Bitwise.and (z80.pc + 1) 0xFFFF
                , main = z80.main |> set_de_main v.value16
                , clockTime = v.time
                , env = { old_env | sp = v.sp }
            }

        --Rst00 ->
        --    --case 0xC7:push(PC); PC=c-199; break;
        --    z80 |> rst 0xC7 cpu_time
        --
        --Rst08 ->
        --    --case 0xCF:push(PC); PC=c-199; break;
        --    z80 |> rst 0xCF cpu_time
        --
        --Rst10 ->
        --    --case 0xD7:push(PC); PC=c-199; break;
        --    z80 |> rst 0xD7 cpu_time
        --
        --Rst18 ->
        --    --case 0xDF:push(PC); PC=c-199; break;
        --    z80 |> rst 0xDF cpu_time
        --
        --Rst20 ->
        --    --case 0xE7:push(PC); PC=c-199; break;
        --    z80 |> rst 0xE7 cpu_time
        --
        --Rst28 ->
        --    --case 0xEF:push(PC); PC=c-199; break;
        --    z80 |> rst 0xEF cpu_time
        --
        --Rst30 ->
        --    --case 0xF7:push(PC); PC=c-199; break;
        --    z80 |> rst 0xF7 cpu_time
        --
        --Rst38 ->
        --    --case 0xFF:push(PC); PC=c-199; break;
        --    z80 |> rst 0xFF cpu_time
        Ret ->
            --ret : Z80ROM -> Z80 -> Z80Delta
            --ret rom48k z80 =
            -- case 0xC9: MP=PC=pop(); break;
            let
                a =
                    z80.env |> z80_pop rom48k cpu_time

                --b = debug_log "ret" (a.value |> subName) Nothing
                --env = z80.env
                --    CpuTimeWithSpAndPc a.time a.sp a.value
            in
            { z80
                | env = { old_env | sp = a.sp }
                , clockTime = a.time
                , pc = a.value16
            }


ex_af : Z80 -> Z80
ex_af z80 =
    -- called by Spectrum loader
    let
        core =
            z80.core
    in
    { z80
        | core = { core | flags = z80.alt_flags }
        , alt_flags = core.flags
    }


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
    in
    { z80
        | core = { core | main = { main | b = alt.b, c = alt.c, d = alt.d, e = alt.e, hl = alt.hl } }
        , alt_main = { alt | b = main.b, c = main.c, d = main.d, e = main.e, hl = main.hl }
    }


execute_0x76_halt : Z80 -> Z80
execute_0x76_halt z80 =
    -- case 0x76: halt(); break;    --
    --	private void halt()
    --	{
    --		halted = true;
    --		int n = time_limit-time+3 >> 2;
    --		if(n>0) {
    --			n = env.halt(n, IR|R&0x7F);
    --			R+=n; time+=4*n;
    --		}
    --	}
    let
        z80_core =
            z80.core |> debugLog "halt" ""

        interrupts =
            z80_core.interrupts

        n =
            shiftRightBy 2 (c_TIME_LIMIT - z80_core.clockTime.cpu_time + 3)

        new_core =
            if n > 0 then
                -- turns out env.halt(n, r) just returns n...?
                { z80_core | interrupts = { interrupts | halted = True, r = interrupts.r + n } } |> add_cpu_time (4 * n)

            else
                { z80_core | interrupts = { interrupts | halted = True } }
    in
    { z80 | core = new_core }


rst : Int -> CpuTimeCTime -> Z80Core -> Z80Core
rst new_pc cpu_time z80 =
    let
        old_env =
            z80.env

        pc =
            Bitwise.and (z80.pc + 1) 0xFFFF
    in
    { z80
        | pc = new_pc
        , clockTime = cpu_time
        , env = old_env |> z80_push pc cpu_time
    }


applyRstDelta : CpuTimeCTime -> RstChange -> Z80ROM -> Z80Core -> Z80Core
applyRstDelta cpu_time z80changeData _ z80 =
    case z80changeData of
        Rst int ->
            --case 0xC7:push(PC); PC=c-199; break;
            z80 |> rst int cpu_time



--Rst08 ->
--    --case 0xCF:push(PC); PC=c-199; break;
--    z80 |> rst 0xCF cpu_time
--
--Rst10 ->
--    --case 0xD7:push(PC); PC=c-199; break;
--    z80 |> rst 0xD7 cpu_time
--
--Rst18 ->
--    --case 0xDF:push(PC); PC=c-199; break;
--    z80 |> rst 0xDF cpu_time
--
--Rst20 ->
--    --case 0xE7:push(PC); PC=c-199; break;
--    z80 |> rst 0xE7 cpu_time
--
--Rst28 ->
--    --case 0xEF:push(PC); PC=c-199; break;
--    z80 |> rst 0xEF cpu_time
--
--Rst30 ->
--    --case 0xF7:push(PC); PC=c-199; break;
--    z80 |> rst 0xF7 cpu_time
--
--Rst38 ->
--    --case 0xFF:push(PC); PC=c-199; break;
--    z80 |> rst 0xFF cpu_time
