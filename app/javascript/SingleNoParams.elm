module SingleNoParams exposing (..)

import Bitwise exposing (shiftRightBy)
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..), c_TIME_LIMIT)
import Dict exposing (Dict)
import RegisterChange exposing (RegisterFlagChange(..))
import Z80Core exposing (CoreChange(..), Z80Core)
import Z80CoreWithClockTime exposing (Z80, add_cpu_time)


singleWithNoParam : Dict Int ( RegisterFlagChange, InstructionDuration )
singleWithNoParam =
    Dict.fromList
        [ ( 0x00, ( RegChangeNoOp, FourTStates ) )

        --, ( 0x08, ( ExAfAfDash, FourTStates ) )
        -- case 0x40: break;
        , ( 0x40, ( RegChangeNoOp, FourTStates ) )

        -- case 0x49: break;
        , ( 0x49, ( RegChangeNoOp, FourTStates ) )

        -- case 0x52: break;
        , ( 0x52, ( RegChangeNoOp, FourTStates ) )

        -- case 0x5B: break;
        , ( 0x5B, ( RegChangeNoOp, FourTStates ) )

        -- case 0x64: break;
        , ( 0x64, ( RegChangeNoOp, FourTStates ) )

        -- case 0x6D: break;
        , ( 0x6D, ( RegChangeNoOp, FourTStates ) )

        -- case 0x7F: break;
        , ( 0x7F, ( RegChangeNoOp, FourTStates ) )
        , ( 0xC1, ( PopBC, TenTStates ) )
        , ( 0xC9, ( Ret, TenTStates ) )
        , ( 0xD1, ( PopDE, TenTStates ) )

        -- case 0xD9: exx(); break;
        --, ( 0xD9, ( Exx, FourTStates ) )
        , ( 0xE1, ( PopHL, TenTStates ) )
        , ( 0xF1, ( PopAF, TenTStates ) )

        --, ( 0xFB, ( EnableInterrupts, FourTStates ) )
        ]


singleNoParamCalls : Dict Int ( RegisterFlagChange, InstructionDuration )
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


singleWithNoParamDD : Dict Int ( RegisterFlagChange, InstructionDuration )
singleWithNoParamDD =
    Dict.fromList
        [ ( 0xE1, ( PopIX, FourteenTStates ) )
        ]


singleWithNoParamFD : Dict Int ( RegisterFlagChange, InstructionDuration )
singleWithNoParamFD =
    Dict.fromList
        [ ( 0xE1, ( PopIY, FourteenTStates ) )
        ]


ex_af : Z80 -> Z80
ex_af z80 =
    -- called by Spectrum loader
    let
        clock =
            z80.coreWithClock

        core =
            clock.core
    in
    { z80
        | coreWithClock = { clock | core = { core | flags = z80.alt_flags } }
        , alt_flags = core.flags
    }


exx : Z80 -> Z80
exx z80 =
    -- case 0xD9: exx(); break;
    let
        clock =
            z80.coreWithClock

        core =
            clock.core

        main =
            core.main

        alt =
            z80.alt_main
    in
    { z80
        | coreWithClock = { clock | core = { core | main = { main | b = alt.b, c = alt.c, d = alt.d, e = alt.e, hl = alt.hl } } }
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
        clock =
            z80.coreWithClock

        --z80_core =
        --    clock.core |> debugLog "halt" ""
        z80_core =
            clock.core

        interrupts =
            z80_core.interrupts

        n =
            shiftRightBy 2 (c_TIME_LIMIT - clock.clockTime.cpu_time + 3)

        new_core =
            if n > 0 then
                -- turns out env.halt(n, r) just returns n...?
                { clock | core = { z80_core | interrupts = { interrupts | halted = True, r = interrupts.r + n } } } |> add_cpu_time (4 * n)

            else
                { clock | core = { z80_core | interrupts = { interrupts | halted = True } } }
    in
    { z80 | coreWithClock = new_core }
