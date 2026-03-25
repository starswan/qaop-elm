module WholeZ80Ops exposing (..)

import Bitwise exposing (shiftRightBy)
import CpuTimeCTime exposing (c_TIME_LIMIT)
import Z80CoreWithClockTime exposing (Z80, add_cpu_time)


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
