module GroupCB exposing (..)

import Bitwise exposing (complement, shiftLeftBy, shiftRightBy)
import CpuTimeCTime exposing (CpuTimePcAnd16BitValue, CpuTimePcAndValue, addCpuTimeTime)
import Utils exposing (byte, char)
import Z80Core exposing (Z80Core, inc_pc2, set408bitHL)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (addCpuTimeEnv, mem, setMem)
import Z80Flags exposing (IntWithFlags, bit, c_F53, shifter)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL(..), IntWithFlagsTimeAndPC, get_ixiy_xy)



--
--	private void group_xy_cb(int xy)
--	{
--		int pc = PC;
--		int a = MP = (char)(xy + (byte)env.mem(pc));
--		time += 3;
--		int c = env.mem((char)(pc+1));
--		PC = (char)(pc+2);
--		time += 5;
--		int v = env.mem(a);
--		time += 4;
--		int o = c>>>3 & 7;


group_xy_cb : IXIY -> Z80ROM -> Z80Core -> Z80Delta
group_xy_cb ixiyhl rom48k z80 =
    let
        xy =
            get_ixiy_xy ixiyhl z80.main

        offset =
            mem z80.pc z80.env.time rom48k z80.env.ram

        addr =
            char (xy + byte offset.value)

        env_1 =
            z80.env

        c =
            mem (char (z80.pc + 1)) (offset.time |> addCpuTimeTime 3) rom48k z80.env.ram

        new_pc =
            z80 |> inc_pc2

        v1 =
            mem addr (c.time |> addCpuTimeTime 5) rom48k z80.env.ram

        z80_3 =
            { z80 | pc = new_pc, env = { env_1 | time = v1.time |> addCpuTimeTime 4 } }

        o =
            Bitwise.and (shiftRightBy 3 c.value) 7

        cAndC0 =
            Bitwise.and c.value 0xC0

        --		switch(c&0xC0) {
        --			case 0x00: v = shifter(o, v); break;
        --			case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
        --			case 0x80: v &= ~(1<<o); break;
        --			case 0xC0: v |= 1<<o; break;
        --		}
        v2 =
            case cAndC0 of
                0x00 ->
                    shifter o v1.value z80_3.flags

                0x40 ->
                    let
                        flags =
                            bit o v1.value z80_3.flags
                    in
                    IntWithFlags v1.value { flags | ff = Bitwise.or (Bitwise.and flags.ff (complement c_F53)) (shiftRightBy (Bitwise.and 8 c_F53) addr) }

                0x80 ->
                    IntWithFlags (Bitwise.and v1.value (complement (shiftLeftBy o 1))) z80_3.flags

                _ ->
                    IntWithFlags (Bitwise.or v1.value (shiftLeftBy o 1)) z80_3.flags

        new_env =
            if cAndC0 == 0x40 then
                z80_3.env

            else
                z80_3.env |> setMem addr v2.value |> addCpuTimeEnv 3

        --y = debug_log "xy_cb2" ((z80.pc |> toHexString) ++ " c " ++ (c.value |> toHexString2) ++
        --                                                   " set " ++ (a |> toHexString) ++
        --                                                   " from " ++ (v1.value |> toHexString2) ++
        --                                                   " to " ++ (v2.value |> toHexString2)) new_env
        --		env.mem(a, v);
        --		time += 3;
        z80_4 =
            { z80_3 | flags = v2.flags, env = new_env }

        --		switch(c&0x07) {
        --			case 0: B = v; break;
        --			case 1: C = v; break;
        --			case 2: D = v; break;
        --			case 3: E = v; break;
        --			case 4: HL = HL&0x00FF | v<<8; break;
        --			case 5: HL = HL&0xFF00 | v; break;
        --			case 7: A = v; break;
        --		}
        caseval =
            Bitwise.and c.value 0x07
    in
    if (caseval /= 6) && (cAndC0 /= 0x40) then
        let
            ( main, flags, env ) =
                set408bitHL caseval v2.value ( z80.main, z80.flags, z80.env )

            --z80_4 |> set408bitHL caseval v2.value |> WholeCore
        in
        { z80_4 | main = main, flags = flags, env = env } |> WholeCore

    else
        z80_4 |> WholeCore



---- There appear to be many situations where we already know that we don't need all
---- this complexity as we're just doing LD A,B or something similar - so stop using it in those cases
--
--
--load408bitHL : Int -> Z80ROM -> Z80Core -> CpuTimePcAndValue
--load408bitHL c_value rom48k z80 =
--    case Bitwise.and c_value 0x07 of
--        0 ->
--            CpuTimePcAndValue z80.env.time z80.pc z80.main.b
--
--        1 ->
--            CpuTimePcAndValue z80.env.time z80.pc z80.main.c
--
--        2 ->
--            CpuTimePcAndValue z80.env.time z80.pc z80.main.d
--
--        3 ->
--            CpuTimePcAndValue z80.env.time z80.pc z80.main.e
--
--        4 ->
--            CpuTimePcAndValue z80.env.time z80.pc (shiftRightBy8 z80.main.hl)
--
--        5 ->
--            CpuTimePcAndValue z80.env.time z80.pc (Bitwise.and z80.main.hl 0xFF)
--
--        6 ->
--            let
--                new_b =
--                    mem z80.main.hl z80.env.time rom48k z80.env.ram
--            in
--            CpuTimePcAndValue new_b.time z80.pc new_b.value
--
--        _ ->
--            a_with_z80 z80
