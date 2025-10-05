module Z80Mem exposing (..)

import Bitwise
import CpuTimeCTime exposing (CTime(..), CpuTimeAnd16BitValue, CpuTimeAndValue, CpuTimeCTime, CpuTimePcAnd16BitValue, CpuTimeSpAnd16BitValue, addCpuTimeTime, cont)
import MemoryAddress exposing (MemoryAddress(..))
import Utils exposing (shiftLeftBy8)
import Z80Core exposing (Z80Core)
import Z80Env exposing (Z80Env, getRamMemoryValue)
import Z80Rom exposing (Z80ROM, getROMValue)



--public final int m1(int addr, int ir) {
--	int n = cpu.time - ctime;
--	if(n>0) cont(n);
--
--	addr -= 0x4000;
--	if((addr&0xC000) == 0)
--		cont1(0);
--	ctime = NOCONT;
--	if((ir&0xC000) == 0x4000)
--		ctime = cpu.time + 4;
--	if(addr >= 0)
--		return ram[addr];
--	n = rom[addr+=0x4000];
--	if(if1rom!=null && (addr&0xE8F7)==0) {
--		if(addr==0x0008 || addr==0x1708) {
--			if(rom==rom48k) rom = if1rom;
--		} else if(addr==0x0700) {
--			if(rom==if1rom) rom = rom48k;
--		}
--	}
--	return n;
--}


m1 : Int -> Int -> Z80ROM -> CpuTimeCTime -> Z80Env -> CpuTimeAndValue
m1 addr ir rom48k clockTime z80env =
    let
        z80env_time =
            case clockTime.ctime of
                NoCont ->
                    clockTime

                ContUntil until ->
                    let
                        n =
                            clockTime.cpu_time - until
                    in
                    if n > 0 then
                        clockTime |> cont n

                    else
                        clockTime

        --ramAddr =
        --    addr - 0x4000
        memAddress =
            addr |> MemoryAddress.fromInt

        --z80env_1_time =
        --    if Bitwise.and ramAddr 0xC000 == 0 then
        --        z80env_time |> cont1 0
        --
        --    else
        --        z80env_time
        --value =
        --    if ramAddr >= 0 then
        --        z80env |> getRamValue ramAddr rom48k
        --
        --    else
        --        -- not implementing IF1 switching for now
        --        rom48k |> getROMValue addr
        ( value, z80env_1_time ) =
            case memAddress of
                ROM romaddr ->
                    ( rom48k |> getROMValue romaddr, z80env_time )

                RAM ramaddress ->
                    z80env |> getRamMemoryValue ramaddress z80env_time rom48k

        ctime =
            if Bitwise.and ir 0xC000 == 0x4000 then
                ContUntil (z80env_1_time.cpu_time + 4)

            else
                NoCont
    in
    CpuTimeAndValue { z80env_1_time | ctime = ctime } value



--public final int mem(int addr) {
--	int n = cpu.time - ctime;
--	if(n>0) cont(n);
--	ctime = NOCONT;
--
--	addr -= 0x4000;
--	if(addr>=0) {
--		if(addr<0x4000) {
--			cont1(0);
--			ctime = cpu.time + 3;
--		}
--		return ram[addr];
--	}
--	return rom[addr+0x4000];
--}


mem : Int -> CpuTimeCTime -> Z80ROM -> Z80Env -> CpuTimeAndValue
mem base_addr time rom48k z80env =
    let
        z80env_time =
            case time.ctime of
                NoCont ->
                    time

                ContUntil until ->
                    let
                        n =
                            time.cpu_time - until
                    in
                    if n > 0 then
                        time |> cont n

                    else
                        time

        --addr =
        --    base_addr - 0x4000
        memAddress =
            base_addr |> MemoryAddress.fromInt

        ( value, new_time ) =
            case memAddress of
                ROM romAddr ->
                    ( rom48k |> getROMValue romAddr, { z80env_time | ctime = NoCont } )

                RAM ramaddress ->
                    z80env |> getRamMemoryValue ramaddress z80env_time rom48k
    in
    CpuTimeAndValue new_time value



--	public final int mem16(int addr) {
--		int n = cpu.time - ctime;
--		if(n>0) cont(n);
--		ctime = NOCONT;
--
--		int addr1 = addr-0x3FFF;
--		if((addr1&0x3FFF)!=0) {
--			if(addr1<0)
--				return rom[addr] | rom[addr1+0x4000]<<8;
--			if(addr1<0x4000) {
--				cont1(0); cont1(3);
--				ctime = cpu.time + 6;
--			}
--			return ram[addr-0x4000] | ram[addr1]<<8;
--		}
--		switch(addr1>>>14) {
--		case 0:
--			cont1(3);
--			ctime = cpu.time + 6;
--			return rom[addr] | ram[0]<<8;
--		case 1:
--			cont1(0);
--		case 2:
--			return ram[addr-0x4000] | ram[addr1]<<8;
--		default:
--			return ram[0xBFFF] | rom[0]<<8;
--		}
--	}


mem16 : Int -> Z80ROM -> CpuTimeCTime -> Z80Env -> CpuTimeAnd16BitValue
mem16 addr rom48k clockTime z80env =
    let
        z80env_time =
            case clockTime.ctime of
                NoCont ->
                    clockTime

                ContUntil until ->
                    let
                        n =
                            clockTime.cpu_time - until
                    in
                    if n > 0 then
                        clockTime |> cont n

                    else
                        clockTime

        --addr1 =
        --    addr - 0x3FFF
        ( memAddress, memAddr1 ) =
            addr |> MemoryAddress.fromInt16
    in
    let
        ( lowValue, lowTime ) =
            case memAddress of
                ROM romAddress ->
                    ( getROMValue romAddress rom48k, z80env_time )

                RAM ramAddress ->
                    z80env |> getRamMemoryValue ramAddress z80env_time rom48k

        ( highValue, highTime ) =
            case memAddr1 of
                ROM romAddress ->
                    ( getROMValue romAddress rom48k, { lowTime | ctime = NoCont } )

                RAM ramAddress ->
                    z80env |> getRamMemoryValue ramAddress lowTime rom48k
    in
    CpuTimeAnd16BitValue highTime (Bitwise.or lowValue (shiftLeftBy8 highValue))


z80_pop : Z80ROM -> CpuTimeCTime -> Z80Env -> CpuTimeSpAnd16BitValue
z80_pop z80rom clockTime z80_env =
    let
        v =
            z80_env |> mem16 z80_env.sp z80rom clockTime

        time =
            v.time |> addCpuTimeTime 6
    in
    CpuTimeSpAnd16BitValue time (Bitwise.and (z80_env.sp + 2) 0xFFFF) v.value16


imm16 : Z80ROM -> CpuTimeCTime -> Int -> Z80Core -> CpuTimePcAnd16BitValue
imm16 rom48k clockTime pc_in z80 =
    let
        v =
            z80.env |> mem16 pc_in rom48k clockTime

        pc =
            Bitwise.and (pc_in + 2) 0xFFFF

        env =
            v.time |> addCpuTimeTime 6
    in
    CpuTimePcAnd16BitValue env pc v.value16
