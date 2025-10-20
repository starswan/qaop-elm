module Z80Mem exposing (..)

import Bitwise exposing (shiftRightBy)
import CpuTimeCTime exposing (CTime(..), CpuTimeAnd16BitValue, CpuTimeAndValue, CpuTimeCTime, CpuTimePcAnd16BitValue, CpuTimeSpAnd16BitValue, addCpuTimeTime, cont, cont1)
import Dict
import Utils exposing (shiftLeftBy8)
import Z80Core exposing (Z80Core)
import Z80Env exposing (Z80Env)
import Z80Ram exposing (getRamValue)
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

        ramAddr =
            addr - 0x4000

        z80env_1_time =
            if Bitwise.and ramAddr 0xC000 == 0 then
                z80env_time |> cont1 0

            else
                z80env_time

        ctime =
            if Bitwise.and ir 0xC000 == 0x4000 then
                ContUntil (z80env_1_time.cpu_time + 4)

            else
                NoCont

        value =
            if ramAddr >= 0 then
                z80env |> getRamValue ramAddr rom48k

            else
                -- not implementing IF1 switching for now
                rom48k |> getROMValue addr
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
mem base_addr time rom48k ram =
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

        addr =
            base_addr - 0x4000

        ( new_time, ctime, value ) =
            if addr >= 0 then
                if addr < 0x4000 then
                    let
                        new_z80_time =
                            z80env_time |> cont1 0
                    in
                    ( new_z80_time, ContUntil (new_z80_time.cpu_time + 3), ram |> getRamValue addr rom48k )

                else
                    ( z80env_time, NoCont, ram |> getRamValue addr rom48k )

            else
                ( z80env_time, NoCont, rom48k |> getROMValue base_addr )
    in
    CpuTimeAndValue { new_time | ctime = ctime } value



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

        addr1 =
            addr - 0x3FFF
    in
    if Bitwise.and addr1 0x3FFF /= 0 then
        if addr1 < 0 then
            let
                low =
                    getROMValue addr rom48k

                high =
                    getROMValue (addr1 + 0x4000) rom48k
            in
            CpuTimeAnd16BitValue { z80env_time | ctime = NoCont } (Bitwise.or low (shiftLeftBy8 high))

        else
            let
                low =
                    z80env |> getRamValue (addr - 0x4000) rom48k

                high =
                    z80env |> getRamValue addr1 rom48k

                z80env_1_time =
                    if addr1 < 0x4000 then
                        z80env_time |> cont1 0 |> cont1 3 |> addCpuTimeTime 6

                    else
                        { z80env_time | ctime = NoCont }
            in
            CpuTimeAnd16BitValue z80env_1_time (Bitwise.or low (shiftLeftBy8 high))

    else
        let
            addr1shift14 =
                shiftRightBy 14 addr1
        in
        if addr1shift14 == 0 then
            let
                new_z80_time =
                    cont1 3 z80env_time

                low =
                    rom48k |> getROMValue addr

                high =
                    z80env |> getRamValue 0 rom48k
            in
            CpuTimeAnd16BitValue new_z80_time (Bitwise.or low (shiftLeftBy8 high))

        else if addr1shift14 == 1 then
            let
                new_env_time =
                    z80env_time |> cont1 0

                low =
                    z80env |> getRamValue (addr - 0x4000) rom48k

                high =
                    z80env |> getRamValue addr1 rom48k
            in
            CpuTimeAnd16BitValue new_env_time (Bitwise.or low (shiftLeftBy8 high))

        else if addr1shift14 == 2 then
            let
                low =
                    z80env |> getRamValue (addr - 0x4000) rom48k

                high =
                    z80env |> getRamValue addr1 rom48k
            in
            CpuTimeAnd16BitValue { z80env_time | ctime = NoCont } (Bitwise.or low (shiftLeftBy8 high))

        else
            let
                low =
                    z80env |> getRamValue 0xBFFF rom48k

                high =
                    rom48k |> getROMValue 0
            in
            CpuTimeAnd16BitValue { z80env_time | ctime = NoCont } (Bitwise.or low (shiftLeftBy8 high))


z80_pop : Z80ROM -> CpuTimeCTime -> Z80Env -> CpuTimeSpAnd16BitValue
z80_pop z80rom clockTime z80_env =
    let
        v =
            z80_env |> mem16 z80_env.sp z80rom clockTime

        time =
            v.time |> addCpuTimeTime 6
    in
    CpuTimeSpAnd16BitValue time (Bitwise.and (z80_env.sp + 2) 0xFFFF) v.value16


getRamValue : Int -> Z80ROM -> Z80Env -> Int
getRamValue addr z80rom z80env =
    case z80env.ram |> Dict.get addr of
        Just a ->
            a

        Nothing ->
            z80rom.z80ram |> Z80Ram.getRamValue addr


imm16 : Z80ROM -> CpuTimeCTime -> Z80Core -> CpuTimePcAnd16BitValue
imm16 rom48k clockTime z80 =
    let
        v =
            z80.env |> mem16 z80.pc rom48k clockTime

        pc =
            Bitwise.and (z80.pc + 2) 0xFFFF

        env =
            v.time |> addCpuTimeTime 6
    in
    CpuTimePcAnd16BitValue env pc v.value16
