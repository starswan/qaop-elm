--
-- $Id$
--


module Z80Env exposing (..)

import Bitwise exposing (and, or, shiftRightBy)
import CpuTimeCTime exposing (CpuTimeAndValue, CpuTimeCTime, CpuTimeIncrement, CpuTimePcAndValue, CpuTimeSpAndValue, addCpuTimeTime, addCpuTimeTimeInc, c_NOCONT, cont, cont1, cont_port)
import Keyboard exposing (Keyboard, z80_keyboard_input)
import Utils exposing (shiftLeftBy8, shiftRightBy8, toHexString2)
import Z80Address exposing (Z80Address, decrement, decrement2, fromInt, incrementBy2, toInt)
import Z80Debug exposing (debugLog)
import Z80Ram exposing (Z80Ram, getRamValue)
import Z80Rom exposing (Z80ROM, getROMValue)


c_FRSTART =
    -14335


c_FRTIME =
    69888


c_TIME_LIMIT =
    c_FRSTART + c_FRTIME



-- changing this to an array results in a recursion error in the browser :-(


type alias Z80Env =
    { --rom48k : Z80ROM
      ram : Z80Ram
    , keyboard : Keyboard
    , time : CpuTimeCTime
    , sp : Z80Address
    }


type alias Z80EnvWithValue =
    { env : Z80Env
    , value : Int
    }


type alias Z80EnvWithPC =
    { env : Z80Env
    , pc : Z80Address
    }


type alias ValueWithTime =
    { value : Int
    , cpu_time : Int
    }


z80env_constructor =
    Z80Env Z80Ram.constructor Keyboard.constructor (CpuTimeCTime c_FRSTART 0) (0 |> fromInt)



--set_rom : Array Int -> Z80Env -> Z80Env
--set_rom romdata z80env =
--    let
--        rommy =
--            make_spectrum_rom romdata
--    in
--    { z80env | rom48k = rommy }
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


m1 : Int -> Int -> Z80ROM -> Z80Env -> CpuTimeAndValue
m1 addr ir rom48k z80env =
    let
        n =
            z80env.time.cpu_time - z80env.time.ctime

        z80env_time =
            if n > 0 then
                z80env.time |> cont n

            else
                z80env.time

        ramAddr =
            addr - 0x4000

        z80env_1_time =
            if Bitwise.and ramAddr 0xC000 == 0 then
                z80env_time |> cont1 0

            else
                z80env_time

        ctime =
            if Bitwise.and ir 0xC000 == 0x4000 then
                z80env_1_time.cpu_time + 4

            else
                c_NOCONT

        value =
            if ramAddr >= 0 then
                z80env.ram |> getRamValue ramAddr

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


mem : Int -> CpuTimeCTime -> Z80ROM -> Z80Ram -> CpuTimeAndValue
mem base_addr time rom48k ram =
    let
        n =
            time.cpu_time - time.ctime

        z80env_time =
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
                        new_z80 =
                            z80env_time |> cont1 0
                    in
                    ( new_z80, new_z80.cpu_time + 3, ram |> getRamValue addr )

                else
                    ( z80env_time, c_NOCONT, ram |> getRamValue addr )

            else
                ( z80env_time, c_NOCONT, rom48k |> getROMValue base_addr )
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


mem16 : Int -> Z80ROM -> Z80Env -> CpuTimeAndValue
mem16 addr rom48k z80env =
    let
        n =
            z80env.time.cpu_time - z80env.time.ctime

        z80env_time =
            if n > 0 then
                z80env.time |> cont n

            else
                z80env.time

        addr1 =
            addr - 0x3FFF
    in
    if and addr1 0x3FFF /= 0 then
        if addr1 < 0 then
            let
                low =
                    getROMValue addr rom48k

                high =
                    getROMValue (addr1 + 0x4000) rom48k
            in
            CpuTimeAndValue { z80env_time | ctime = c_NOCONT } (Bitwise.or low (shiftLeftBy8 high))

        else
            let
                low =
                    getRamValue (addr - 0x4000) z80env.ram

                high =
                    getRamValue addr1 z80env.ram

                z80env_1_time =
                    if addr1 < 0x4000 then
                        z80env_time |> cont1 0 |> cont1 3 |> addCpuTimeTime 6

                    else
                        { z80env_time | ctime = c_NOCONT }
            in
            CpuTimeAndValue z80env_1_time (Bitwise.or low (shiftLeftBy8 high))

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
                    getRamValue 0 z80env.ram
            in
            CpuTimeAndValue new_z80_time (or low (shiftLeftBy8 high))

        else if addr1shift14 == 1 then
            let
                new_env_time =
                    z80env_time |> cont1 0

                low =
                    getRamValue (addr - 0x4000) z80env.ram

                high =
                    getRamValue addr1 z80env.ram
            in
            CpuTimeAndValue new_env_time (or low (shiftLeftBy8 high))

        else if addr1shift14 == 2 then
            let
                low =
                    getRamValue (addr - 0x4000) z80env.ram

                high =
                    getRamValue addr1 z80env.ram
            in
            CpuTimeAndValue { z80env_time | ctime = c_NOCONT } (or low (shiftLeftBy8 high))

        else
            let
                low =
                    z80env.ram |> getRamValue 0xBFFF

                high =
                    rom48k |> getROMValue 0
            in
            CpuTimeAndValue { z80env_time | ctime = c_NOCONT } (or low (shiftLeftBy8 high))



--
--public final void mem(int addr, int v) {
--	int n = cpu.time - ctime;
--	if(n>0) cont(n);
--	ctime = NOCONT;
--
--	addr -= 0x4000;
--	if(addr < 0x4000) {
--		if(addr < 0) return;
--		cont1(0);
--		ctime = cpu.time + 3;
--		if(ram[addr]==v)
--			return;
--		if(addr<6912)
--			refresh_screen();
--	}
--	ram[addr] = v;
--}


setRam : Int -> Int -> Z80Env -> Z80Env
setRam addr value z80env =
    --let
    --ram_value = getValue addr z80env.ram
    --n = if addr == 0x1CB6 || addr == 0x1CB7 then
    --       debug_log "Alert!" ("setting " ++ (addr |> toHexString) ++ " from " ++ (ram_value |> toHexString2) ++ " to " ++ (value |> toHexString2)) Nothing
    --    else
    --       Nothing
    --in
    { z80env | ram = z80env.ram |> Z80Ram.setRamValue addr value }


setMem : Int -> Int -> Z80Env -> Z80Env
setMem z80_addr value z80env =
    let
        n =
            z80env.time.cpu_time - z80env.time.ctime

        z80env_time =
            if n > 0 then
                z80env.time |> cont n

            else
                CpuTimeCTime z80env.time.cpu_time c_NOCONT

        addr =
            z80_addr - 0x4000

        ( new_env, ctime ) =
            if addr < 0x4000 then
                if addr < 0 then
                    ( z80env, c_NOCONT )

                else
                    let
                        z80env_1_time =
                            z80env_time |> cont1 0

                        new_time =
                            z80env_1_time.cpu_time + 3

                        ram_value =
                            getRamValue addr z80env.ram
                    in
                    if ram_value == value then
                        ( z80env, new_time )

                    else
                        ( z80env |> setRam addr value, new_time )

            else
                ( z80env |> setRam addr value, c_NOCONT )
    in
    { new_env | time = CpuTimeCTime z80env_time.cpu_time ctime }



--public final void mem16(int addr, int v) {
--
--	int addr1 = addr-0x3FFF;
--	if((addr1&0x3FFF)!=0) {
--		int n = cpu.time - ctime;
--		if(n>0) cont(n);
--		ctime = NOCONT;
--
--		if(addr1<0) return;
--		if(addr1>=0x4000) {
--			ram[addr1-1] = v&0xFF;
--			ram[addr1] = v>>>8;
--			return;
--		}
--	}
--	mem(addr, v&0xFF);
--	cpu.time += 3;
--	mem((char)(addr+1), v>>>8);
--	cpu.time -= 3;
--}


setMem16 : Int -> Int -> Z80Env -> Z80Env
setMem16 addr value z80env =
    let
        addr1 =
            addr - 0x3FFF
    in
    if Bitwise.and addr1 0x3FFF /= 0 then
        let
            n =
                z80env.time.cpu_time - z80env.time.ctime

            z80env_time =
                if n > 0 then
                    cont n z80env.time

                else
                    z80env.time

            env_1 =
                { z80env | time = CpuTimeCTime z80env_time.cpu_time c_NOCONT }
        in
        if addr1 < 0 then
            env_1

        else if addr1 >= 0x4000 then
            env_1
                |> setRam (addr1 - 1) (Bitwise.and value 0xFF)
                |> setRam addr1 (shiftRightBy8 value)

        else
            env_1
                |> setMem addr (Bitwise.and value 0xFF)
                |> setMem (addr + 1) (shiftRightBy8 value)

    else
        z80env
            |> setMem addr (Bitwise.and value 0xFF)
            |> setMem (addr + 1) (shiftRightBy8 value)


contPortEnv : Int -> Z80Env -> Z80Env
contPortEnv portn z80env =
    { z80env | time = z80env.time |> cont_port portn }



--	public void out(int port, int v)
--	{
--		cont_port(port);
--
--		if((port&0x0001)==0) {
--			ula28 = (byte)v;
--			int n = v&7;
--			if(n != border) {
--				refresh_border();
--				border = (byte)n;
--			}
--			n = sp_volt[v>>3 & 3];
--			if(n != speaker) {
--				au_update();
--				speaker = n;
--			}
--		}
--		if((port&0x8002)==0x8000 && ay_enabled) {
--			if((port&0x4000)!=0)
--				ay_idx = (byte)(v&15);
--			else {
--				au_update();
--				ay_write(ay_idx, v);
--			}
--		}
--	}


out : Int -> Int -> Z80Env -> Z80Env
out portnum _ env_in =
    let
        env =
            env_in |> contPortEnv portnum
    in
    env


z80_in : Int -> Z80Env -> CpuTimeAndValue
z80_in portnum env_in =
    let
        env =
            env_in.time |> cont_port portnum

        --x = debug_log "z80_in" (portnum |> toHexString) Nothing
        value =
            env_in.keyboard |> z80_keyboard_input portnum

        x =
            if value /= 0xFF then
                debugLog "keyboard value" ((portnum |> toHexString2) ++ " " ++ toHexString2 value) value

            else
                value
    in
    CpuTimeAndValue env x


addCpuTimeEnv : Int -> Z80Env -> Z80Env
addCpuTimeEnv value z80env =
    { z80env | time = z80env.time |> addCpuTimeTime value }


addCpuTimeEnvInc : CpuTimeIncrement -> Z80Env -> Z80Env
addCpuTimeEnvInc value z80env =
    { z80env | time = z80env.time |> addCpuTimeTimeInc value }


reset_cpu_time : Z80Env -> Z80Env
reset_cpu_time z80env =
    { z80env | time = CpuTimeCTime c_FRSTART z80env.time.ctime }



--public void push(int v) {
--	int sp;
--	time++;
--	env.mem((char)((sp=SP)-1), v>>>8);
--	time += 3;
--	env.mem(SP = (char)(sp-2), v&0xFF);
--	time += 3;
--}


z80_push : Z80Address -> Z80Env -> Z80Env
z80_push v z80env =
    let
        --a = debug_log "push" ((v |> toHexString) ++ " onto " ++ (z80.sp |> toHexString)) Nothing
        sp_minus_1 =
            --Bitwise.and (z80env.sp - 1) 0xFFFF
            z80env.sp |> decrement

        new_sp =
            --Bitwise.and (z80env.sp - 2) 0xFFFF
            z80env.sp |> decrement2

        value =
            v |> toInt

        env_2 =
            z80env
                |> addCpuTimeEnv 1
                |> setMem (sp_minus_1 |> toInt) (shiftRightBy8 value)
                --|> setMem (sp_minus_1 |> toInt) (top8Bits v)
                |> addCpuTimeEnv 3
                |> setMem (new_sp |> toInt) (Bitwise.and value 0xFF)
                --|> setMem (new_sp |> toInt) (lower8Bits v)
                |> addCpuTimeEnv 3
    in
    { env_2 | sp = new_sp }


z80_pop : Z80ROM -> Z80Env -> CpuTimeSpAndValue
z80_pop z80rom z80_env =
    let
        v =
            z80_env |> mem16 (z80_env.sp |> toInt) z80rom

        time =
            v.time |> addCpuTimeTime 6
    in
    --CpuTimeSpAndValue time (Bitwise.and (z80_env.sp + 2) 0xFFFF) v.value
    CpuTimeSpAndValue time (z80_env.sp |> incrementBy2) (v.value |> fromInt)
