--
-- $Id$
--


module Z80Env exposing (..)

import Bitwise
import CpuTimeCTime exposing (CTime(..), CpuTimeAnd16BitValue, CpuTimeAndValue, CpuTimeCTime, CpuTimeSpAnd16BitValue, cont, cont1, cont_port)
import Dict exposing (Dict)
import Keyboard exposing (Keyboard, z80_keyboard_input)
import Utils exposing (shiftRightBy8, toHexString)
import Z80Debug exposing (debugLog)
import Z80Ram exposing (Z80Ram, getRamValue)
import Z80Rom exposing (Z80ROM, get16BitROMValue, getROMValue)


c_FRSTART =
    -14335


c_FRTIME =
    69888


c_TIME_LIMIT =
    c_FRSTART + c_FRTIME



-- changing this to an array results in a recursion error in the browser :-(


type alias Z80Env =
    { ram : Dict Int Int
    , sp : Int
    , borderColour : Int
    , speaker : Bool
    }


type alias Z80EnvWithTime =
    { z80env : Z80Env
    , time : CpuTimeCTime
    }


z80env_constructor =
    Z80Env Z80Ram.constructor Keyboard.constructor (CpuTimeCTime c_FRSTART 0) 0



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


mem16 : Int -> Z80ROM -> Z80Env -> CpuTimeAnd16BitValue
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
                value =
                    rom48k |> get16BitROMValue addr

                --low =
                --    getROMValue addr rom48k
                --
                --high =
                --    getROMValue (addr1 + 0x4000) rom48k
            in
            --CpuTimeAnd16BitValue { z80env_time | ctime = c_NOCONT } (Bitwise.or low (shiftLeftBy8 high))
            CpuTimeAnd16BitValue { z80env_time | ctime = c_NOCONT } value

setRam : Int -> Int -> Z80Env -> Z80Env
setRam addr value z80env =
    --    --let
    --    --ram_value = getValue addr z80env.ram
    --    --n = if addr == 0x1CB6 || addr == 0x1CB7 then
    --    --       debug_log "Alert!" ("setting " ++ (addr |> toHexString) ++ " from " ++ (ram_value |> toHexString2) ++ " to " ++ (value |> toHexString2)) Nothing
    --    --    else
    --    --       Nothing
    --    --in
    { sp = z80env.sp, borderColour = z80env.borderColour, ram = z80env.ram |> Dict.insert addr value, speaker = z80env.speaker }



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


setMemWithTime : Int -> Int -> Z80EnvWithTime -> Z80EnvWithTime
setMemWithTime z80_addr value z80env =
    let
        env =
            z80env.z80env

        cpu_time =
            z80env.time

        ( env2, time ) =
            setMem z80_addr value cpu_time env
    in
    { z80env = env2, time = time }


setMem : Int -> Int -> CpuTimeCTime -> Z80Env -> ( Z80Env, CpuTimeCTime )
setMem z80_addr value time_input z80env =
    let
        z80env_time =
            case time_input.ctime of
                NoCont ->
                    time_input

                ContUntil until ->
                    let
                        n =
                            time_input.cpu_time - until
                    in
                    if n > 0 then
                        time_input |> cont n

                    else
                        { time_input | ctime = NoCont }

        addr =
            z80_addr - 0x4000

        ( new_env, ctime ) =
            if addr < 0x4000 then
                if addr < 0 then
                    ( z80env, NoCont )

                else
                    let
                        z80env_1_time =
                            z80env_time |> cont1 0

                        new_time =
                            ContUntil (z80env_1_time.cpu_time + 3)

                        -- maybe this code is all about refreshing the screen only if it changes...?
                        --ram_value =
                        --    z80env |> getRamValue addr z80_rom
                    in
                    --if ram_value == value then
                    --    ( z80env, new_time )
                    --else
                    --( z80env |> setRam addr value, new_time )
                    ( { z80env | ram = z80env.ram |> Dict.insert addr value }, new_time )

            else
                --( z80env |> setRam addr value, NoCont )
                ( { z80env | ram = z80env.ram |> Dict.insert addr value }, NoCont )
    in
    ( new_env, { z80env_time | ctime = ctime } )


setMemIgnoringTime : Int -> Int -> CpuTimeCTime -> Z80Env -> Z80Env
setMemIgnoringTime z80_addr value time_input z80env =
    -- discard the extra time field mostly for tests
    setMem z80_addr value time_input z80env |> Tuple.first



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


setMem16WithTime : Int -> Int -> Z80EnvWithTime -> Z80EnvWithTime
setMem16WithTime z80_addr value z80env =
    let
        env =
            z80env.z80env

        cpu_time =
            z80env.time

        ( env2, time ) =
            setMem16 z80_addr value cpu_time env
    in
    { z80env = env2, time = time }



--setMem16IgnoringTime : Int -> Int -> CpuTimeCTime -> Z80Env -> Z80Env
--setMem16IgnoringTime addr value time_input z80env =
--    setMem16 addr value time_input z80env |> Tuple.first


setMem16 : Int -> Int -> CpuTimeCTime -> Z80Env -> ( Z80Env, CpuTimeCTime )
setMem16 addr value time_input z80env =
    let
        addr1 =
            addr - 0x3FFF
    in
    if Bitwise.and addr1 0x3FFF /= 0 then
        let
            z80env_time =
                case time_input.ctime of
                    NoCont ->
                        time_input

                    ContUntil until ->
                        let
                            n =
                                time_input.cpu_time - until
                        in
                        if n > 0 then
                            cont n time_input

                        else
                            time_input

            newTime =
                { z80env_time | ctime = NoCont }
        in
        if addr1 < 0 then
            ( z80env, newTime )

        else if addr1 >= 0x4000 then
            ( z80env
                |> setRam (addr1 - 1) (Bitwise.and value 0xFF)
                |> setRam addr1 (shiftRightBy8 value)
            , newTime
            )

        else
            ( z80env
                |> setMemIgnoringTime addr (Bitwise.and value 0xFF) newTime
                |> setMemIgnoringTime (addr + 1) (shiftRightBy8 value) newTime
            , newTime
            )

    else
        ( z80env
            |> setMemIgnoringTime addr (Bitwise.and value 0xFF) time_input
            |> setMemIgnoringTime (addr + 1) (shiftRightBy8 value) time_input
        , time_input
        )



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


z80_out : Int -> Int -> CpuTimeCTime -> Z80Env -> ( Z80Env, CpuTimeCTime, Bool )
z80_out portnum value clockTime env_in =
    let
        newTime =
            clockTime |> cont_port portnum
    in
    if (portnum |> Bitwise.and 0x01) == 0 then
        let
            border =
                value |> Bitwise.and 0x07

            speaker =
                (value |> Bitwise.and 0x10) /= 0

            blip =
                not env_in.speaker && speaker
        in
        if border /= env_in.borderColour then
            ( debugLog "border" border { env_in | borderColour = border, speaker = speaker }, newTime, blip )
            --else if speaker /= env_in.speaker then
            --    ( debugLog "speaker" clockTime.cpu_time { env_in | borderColour = border, speaker = speaker }, newTime, blip )

        else
            ( env_in, newTime, blip )

    else
        ( env_in, newTime, False )


z80_in : Int -> Keyboard -> CpuTimeCTime -> Z80Env -> CpuTimeAndValue
z80_in portnum keyboard clockTime _ =
    let
        newTime =
            clockTime |> cont_port portnum

        value =
            if (portnum |> Bitwise.and 0x01) == 0 then
                keyboard |> z80_keyboard_input portnum |> Bitwise.and 0xBF

            else
                0xFF

        --x =
        --    if value /= 0xFF then
        --        debugLog "keyboard value" ((portnum |> toHexString2) ++ " " ++ toHexString2 value) value
        --
        --    else
        --        value
    in
    CpuTimeAndValue newTime value



--public void push(int v) {
--	int sp;
--	time++;
--	env.mem((char)((sp=SP)-1), v>>>8);
--	time += 3;
--	env.mem(SP = (char)(sp-2), v&0xFF);
--	time += 3;
--}


z80_push : Int -> CpuTimeCTime -> Z80Env -> Z80Env
z80_push v clockTime z80env =
    let
        --a = debug_log "push" ((v |> toHexString) ++ " onto " ++ (z80.sp |> toHexString)) Nothing
        sp_minus_1 =
            Bitwise.and (z80env.sp - 1) 0xFFFF

        new_sp =
            Bitwise.and (z80env.sp - 2) 0xFFFF

        env_2 =
            z80env
                --|> addCpuTimeEnv 1
                |> setMemIgnoringTime sp_minus_1 (shiftRightBy8 v) clockTime
                --|> addCpuTimeEnv 3
                |> setMemIgnoringTime new_sp (Bitwise.and v 0xFF) clockTime

        --|> addCpuTimeEnv 3
    in
    { env_2 | sp = new_sp }
