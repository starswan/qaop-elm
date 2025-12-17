--
-- $Id$
--


module Z80Env exposing (..)

import Bitwise
import CpuTimeCTime exposing (CTime(..), CpuTimeAnd16BitValue, CpuTimeAndValue, CpuTimeCTime, CpuTimeSpAnd16BitValue, addCpuTimeTime, cont, cont1, cont_port)
import Dict exposing (Dict)
import Keyboard exposing (Keyboard, z80_keyboard_input)
import Utils exposing (shiftRightBy8)
import Z80Ram exposing (Z80Ram)


type alias Z80Env =
    { -- changing this to an array results in a recursion error in the browser :-(
      ram : Dict Int Int
    , sp : Int
    }


type alias Z80EnvWithTime =
    { z80env : Z80Env
    , time : CpuTimeCTime
    }


type alias EnvWithPCAndValue =
    { env : Z80Env
    , pc : Int
    , value : Int
    }


z80env_constructor =
    Z80Env Dict.empty 0


setRam : Int -> Int -> Z80Env -> Z80Env
setRam addr value z80env =
    --    --let
    --    --ram_value = getValue addr z80env.ram
    --    --n = if addr == 0x1CB6 || addr == 0x1CB7 then
    --    --       debug_log "Alert!" ("setting " ++ (addr |> toHexString) ++ " from " ++ (ram_value |> toHexString2) ++ " to " ++ (value |> toHexString2)) Nothing
    --    --    else
    --    --       Nothing
    --    --in
    { z80env | ram = z80env.ram |> Dict.insert addr value }



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


setMem16IgnoringTime : Int -> Int -> CpuTimeCTime -> Z80Env -> Z80Env
setMem16IgnoringTime addr value time_input z80env =
    setMem16 addr value time_input z80env |> Tuple.first


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


z80_out : Int -> Int -> CpuTimeCTime -> Z80Env -> ( Z80Env, CpuTimeCTime )
z80_out portnum _ clockTime env_in =
    let
        newTime =
            clockTime |> cont_port portnum
    in
    ( env_in, newTime )


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
