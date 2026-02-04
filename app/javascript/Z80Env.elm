--
-- $Id$
--


module Z80Env exposing (..)

import Bitwise
import CpuTimeCTime exposing (CTime(..), CpuTimeAnd16BitValue, CpuTimeAndValue, CpuTimeCTime, CpuTimeSpAnd16BitValue, cont, cont1, cont_port)
import Dict exposing (Dict)
import Keyboard exposing (Keyboard, z80_keyboard_input)
import MemoryAddress exposing (HimemType(..), MemoryAddress(..), RamAddress(..), ScreenType(..))
import ScreenStorage exposing (getScreenValue)
import Utils exposing (shiftRightBy8)
import Z80MemoryDict exposing (getMemValue)
import Z80Ram exposing (Z80Ram)
import Z80Rom exposing (Z80ROM)


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

        --addr =
        --    z80_addr - 0x4000
        memAddress =
            z80_addr |> MemoryAddress.fromInt

        ( new_env, ctime ) =
            --if addr < 0x4000 then
            --    if addr < 0 then
            --        ( z80env, NoCont )
            --
            --    else
            --        let
            --            z80env_1_time =
            --                z80env_time |> cont1 0
            --
            --            new_time =
            --                ContUntil (z80env_1_time.cpu_time + 3)
            --        in
            --        ( z80env |> setRam addr value, new_time )
            --
            --else
            --    ( z80env |> setRam addr value, NoCont )
            case memAddress of
                ROM _ ->
                    ( z80env, NoCont )

                RAM ramaddress ->
                    case ramaddress of
                        ULAMem _ _ ->
                            let
                                z80env_1_time =
                                    z80env_time |> cont1 0

                                new_time =
                                    ContUntil (z80env_1_time.cpu_time + 3)
                            in
                            ( z80env |> setRamMemoryValue ramaddress value, new_time )

                        Himem _ _ ->
                            ( z80env |> setRamMemoryValue ramaddress value, NoCont )
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
        --addr1 =
        --    addr - 0x3FFF
        ( memAddress, memAddr1 ) =
            addr |> MemoryAddress.fromInt16
    in
    --if Bitwise.and addr1 0x3FFF /= 0 then
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
    --if addr1 < 0 then
    --    ( z80env, newTime )
    --
    --else if addr1 >= 0x4000 then
    --    ( z80env
    --        |> setRam (addr1 - 1) (Bitwise.and value 0xFF)
    --        |> setRam addr1 (shiftRightBy8 value)
    --    , newTime
    --    )
    --
    --else
    --    ( z80env
    --        |> setMemIgnoringTime addr (Bitwise.and value 0xFF) newTime
    --        |> setMemIgnoringTime (addr + 1) (shiftRightBy8 value) newTime
    --    , newTime
    --    )
    case memAddress of
        ROM _ ->
            ( z80env, newTime )

        RAM ramaddress ->
            let
                ( lowenv, time ) =
                    ( z80env |> setRamMemoryValue ramaddress (Bitwise.and value 0xFF), newTime )

                ( high, time2 ) =
                    case memAddr1 of
                        ROM _ ->
                            ( lowenv, time )

                        RAM ramaddress1 ->
                            ( lowenv |> setRamMemoryValue ramaddress1 (shiftRightBy8 value), time )
            in
            ( high, time2 )



--case ramaddress of
--    ULAMem _ _ ->
--        let
--            ( env_1, time1 ) =
--                z80env |> setMem addr (Bitwise.and value 0xFF) newTime
--        in
--        env_1 |> setMem (addr + 1) (shiftRightBy8 value) time1
--
--    --( z80env
--    --    |> setMemIgnoringTime addr (Bitwise.and value 0xFF) newTime
--    --    |> setMemIgnoringTime (addr + 1) (shiftRightBy8 value) newTime
--    --, newTime
--    --)
--    Himem _ _ ->
--        --
--        ( z80env
--            |> setRam (addr1 - 1) (Bitwise.and value 0xFF)
--            |> setRam addr1 (shiftRightBy8 value)
--        , newTime
--        )
--else
--    ( z80env
--        |> setMemIgnoringTime addr (Bitwise.and value 0xFF) time_input
--        |> setMemIgnoringTime (addr + 1) (shiftRightBy8 value) time_input
--    , time_input
--    )
--contPortEnv : Int -> Z80Env -> Z80Env
--contPortEnv portn z80env =
--    { z80env | time = z80env.time |> cont_port portn }
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



--setRam : Int -> Int -> Z80Env -> Z80Env
--setRam addr value z80env =
--    --let
--    --ram_value = getValue addr z80env.ram
--    --n = if addr == 0x1CB6 || addr == 0x1CB7 then
--    --       debug_log "Alert!" ("setting " ++ (addr |> toHexString) ++ " from " ++ (ram_value |> toHexString2) ++ " to " ++ (value |> toHexString2)) Nothing
--    --    else
--    --       Nothing
--    --in
--    -- addr is in range 0 - 0xBFFF
--    { z80env | ram = z80env.ram |> Dict.insert addr value }


setRamMemoryValue : RamAddress -> Int -> Z80Env -> Z80Env
setRamMemoryValue ramAddress value z80env =
    case ramAddress of
        ULAMem screenType addr ->
            case screenType of
                Screen ->
                    { z80env | ram = z80env.ram |> Dict.insert addr value }

                ULA ->
                    { z80env | ram = z80env.ram |> Dict.insert (addr + 6912) value }

        Himem himemType addr ->
            case himemType of
                HimemLow ->
                    { z80env | ram = z80env.ram |> Dict.insert (addr + 0x4000) value }

                HimemHigh ->
                    { z80env | ram = z80env.ram |> Dict.insert (addr + 0x8000) value }



--getRamValue : Int -> Z80ROM -> Z80Env -> Int
--getRamValue addr z80rom z80env =
--    case z80env.ram |> Dict.get addr of
--        Just a ->
--            a
--
--        Nothing ->
--            z80rom.z80ram |> Z80Ram.getRamValue addr


getRamMemoryValue : RamAddress -> CpuTimeCTime -> Z80ROM -> Z80Env -> ( Int, CpuTimeCTime )
getRamMemoryValue ramAddress clockTime z80rom z80env =
    let
        ( addressInt, time ) =
            case ramAddress of
                ULAMem screenType addr ->
                    let
                        newTime =
                            clockTime |> cont1 0

                        newNewTime =
                            { newTime | ctime = ContUntil (newTime.cpu_time + 3) }
                    in
                    case screenType of
                        Screen ->
                            ( addr, newNewTime )

                        ULA ->
                            ( addr + 6912, newNewTime )

                Himem himemType addr ->
                    let
                        newTime =
                            { clockTime | ctime = NoCont }
                    in
                    case himemType of
                        HimemLow ->
                            ( addr + 0x4000, newTime )

                        HimemHigh ->
                            ( addr + 0x8000, newTime )
    in
    case z80env.ram |> Dict.get addressInt of
        Just a ->
            ( a, time )

        Nothing ->
            let
                ram_addr =
                    addressInt - 6912

                z80ram =
                    z80rom.z80ram
            in
            if ram_addr >= 0 then
                ( z80ram.non_screen |> getMemValue ram_addr, time )

            else
                ( z80ram.screen |> getScreenValue addressInt, time )
