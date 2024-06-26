module CpuTimeCTime exposing (..)

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Z80Byte exposing (Z80Byte)
import Z80Word exposing (Z80Word, z80wordToInt)


type
    InstructionDuration
    --= ZeroTStates
    = FourTStates
    | FiveTStates
    | SixTStates
    | SevenTStates
    | EightTStates
    | TenTStates
    | ElevenTStates
    | TwelveTStates
    | ThirteenTStates
    | FifteenTStates
    | SixteenTStates
    | SeventeenTStates
    | TwentyTStates


c_NOCONT =
    99999


c_SCRENDT =
    191 * 224 + 126


type alias CpuTimeCTime =
    { cpu_time : Int
    , ctime : Int
    }


type alias CpuTimeAndValue =
    { time : CpuTimeCTime
    , value : Z80Byte
    }


type alias CpuTimeAndWord =
    { time : CpuTimeCTime
    , value : Z80Word
    }


type alias CpuTimeAndZ80Byte =
    { time : CpuTimeCTime
    , value : Z80Byte
    }


type alias CpuTimeAnd16BitValue =
    { time : CpuTimeCTime
    , value16 : Z80Word
    }


type alias CpuTimePcAndValue =
    { time : CpuTimeCTime
    , pc : Z80Word
    , value : Z80Byte
    }


type alias CpuTimePcAnd16BitValue =
    { time : CpuTimeCTime
    , pc : Z80Word
    , value16 : Z80Word
    }


type alias CpuTimePcAndZ80Byte =
    { time : CpuTimeCTime
    , pc : Z80Word
    , value : Z80Byte
    }



--type alias CpuTimePcAnd16BitValue =
--    { time : CpuTimeCTime
--    , pc : Int
--    , value16 : Int
--    }


type alias CpuTimeSpAndValue =
    { time : CpuTimeCTime
    , sp : Int
    , value : Int
    }


type alias CpuTimeSpAnd16BitValue =
    { time : CpuTimeCTime
    , sp : Z80Word
    , value16 : Z80Word
    }


type alias CpuTimeAndPc =
    { time : CpuTimeCTime
    , pc : Int
    }


type CpuTimeIncrement
    = CpuTimeIncrement Int


increment3 =
    CpuTimeIncrement 3



--	private final void cont1(int t) {
--		t += cpu.time;
--		if(t<0 || t>=SCRENDT) return;
--		if((t&7) >= 6) return;
--		if(t%224 < 126)
--			cpu.time += 6 - (t&7);
--	}


cont1 : Int -> CpuTimeCTime -> CpuTimeCTime
cont1 tmp_t z80 =
    let
        t =
            tmp_t + z80.cpu_time
    in
    if (t < 0) || (t >= c_SCRENDT) then
        z80

    else if Bitwise.and t 7 >= 6 then
        z80

    else if modBy 224 t < 126 then
        z80 |> addCpuTimeTime (6 - Bitwise.and t 7)

    else
        z80



--
--	private final void cont(int n) {
--		int s, k;
--		int t = ctime;
--		if(t+n <= 0) return;
--		s = SCRENDT - t;
--		if(s < 0) return;
--		s %= 224;
--		if(s > 126) {
--			n -= s-126;
--			if(n <= 0) return;
--			t = 6; k = 15;
--		} else {
--			k = s>>>3;
--			s &= 7;
--			if(s == 7) {
--				s--;
--				if(--n == 0) return;
--			}
--			t = s;
--		}
--		n = n-1 >> 1;
--		if(k<n) n = k;
--		cpu.time += t + 6*n;
--	}


cont : Int -> CpuTimeCTime -> CpuTimeCTime
cont n z80env =
    let
        t =
            z80env.ctime
    in
    if t + n <= 0 then
        z80env

    else
        let
            s =
                c_SCRENDT - t
        in
        if s < 0 then
            z80env

        else
            let
                new_s =
                    s |> modBy 224

                maybe_ntk =
                    if new_s > 126 then
                        let
                            new_n =
                                n - (new_s - 126)
                        in
                        if new_n <= 0 then
                            Nothing

                        else
                            Just { n = new_n, t = 6, k = 15 }

                    else
                        let
                            k =
                                new_s |> shiftRightBy 3

                            s2 =
                                Bitwise.and new_s 0x07
                        in
                        if s2 == 7 then
                            let
                                n1 =
                                    n - 1
                            in
                            if n1 == 0 then
                                Nothing

                            else
                                Just { n = n1, t = s2 - 1, k = k }

                        else
                            Just { n = n, t = s2, k = k }
            in
            case maybe_ntk of
                Just ntk ->
                    let
                        n3 =
                            (ntk.n - 1) |> shiftRightBy 1

                        n4 =
                            if ntk.k < n3 then
                                ntk.k

                            else
                                n3
                    in
                    { z80env | cpu_time = z80env.cpu_time + (ntk.t + 6 * n4) }

                Nothing ->
                    z80env



--private void cont_port(int port)
--{
--	int n = cpu.time - ctime;
--	if(n>0) cont(n);
--
--	if((port&0xC000) != 0x4000) {
--		if((port&0x0001)==0)
--			cont1(1);
--		ctime = NOCONT;
--	} else {
--		ctime = cpu.time;
--		cont(2 + ((port&1)<<1));
--		ctime = cpu.time+4;
--	}
--}


cont_port : Z80Word -> CpuTimeCTime -> CpuTimeCTime
cont_port in_portn z80env =
    let
        portn =
            in_portn |> z80wordToInt

        n =
            z80env.cpu_time - z80env.ctime

        env1_time =
            if n > 0 then
                z80env |> cont n

            else
                z80env

        env2 =
            if Bitwise.and portn 0xC000 /= 0x4000 then
                let
                    env3 =
                        if Bitwise.and portn 0x01 == 0 then
                            env1_time |> cont1 1

                        else
                            env1_time
                in
                { env3 | ctime = c_NOCONT }

            else
                let
                    env3 =
                        CpuTimeCTime env1_time.cpu_time env1_time.cpu_time

                    contval =
                        Bitwise.and portn 1 |> shiftLeftBy 1

                    env4 =
                        env3 |> cont (2 + contval)
                in
                env4 |> addCpuTimeTime 4
    in
    env2


addCpuTimeTime : Int -> CpuTimeCTime -> CpuTimeCTime
addCpuTimeTime value z80env =
    { z80env | cpu_time = z80env.cpu_time + value }


addCpuTimeTimeInc : CpuTimeIncrement -> CpuTimeCTime -> CpuTimeCTime
addCpuTimeTimeInc value z80env =
    case value of
        CpuTimeIncrement int ->
            { z80env | cpu_time = z80env.cpu_time + int }


addDuration : InstructionDuration -> CpuTimeCTime -> CpuTimeCTime
addDuration duration time =
    let
        offset =
            case duration of
                --ZeroTStates ->
                --    value
                FourTStates ->
                    4

                FiveTStates ->
                    5

                SixTStates ->
                    6

                SevenTStates ->
                    7

                EightTStates ->
                    8

                TenTStates ->
                    10

                ElevenTStates ->
                    11

                TwelveTStates ->
                    12

                ThirteenTStates ->
                    13

                FifteenTStates ->
                    15

                SixteenTStates ->
                    16

                SeventeenTStates ->
                    17

                TwentyTStates ->
                    20
    in
    { time | cpu_time = time.cpu_time + offset }
