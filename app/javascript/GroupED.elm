module GroupED exposing (..)

import Bitwise exposing (complement, shiftLeftBy, shiftRightBy)
import CpuTimeCTime exposing (CpuTimeCTime, InstructionDuration(..), ShortDelay(..))
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..))
import RegisterChange exposing (EDFourByteChange(..), EDRegisterChange(..), InterruptChange(..), RegisterFlagChange(..), SixteenBit(..))
import Utils exposing (char, shiftLeftBy8, shiftRightBy8, toHexString2)
import Z80Change exposing (Z80Change(..))
import Z80Core exposing (DirectionForLDIR(..), RepeatPCOffset(..), Z80Core)
import Z80Debug exposing (debugLog)
import Z80Env exposing (Z80Env, setMem, setMemIgnoringTime, z80_in)
import Z80Flags exposing (FlagRegisters, c_F3, c_F5, c_F53, c_FC, c_FH, f_szh0n0p, z80_sub)
import Z80Mem exposing (mem)
import Z80Registers exposing (ChangeMainRegister(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (InterruptMode(..), InterruptRegisters, MainWithIndexRegisters, get_bc, get_de, set_bc_main, set_de_main)


execute_ED78 : Z80ROM -> CpuTimeCTime -> Z80Core -> Z80Core
execute_ED78 rom48k clockTime z80 =
    --  case 0x78: MP=(v=B<<8|C)+1; f_szh0n0p(A=env.in(v)); time+=4; break;
    let
        v =
            z80.main |> get_bc

        new_a =
            z80.env |> z80_in v rom48k.keyboard clockTime

        flags =
            z80.flags

        -- can't really log this is as it's the keyboard scanner run every 20ms
        --new_flags =
        --    debugLog "ED78 - IN A,(C)" ( new_a.value |> toHexString2, z80.main.b |> toHexString2, z80.main.c |> toHexString2 ) { flags | a = new_a.value } |> f_szh0n0p new_a.value
        new_flags =
            { flags | a = new_a.value } |> f_szh0n0p new_a.value
    in
    --( DeltaFlags new_flags, new_a.time |> addCpuTimeTime 4, IncrementByTwo )
    { z80 | flags = new_flags }



-- case 0x46:
-- case 0x4E:
-- case 0x56:
-- case 0x5E:
-- case 0x66:
-- case 0x6E:
-- case 0x76:
-- case 0x7E: IM = c>>3&3; break;


adc_hl_sp : Z80ROM -> CpuTimeCTime -> Z80Core -> Z80Core
adc_hl_sp _ clockTime z80 =
    -- case 0x7A: adc_hl(SP); break;
    z80 |> adc_hl z80.env.sp


execute_ED70 : Z80ROM -> CpuTimeCTime -> Z80Core -> Z80Core
execute_ED70 rom48k clockTime z80 =
    let
        v =
            z80.main |> get_bc

        new_a =
            z80.env |> z80_in v rom48k.keyboard clockTime

        new_flags =
            debugLog "ED70 - IN (C)" ( new_a.value, z80.main.b |> toHexString2, z80.main.c |> toHexString2 ) z80.flags |> f_szh0n0p new_a.value
    in
    --( DeltaFlags new_flags, new_a.time |> addCpuTimeTime 4, IncrementByTwo )
    { z80 | flags = new_flags }



-- case 0x78: MP=(v=B<<8|C)+1; f_szh0n0p(A=env.in(v)); time+=4; break;
-- case 0x41: env.out(B<<8|C,B); time+=4; break;
-- case 0x49: env.out(B<<8|C,C); time+=4; break;
-- case 0x51: env.out(B<<8|C,D); time+=4; break;
-- case 0x59: env.out(B<<8|C,E); time+=4; break;
-- case 0x61: env.out(B<<8|C,HL>>>8); time+=4; break;
-- case 0x69: env.out(B<<8|C,HL&0xFF); time+=4; break;
-- case 0x71: env.out(B<<8|C,0); time+=4; break;
-- case 0x79: MP=(v=B<<8|C)+1; env.out(v,A); time+=4; break;
-- case 0x62: sbc_hl(HL); break;
-- case 0x63: MP=(v=imm16())+1; env.mem16(v,HL); time+=6; break;
-- case 0x6B: MP=(v=imm16())+1; HL=env.mem16(v); time+=6; break;
-- case 0x7A: adc_hl(SP); break;
-- case 0x45:
-- case 0x4D:
-- case 0x55:
-- case 0x5D:
-- case 0x65:
-- case 0x6D:
-- case 0x75:
-- case 0x7D: IFF|=IFF>>1; MP=PC=pop(); break;
-- case 0xA0: ldir(1,false); break;
-- case 0xA8: ldir(-1,false); break;
-- case 0xA1: cpir(1,false); break;
-- case 0xA9: cpir(-1,false); break;
-- case 0xB1: cpir(1,true); break;
-- case 0xB9: cpir(-1,true); break;
-- case 0xA2:
-- case 0xA3:
-- case 0xAA:
-- case 0xAB:
-- case 0xB2:
-- case 0xB3:
-- case 0xBA:
-- case 0xBB: inir_otir(c); break;
--// -------------- >8
--    default: System.out.println(PC+": Not emulated ED/"+c);
--    }
--  }
--
--
--  private void sbc_hl(int b)
--  {
--    int a,r;
--    r = (a=HL) - b - (Ff>>>8 & FC);
--    Ff = r>>>8;
--    Fa = a>>>8; Fb = ~(b>>>8);
--    HL = r = (char)r;
--    Fr = r>>>8 | r<<8;
--    MP = a+1;
--    time += 7;
--  }


sbc_hl : Int -> Z80Core -> ( FlagRegisters, MainWithIndexRegisters )
sbc_hl b z80 =
    let
        a =
            z80.main.hl

        r1 =
            a - b - Bitwise.and (shiftRightBy8 z80.flags.ff) c_FC

        ff =
            shiftRightBy8 r1

        fa =
            shiftRightBy8 a

        fb =
            complement (shiftRightBy8 b)

        r =
            char r1

        fr =
            Bitwise.or (shiftRightBy8 r) (shiftLeftBy8 r)

        main =
            z80.main

        flags =
            z80.flags
    in
    ( { flags | ff = ff, fa = fa, fb = fb, fr = fr }, { main | hl = r } )



-- return data, delay and PC


ldir : DirectionForLDIR -> Bool -> Z80ROM -> CpuTimeCTime -> Z80Core -> ( Z80Core, Maybe ShortDelay, RepeatPCOffset )
ldir incOrDec repeat rom48k clockTime z80 =
    --  private void ldir(int i, boolean r)
    let
        --v = env.mem(a = HL); HL = (char)(a+i); time += 3;
        --env.mem(a = de(), v); de((char)(a+i)); time += 5;
        main =
            z80.main

        v1 =
            z80.env |> mem main.hl clockTime rom48k

        de =
            main |> get_de

        ( env_1, newClock ) =
            z80.env |> setMem de v1.value v1.time

        ( new_hl, new_de ) =
            case incOrDec of
                Forwards ->
                    ( main.hl + 1, de + 1 )

                Backwards ->
                    ( main.hl - 1, de - 1 )

        z80_2 =
            { z80
                | env = env_1
                , main =
                    { main | hl = new_hl |> Bitwise.and 0xFFFF }
                        |> set_de_main (new_de |> Bitwise.and 0xFFFF)
            }

        --if(Fr!=0) Fr = 1; // keep Z
        fr =
            if z80_2.flags.fr /= 0 then
                1

            else
                z80_2.flags.fr

        --v += A;
        v2 =
            v1.value + z80_2.flags.a

        --Ff = Ff&~F53 | v&F3 | v<<4&F5;
        --ff =
        --    Bitwise.or (Bitwise.or (Bitwise.and z80_2.flags.ff (complement c_F53)) (Bitwise.and v2 c_F3)) (Bitwise.and (shiftLeftBy 4 v2) c_F5)
        ff =
            (z80_2.flags.ff |> Bitwise.and (complement c_F53))
                |> Bitwise.or (v2 |> Bitwise.and c_F3)
                |> Bitwise.or (v2 |> shiftLeftBy 4 |> Bitwise.and c_F5)

        --    bc(a = (char)(bc()-1));
        --    v = 0;
        --    if(a!=0) {
        --      if(r) {
        --        time += 5;
        --        MP = (PC = (char)(PC-2)) + 1;
        --      }
        --      v = 0x80;
        --    }
        --    Fa = Fb = v;
        new_bc =
            Bitwise.and ((main |> get_bc) - 1) 0xFFFF

        ( v, pc, time ) =
            if new_bc /= 0 then
                if repeat then
                    ( 0x80, JumpBack, Just FiveExtraTStates )

                else
                    ( 0x80, NoOffset, Nothing )

            else
                ( 0, NoOffset, Nothing )

        flags =
            z80_2.flags
    in
    ( { z80_2
        | main = z80_2.main |> set_bc_main new_bc
        , flags = { flags | fr = fr, ff = ff, fa = v, fb = v }
      }
    , time
    , pc
    )



--set_i : Int -> Z80Core -> InterruptRegisters
--set_i v z80_core =
--    --  void i(int v) {IR = IR&0xFF | v<<8;}
--    let
--        ir =
--            Bitwise.or (Bitwise.and z80_core.interrupts.ir 0xFF) (shiftLeftBy8 v)
--
--        main =
--            z80_core.interrupts
--    in
--    --{ z80 | interrupts = { interrupts | ir = ir } }
--    { main | ir = ir }
--  private void adc_hl(int b)
--  {
--    int a,r;
--    r = (a=HL) + b + (Ff>>>8 & FC);
--    Ff = r>>>8;
--    Fa = a>>>8; Fb = b>>>8;
--    HL = r = (char)r;
--    Fr = r>>>8 | r<<8;
--    MP = a+1;
--    time += 7;
--  }


adc_hl : Int -> Z80Core -> Z80Core
adc_hl b z80 =
    let
        z80_main =
            z80.main

        ( flags, hl ) =
            ed_adc_hl b z80_main z80.flags
    in
    --( FlagsWithPCMainAndCpuTime flags { z80_main | hl = hl }, clockTime |> addCpuTimeTime 7, IncrementByTwo )
    { z80 | flags = flags, main = { z80_main | hl = hl } }



--private void cpir(int i, boolean r)
--{
--  int a,b,v;
--
--  v = A-(b = env.mem(a=HL)) & 0xFF;
--  MP += i;
--  HL = (char)(a+i);
--  time += 8;
--
--  Fr = v & 0x7F | v>>>7;
--  Fb = ~(b | 0x80);
--  Fa = A & 0x7F;
--
--  bc(a = (char)(bc() - 1));
--  if(a!=0) {
--    Fa |= 0x80;
--    Fb |= 0x80;
--    if(r && v!=0) {
--      MP = (PC = (char)(PC-2)) + 1;
--      time += 5;
--    }
--  }
--
--  Ff = Ff&~0xFF | v&~F53;
--  if(((v ^ b ^ A)&FH) != 0) v--;
--  Ff |= v<<4&0x20 | v&8;
--}


cpir : DirectionForLDIR -> Bool -> Z80ROM -> CpuTimeCTime -> Z80Core -> ( Z80Core, CpuTimeCTime, RepeatPCOffset )
cpir incOrDec repeat rom48k clockTime z80_core =
    let
        z80_flags =
            z80_core.flags

        old_a =
            z80_core.main.hl

        b =
            z80_core.env |> mem old_a clockTime rom48k

        v =
            z80_flags.a - b.value |> Bitwise.and 0xFF

        hl =
            case incOrDec of
                Forwards ->
                    (old_a + 1) |> Bitwise.and 0xFFFF

                Backwards ->
                    (old_a - 1) |> Bitwise.and 0xFFFF

        fr =
            Bitwise.or (v |> Bitwise.and 0x7F) (v |> shiftRightBy 7)

        fb =
            b.value |> Bitwise.or 0x80 |> complement

        fa =
            z80_core.flags.a |> Bitwise.and 0x7F

        new_bc =
            (z80_core.main |> get_bc) - 1 |> Bitwise.and 0xFFFF

        a =
            new_bc

        ( new_fa, new_fb, new_pc ) =
            if a /= 0 then
                let
                    newish_pc =
                        if repeat && v /= 0 then
                            JumpBack

                        else
                            NoOffset
                in
                ( Bitwise.or 0x80 fa, Bitwise.or 0x80 fb, newish_pc )

            else
                ( fa, fb, NoOffset )

        old_ff =
            Bitwise.or (z80_core.flags.ff |> Bitwise.and 0xFF00) (v |> Bitwise.and (c_F53 |> complement))

        new_v =
            if (v |> Bitwise.xor b.value |> Bitwise.xor z80_core.flags.a |> Bitwise.and c_FH) /= 0 then
                v - 1

            else
                v

        ff =
            Bitwise.or old_ff ((new_v |> shiftLeftBy 4) |> Bitwise.and 0x20) |> Bitwise.or (Bitwise.and new_v 0x08)

        newMain =
            z80_core.main |> set_bc_main new_bc
    in
    --HLBCWithFlagsAndPc hl new_bc { z80_flags | ff = ff, fa = new_fa, fb = new_fb, fr = fr } new_pc
    ( { z80_core | main = { newMain | hl = hl }, flags = { z80_flags | ff = ff, fa = new_fa, fb = new_fb, fr = fr } }
    , b.time
    , new_pc
    )



--  private void rld()
--  {
--    int v = env.mem(HL)<<4 | A&0x0F;
--    time += 7;
--    f_szh0n0p(A = A&0xF0 | v>>>8);
--    env.mem(HL, v & 0xFF);
--    MP = HL+1;
--    time += 3;
--  }


rld : Z80ROM -> CpuTimeCTime -> Z80Core -> Z80Core
rld rom48k clockTime z80 =
    let
        v_lhs_1 =
            z80.env |> mem z80.main.hl clockTime rom48k

        v_rhs =
            Bitwise.and z80.flags.a 0x0F

        v_lhs =
            v_lhs_1.value |> shiftLeftBy 4

        v =
            Bitwise.or v_lhs v_rhs

        new_a =
            Bitwise.and z80.flags.a 0xF0 |> Bitwise.or (shiftRightBy8 v)

        flags =
            z80.flags

        new_flags =
            { flags | a = new_a } |> f_szh0n0p new_a

        ( env_1, newClock ) =
            z80.env |> setMem z80.main.hl (Bitwise.and v 0xFF) v_lhs_1.time
    in
    { z80 | env = env_1, flags = new_flags }



--private void rrd()
--{
--  int v = env.mem(HL) | A<<8;
--  time += 7;
--  f_szh0n0p(A = A&0xF0 | v&0x0F);
--  env.mem(HL, v>>>4 & 0xFF);
--  MP = HL+1;
--  time += 3;
--}


rrd : Z80ROM -> CpuTimeCTime -> Z80Core -> Z80Core
rrd rom48k clockTime z80 =
    let
        v_lhs =
            z80.env |> mem z80.main.hl clockTime rom48k

        v_rhs =
            z80.flags.a |> shiftLeftBy8

        v =
            Bitwise.or v_lhs.value v_rhs

        new_a =
            Bitwise.and z80.flags.a 0xF0 |> Bitwise.or (v |> Bitwise.and 0x0F)

        flags =
            z80.flags

        new_flags =
            { flags | a = new_a } |> f_szh0n0p new_a

        env_1 =
            z80.env |> setMemIgnoringTime z80.main.hl (Bitwise.and (v |> shiftRightBy 4) 0xFF) v_lhs.time
    in
    { z80 | env = env_1, flags = new_flags }


fourByteMainED : Dict Int ( MainWithIndexRegisters -> Int -> EDFourByteChange, InstructionDuration )
fourByteMainED =
    Dict.fromList
        [ ( 0x43, ( \z80main address -> SetMemFrom address RegBC, TwentyTStates ) )
        , ( 0x4B, ( \z80main address -> GetFromMem address RegBC, TwentyTStates ) )
        , ( 0x53, ( \z80main address -> SetMemFrom address RegDE, TwentyTStates ) )
        , ( 0x5B, ( \z80main address -> GetFromMem address RegDE, TwentyTStates ) )
        , ( 0x63, ( \z80main address -> SetMemFrom address RegHL, TwentyTStates ) )
        , ( 0x6B, ( \z80main address -> GetFromMem address RegHL, TwentyTStates ) )
        , ( 0x73, ( \z80main address -> SetMemFrom address RegSP, TwentyTStates ) )
        , ( 0x7B, ( \z80main address -> GetFromMem address RegSP, TwentyTStates ) )
        ]


singleByteMainRegsED : Dict Int ( EDRegisterChange, InstructionDuration )
singleByteMainRegsED =
    Dict.fromList
        [ -- ED08/ED09 no-op in Java version of Qaop
          ( 0x00, ( EDNoOp, EightTStates ) )
        , ( 0x01, ( EDNoOp, EightTStates ) )
        , ( 0x04, ( EDNoOp, EightTStates ) )
        , ( 0x08, ( EDNoOp, EightTStates ) )
        , ( 0x09, ( EDNoOp, EightTStates ) )
        , ( 0x10, ( EDNoOp, EightTStates ) )
        , ( 0x11, ( EDNoOp, EightTStates ) )
        , ( 0x14, ( EDNoOp, EightTStates ) )
        , ( 0x18, ( EDNoOp, EightTStates ) )
        , ( 0x19, ( EDNoOp, EightTStates ) )
        , ( 0x20, ( EDNoOp, EightTStates ) )
        , ( 0x21, ( EDNoOp, EightTStates ) )
        , ( 0x24, ( EDNoOp, EightTStates ) )
        , ( 0x28, ( EDNoOp, EightTStates ) )
        , ( 0x29, ( EDNoOp, EightTStates ) )
        , ( 0x34, ( EDNoOp, EightTStates ) )
        , ( 0x38, ( EDNoOp, EightTStates ) )
        , ( 0x39, ( EDNoOp, EightTStates ) )
        , ( 0x40, ( InRC ChangeMainB, TwelveTStates ) )

        -- case 0x41: env.out(B<<8|C,B); time+=4; break;
        , ( 0x41, ( EDNoOp, EightTStates ) )

        -- case 0x42: sbc_hl(B<<8|C); break;
        , ( 0x42, ( SbcHL RegBC, FifteenTStates ) )

        -- RETN - end of NMI. NMIs aren't enabled on the Spectrum?
        , ( 0x45, ( EDNoOp, EightTStates ) )

        --case 0x46:
        --case 0x4E:
        --case 0x56:
        --case 0x5E:
        --case 0x66:
        --case 0x6E:
        --case 0x76:
        --case 0x7E: IM = c>>3&3; break;
        --https://www.cpcwiki.eu/index.php/Z80_-_undocumented_opcodes
        --( 0x46, ( \_ -> RegChangeIm (0x46 |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        -- IM0 (iM == 1) seems to crash the emulator quite badly because of this code:
        --  1 ->
        --      new_z80 |> im0 bus
        -- iM == 0 (which has the same code) appears to be impossible as there is no IM3 instruction
        --( 0x46, ( \_ -> RegChangeIm 0, EightTStates ) )
        , ( 0x46, ( EDNoOp, EightTStates ) )
        , ( 0x48, ( InRC ChangeMainC, TwelveTStates ) )

        -- case 0x49: env.out(B<<8|C,C); time+=4; break;       , IncrementByTwo
        , ( 0x49, ( EDNoOp, EightTStates ) )

        -- RETI (return from maskable interupt, unused on the Spectrum I'm pretty sure)
        , ( 0x4D, ( EDNoOp, EightTStates ) )

        --( 0x4E, ( \_ -> RegChangeIm (0x4E |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        --, ( 0x4E, ( \_ -> RegChangeIm 0, EightTStates ) )
        , ( 0x4E, ( EDNoOp, EightTStates ) )
        , ( 0x50, ( InRC ChangeMainD, TwelveTStates ) )

        -- case 0x51: env.out(B<<8|C,D); time+=4; break;       , IncrementByTwo
        , ( 0x51, ( EDNoOp, EightTStates ) )

        -- case 0x52: sbc_hl(D<<8|E); break;
        , ( 0x52, ( SbcHL RegDE, FifteenTStates ) )

        -- case 0x7D: IFF|=IFF>>1; MP=PC=pop(); break;
        -- TODO: Implement ED 7D (all these are the same)
        , ( 0x55, ( EDNoOp, EightTStates ) )

        --, ( 0x56, ( \_ -> RegChangeIm (0x56 |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        , ( 0x56, ( RegChangeIm IM1, EightTStates ) )
        , ( 0x58, ( InRC ChangeMainE, TwelveTStates ) )

        -- case 0x59: env.out(B<<8|C,E); time+=4; break;       , IncrementByTwo
        , ( 0x59, ( EDNoOp, EightTStates ) )
        , ( 0x5D, ( EDNoOp, EightTStates ) )

        --, ( 0x5E, ( \_ -> RegChangeIm (0x5E |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        , ( 0x5E, ( RegChangeIm IM2, EightTStates ) )
        , ( 0x60, ( InRC ChangeMainH, TwelveTStates ) )

        -- case 0x61: env.out(B<<8|C,HL>>>8); time+=4; break;  , IncrementByTwo
        , ( 0x61, ( EDNoOp, EightTStates ) )

        -- case 0x62: sbc_hl(HL); break;
        , ( 0x62, ( SbcHL RegHL, FifteenTStates ) )
        , ( 0x65, ( EDNoOp, EightTStates ) )

        --, ( 0x66, (  RegChangeIm (0x66 |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        --, ( 0x66, (  RegChangeIm 0, EightTStates ) )
        , ( 0x66, ( EDNoOp, EightTStates ) )
        , ( 0x67, ( RRD, EighteenTStates ) )
        , ( 0x68, ( InRC ChangeMainL, TwelveTStates ) )

        -- case 0x69: env.out(B<<8|C,HL&0xFF); time+=4; break; , IncrementByTwo
        , ( 0x69, ( EDNoOp, EightTStates ) )
        , ( 0x6D, ( EDNoOp, EightTStates ) )

        --, ( 0x6E, (  RegChangeIm (0x6E |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        --, ( 0x6E, (  RegChangeIm 0, EightTStates ) )
        , ( 0x6E, ( EDNoOp, EightTStates ) )
        , ( 0x6F, ( RLD, EighteenTStates ) )
        , ( 0x70, ( IN_C, TwelveTStates ) )

        -- case 0x71: env.out(B<<8|C,0); time+=4; break;       , IncrementByTwo
        , ( 0x71, ( EDNoOp, EightTStates ) )

        -- case 0x72: sbc_hl(SP); break;
        , ( 0x72, ( SbcHL RegSP, FifteenTStates ) )
        , ( 0x75, ( EDNoOp, EightTStates ) )

        --, ( 0x76, (  RegChangeIm (0x76 |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        , ( 0x76, ( RegChangeIm IM1, EightTStates ) )
        , ( 0x78, ( IN_A_C, TwelveTStates ) )

        -- case 0x79: MP=(v=B<<8|C)+1; env.out(v,A); time+=4; b, IncrementByTworeak;
        , ( 0x79, ( EDNoOp, EightTStates ) )
        , ( 0x7A, ( AdcHLSP, FifteenTStates ) )
        , ( 0x7D, ( EDNoOp, EightTStates ) )

        --, ( 0x7E, (  RegChangeIm (0x7E |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        , ( 0x7E, ( RegChangeIm IM2, EightTStates ) )
        , ( 0xA2, ( Z80InI Forwards False, SixteenTStates ) )
        , ( 0xA3, ( Z80OutI Forwards False, SixteenTStates ) )
        , ( 0xAA, ( Z80InI Backwards False, SixteenTStates ) )
        , ( 0xAB, ( Z80OutI Backwards False, SixteenTStates ) )
        , ( 0xB2, ( Z80InI Forwards True, SixteenTStates ) )
        , ( 0xB3, ( Z80OutI Forwards True, SixteenTStates ) )
        , ( 0xBA, ( Z80InI Backwards True, SixteenTStates ) )
        , ( 0xBB, ( Z80OutI Backwards True, SixteenTStates ) )

        --The ED opcodes in the range 00-3F and 80-FF (except for the block instructions of course) do nothing at all but taking up 8 T states
        -- and incrementing the R register by 2. Most of the unlisted opcodes in the range 0x40 to 0x7f do have an effect, however
        , ( 0xBF, ( EDNoOp, EightTStates ) )

        -- case 0xA0: ldir(1,false); break;
        , ( 0xA0, ( Ldir Forwards False, SixteenTStates ) )

        -- case 0xA1: cpir(1,false); break;
        , ( 0xA1, ( Cpir Forwards False, SixteenTStates ) )

        -- case 0xA8: ldir(-1,false); break;
        , ( 0xA8, ( Ldir Backwards False, SixteenTStates ) )

        -- case 0xA9: cpir(-1,false); break;
        , ( 0xA9, ( Cpir Backwards False, SixteenTStates ) )

        -- case 0xB0: ldir(1,true); break;
        , ( 0xB0, ( Ldir Forwards True, SixteenTStates ) )

        -- case 0xB1: cpir(1,true); break;
        , ( 0xB1, ( Cpir Forwards True, SixteenTStates ) )

        -- case 0xB8: ldir(-1,true); break;
        , ( 0xB8, ( Ldir Backwards True, SixteenTStates ) )

        -- case 0xB9: cpir(-1,true); break;
        , ( 0xB9, ( Cpir Backwards True, SixteenTStates ) )

        -- case 0xA2:
        -- case 0xA3:
        -- case 0xAA:
        -- case 0xAB:
        -- case 0xB2:
        -- case 0xB3:
        -- case 0xBA:
        -- case 0xBB: inir_otir(c); break;
        ]


singleByteFlagsED : Dict Int ( FlagRegisters -> RegisterFlagChange, InstructionDuration )
singleByteFlagsED =
    Dict.fromList
        [ ( 0x44, ( ed_44_neg, EightTStates ) )

        -- case 0x47: i(A); time++; break;
        , ( 0x47, ( \z80_flags -> FlagNewIValue z80_flags.a, NineTStates ) )
        , ( 0x4C, ( ed_44_neg, EightTStates ) )

        -- case 0x4F: r(A); time++; break;
        , ( 0x4F, ( \z80_flags -> FlagNewRValue z80_flags.a, NineTStates ) )
        , ( 0x54, ( ed_44_neg, EightTStates ) )
        , ( 0x5C, ( ed_44_neg, EightTStates ) )
        , ( 0x64, ( ed_44_neg, EightTStates ) )
        , ( 0x6C, ( ed_44_neg, EightTStates ) )
        , ( 0x74, ( ed_44_neg, EightTStates ) )
        , ( 0x7C, ( ed_44_neg, EightTStates ) )
        ]


ed_44_neg : FlagRegisters -> RegisterFlagChange
ed_44_neg z80_flags =
    -- All these other ED codes are 'undocumented' and do interesting things,
    -- but point back to ED44 in Qaop Java version
    --case 0x44:
    --case 0x4C:
    --case 0x54:
    --case 0x5C:
    --case 0x64:
    --case 0x6C:
    --case 0x74:
    --case 0x7C: v=A; A=0; sub(v); break;
    let
        v =
            z80_flags.a

        new_flags =
            { z80_flags | a = 0 } |> z80_sub v
    in
    OnlyFlags new_flags


singleByteMainAndFlagsED : Dict Int ( MainWithIndexRegisters -> FlagRegisters -> Z80Change, PCIncrement, InstructionDuration )
singleByteMainAndFlagsED =
    Dict.fromList
        [ ( 0x4A, ( ed4a_adc_hl_bc, IncrementByTwo, FifteenTStates ) )
        , ( 0x5A, ( ed5a_adc_hl_de, IncrementByTwo, FifteenTStates ) )
        , ( 0x6A, ( ed6a_adc_hl_hl, IncrementByTwo, FifteenTStates ) )
        ]


ed4a_adc_hl_bc : MainWithIndexRegisters -> FlagRegisters -> Z80Change
ed4a_adc_hl_bc z80_main z80_flags =
    -- case 0x4A: adc_hl(B<<8|C); break;
    let
        ( flags, hl ) =
            ed_adc_hl (z80_main |> get_bc) z80_main z80_flags
    in
    FlagsWithHLRegister flags hl


ed5a_adc_hl_de : MainWithIndexRegisters -> FlagRegisters -> Z80Change
ed5a_adc_hl_de z80_main z80_flags =
    ---- case 0x5A: adc_hl(D<<8|E); break;
    let
        ( flags, hl ) =
            ed_adc_hl (z80_main |> get_de) z80_main z80_flags
    in
    FlagsWithHLRegister flags hl


ed6a_adc_hl_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
ed6a_adc_hl_hl z80_main z80_flags =
    ---- case 0x6A: adc_hl(HL); break;
    let
        ( flags, hl ) =
            ed_adc_hl z80_main.hl z80_main z80_flags
    in
    FlagsWithHLRegister flags hl



--  private void adc_hl(int b)
--  {
--    int a,r;
--    r = (a=HL) + b + (Ff>>>8 & FC);
--    Ff = r>>>8;
--    Fa = a>>>8; Fb = b>>>8;
--    HL = r = (char)r;
--    Fr = r>>>8 | r<<8;
--    MP = a+1;
--    time += 7;
--  }


ed_adc_hl : Int -> MainWithIndexRegisters -> FlagRegisters -> ( FlagRegisters, Int )
ed_adc_hl b z80_main z80_flags =
    let
        a =
            z80_main.hl

        r1 =
            a + b + Bitwise.and (z80_flags.ff |> shiftRightBy8) c_FC

        ff =
            r1 |> shiftRightBy8

        fa =
            a |> shiftRightBy8

        fb =
            b |> shiftRightBy8

        r =
            char r1

        fr =
            Bitwise.or (r |> shiftRightBy8) (r |> shiftLeftBy8)
    in
    ( { z80_flags | ff = ff, fa = fa, fb = fb, fr = fr }, r )



--	private void inir_otir(int op) // op: 101rd01o
--	{
--		int bc, hl, d, v;
--
--		hl = (char)(HL + (d = (op&8)==0 ? 1 : -1));
--		bc = B<<8|C;
--		time++;
--		if((op&1)==0) {
--			v = env.in(bc); time += 4;
--			MP = bc+d;
--			bc = (char)(bc-256);
--			env.mem(HL, v); time += 3;
--			d += bc;
--		} else {
--			v = env.mem(HL); time += 3;
--			bc = (char)(bc-256);
--			MP = bc+d;
--			env.out(bc, v); time += 4;
--			d = hl;
--		}
--		d = (d&0xFF) + v;
--		HL = hl;
--		B = (bc >>= 8);
--		if(op>0xB0 && bc>0) {
--			time += 5;
--			PC = (char)(PC-2);
--		}
--		int x = d&7 ^ bc;
--		Ff = bc | (d &= 0x100);
--		Fa = (Fr = bc) ^ 0x80;
--		x = 0x4B3480 >> ((x^x>>>4)&15);
--		Fb = (x^bc) & 0x80 | d>>>4 | (v & 0x80)<<2;
--	}


type alias Z80IniOuti =
    { flags : FlagRegisters
    , hl : Int
    , bc : Int
    }



--inir : DirectionForLDIR -> Bool -> Z80ROM -> Z80Core -> Z80IniOuti
--inir direction repeat rom48k z80_core =
--    let
--        d_original =
--            case direction of
--                Forwards ->
--                    1
--
--                Backwards ->
--                    -1
--
--        hl =
--            z80_core.main.hl + d_original |> Bitwise.and 0xFFFF
--
--        bc =
--            z80_core.main |> get_bc
--
--        flags =
--            z80_core.flags
--
--        new_bc =
--            bc - 256 |> Bitwise.and 0xFFFF
--
--        v =
--            z80_core.env |> z80_in bc rom48k.keyboard z80_core.clockTime
--
--        d =
--            (d_original + new_bc |> Bitwise.and 0xFFFF) + v.value
--
--        pc =
--            if repeat && (new_bc > 0) then
--                z80_core.pc
--
--            else
--                z80_core.pc + 2
--    in
--    { flags = flags |> inirOtirFlags d new_bc v.value, hl = hl, bc = new_bc }
--otir : DirectionForLDIR -> Bool -> Z80ROM -> Z80Core -> Z80IniOuti
--otir direction repeat rom48k z80_core =
--    let
--        hl =
--            case direction of
--                Forwards ->
--                    z80_core.main.hl + 1 |> Bitwise.and 0xFFFF
--
--                Backwards ->
--                    z80_core.main.hl - 1 |> Bitwise.and 0xFFFF
--
--        in_bc =
--            z80_core.main |> get_bc
--
--        flags =
--            z80_core.flags
--
--        v =
--            z80_core.env |> mem hl z80_core.clockTime rom48k
--
--        new_bc =
--            in_bc - 256 |> Bitwise.and 0xFFFF
--
--        ( env_1, time ) =
--            z80_core.env |> z80_out new_bc v.value z80_core.clockTime
--
--        pc =
--            if repeat && (new_bc > 0) then
--                z80_core.pc
--
--            else
--                z80_core.pc + 2
--    in
--    { flags = flags |> inirOtirFlags hl new_bc v.value, hl = hl, bc = new_bc }


inirOtirFlags : Int -> Int -> Int -> FlagRegisters -> FlagRegisters
inirOtirFlags d bc v flags =
    let
        x_orig =
            d |> Bitwise.and 0x07 |> Bitwise.xor bc

        ff =
            bc |> Bitwise.or (d |> Bitwise.and 0x0100)

        fr =
            bc

        fa =
            fr |> Bitwise.xor 0x80

        x =
            0x004B3480 |> shiftRightBy (x_orig |> Bitwise.xor (x_orig |> shiftRightBy 4) |> Bitwise.and 0x0F)

        fb =
            ((x |> Bitwise.xor bc) |> Bitwise.and 0x80) |> Bitwise.or (d |> shiftRightBy 4) |> Bitwise.or (v |> Bitwise.and 0x80 |> shiftLeftBy 2)
    in
    { flags | ff = ff, fr = fr, fa = fa, fb = fb }


edWithInterrupts : Dict Int ( InterruptRegisters -> InterruptChange, InstructionDuration )
edWithInterrupts =
    Dict.fromList
        [ -- case 0x57: ld_a_ir(IR>>>8); break;
          ( 0x57, ( \z80_main -> LoadAFromIR (z80_main.ir |> shiftRightBy8), NineTStates ) )

        --ld_a_r : Z80ROM -> Z80Core -> Z80Delta
        --ld_a_r rom48k z80 =
        --    --int r() {return R&0x7F | IR&0x80;}
        --    -- case 0x5F: ld_a_ir(r()); break;
        --    NewAValue z80.interrupts.r
        , ( 0x5F, ( \z80_main -> LoadAFromIR (Bitwise.or (z80_main.r |> Bitwise.and 0x7F) (z80_main.ir |> Bitwise.and 0x80)), NineTStates ) )
        ]
