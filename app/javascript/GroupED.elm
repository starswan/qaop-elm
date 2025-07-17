module GroupED exposing (..)

--  private void group_ed()
--  {
--    int v, c = env.m1(PC, IR|R++&0x7F);
--    PC = (char)(PC+1); time += 4;
--    switch(c) {

import Bitwise exposing (complement, shiftLeftBy, shiftRightBy)
import CpuTimeCTime exposing (InstructionDuration(..), addCpuTimeTime)
import Dict exposing (Dict)
import Keyboard exposing (Keyboard)
import PCIncrement exposing (PCIncrement(..))
import RegisterChange exposing (RegisterChange(..))
import Utils exposing (char, shiftLeftBy8, shiftRightBy8)
import Z80Change exposing (FlagChange(..), Z80Change(..))
import Z80Core exposing (Z80Core, add_cpu_time, imm16, inc_pc)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (Z80Env, addCpuTimeEnv, m1, mem, mem16, setMem, setMem16, z80_in)
import Z80Flags exposing (FlagRegisters, c_F3, c_F5, c_F53, c_FC, c_FH, f_szh0n0p, z80_sub)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), InterruptRegisters, MainWithIndexRegisters, get_bc, get_de, set_bc_main, set_de_main)


group_ed_dict : Dict Int (Z80ROM -> Z80Core -> Z80Delta)
group_ed_dict =
    Dict.fromList
        [ ( 0x40, execute_ED40 )
        , ( 0x42, execute_ED42 )
        , ( 0x43, execute_ED43 )

        --, ( 0x46, setImED46 )
        , ( 0x47, execute_ED47 )
        , ( 0x48, execute_ED48 )

        --, ( 0x4A, adc_hl_bc )
        , ( 0x4B, execute_ED4B )

        --, ( 0x4E, setImED4E )
        --, ( 0x4F, ld_r_a )
        , ( 0x50, execute_ED50 )

        --, ( 0x5A, adc_hl_de )
        --, ( 0x6A, adc_hl_hl )
        , ( 0x7A, adc_hl_sp )

        -- case 0x4F: r(A); time++; break;
        -- case 0x57: ld_a_ir(IR>>>8); break;
        , ( 0x57, ld_a_i )

        -- case 0x5F: ld_a_ir(r()); break;
        , ( 0x5F, ld_a_r )

        -- case 0x67: rrd(); break;
        -- case 0x6F: rld(); break;
        , ( 0x78, execute_ED78 )
        , ( 0x52, execute_ED52 )
        , ( 0x53, execute_ED53 )
        , ( 0x63, execute_ED63 )
        , ( 0x7B, execute_ED7B )
        , ( 0x73, execute_ED73 )
        , ( 0x5B, execute_ED5B )
        , ( 0x6B, execute_ED6B )
        , ( 0x72, execute_ED72 )

        --, ( 0x56, setIm0x56 )
        --, ( 0x5E, setIm0x5E )
        --, ( 0x66, setImED66 )
        , ( 0x67, rrd )
        , ( 0x6F, rld )

        --, ( 0x6E, setImED6E )
        --, ( 0x76, setImED76 )
        --, ( 0x7E, setImED7E )
        , ( 0x58, execute_ED58 )
        , ( 0x60, execute_ED60 )
        , ( 0x68, execute_ED68 )
        , ( 0x70, execute_ED70 )
        , ( 0x62, execute_ED62 )
        , ( 0xA0, ed_ldi )
        , ( 0xA1, ed_cpi )
        , ( 0xA8, ed_ldd )
        , ( 0xA9, ed_cpd )
        , ( 0xB0, ed_ldir )
        , ( 0xB1, ed_cpir )
        , ( 0xB8, ed_lddr )
        , ( 0xB9, ed_cpdr )

        -- ED08/ED09 no-op in Java version of Qaop
        , ( 0x00, \rom48k z80 -> NoOp )
        , ( 0x01, \rom48k z80 -> NoOp )
        , ( 0x04, \rom48k z80 -> NoOp )
        , ( 0x08, \rom48k z80 -> NoOp )
        , ( 0x09, \rom48k z80 -> NoOp )
        , ( 0x10, \rom48k z80 -> NoOp )
        , ( 0x11, \rom48k z80 -> NoOp )
        , ( 0x14, \rom48k z80 -> NoOp )
        , ( 0x18, \rom48k z80 -> NoOp )
        , ( 0x19, \rom48k z80 -> NoOp )
        , ( 0x20, \rom48k z80 -> NoOp )
        , ( 0x21, \rom48k z80 -> NoOp )
        , ( 0x24, \rom48k z80 -> NoOp )
        , ( 0x28, \rom48k z80 -> NoOp )
        , ( 0x29, \rom48k z80 -> NoOp )
        , ( 0x34, \rom48k z80 -> NoOp )
        , ( 0x38, \rom48k z80 -> NoOp )
        , ( 0x39, \rom48k z80 -> NoOp )

        -- case 0x41: env.out(B<<8|C,B); time+=4; break;
        , ( 0x41, \rom48k z80 -> NoOp )

        -- case 0x49: env.out(B<<8|C,C); time+=4; break;
        , ( 0x49, \rom48k z80 -> NoOp )

        -- case 0x51: env.out(B<<8|C,D); time+=4; break;
        , ( 0x51, \rom48k z80 -> NoOp )

        -- case 0x59: env.out(B<<8|C,E); time+=4; break;
        , ( 0x59, \rom48k z80 -> NoOp )

        -- case 0x61: env.out(B<<8|C,HL>>>8); time+=4; break;
        , ( 0x61, \rom48k z80 -> NoOp )

        -- case 0x69: env.out(B<<8|C,HL&0xFF); time+=4; break;
        , ( 0x69, \rom48k z80 -> NoOp )

        -- case 0x71: env.out(B<<8|C,0); time+=4; break;
        , ( 0x71, \rom48k z80 -> NoOp )

        -- case 0x79: MP=(v=B<<8|C)+1; env.out(v,A); time+=4; break;
        , ( 0x79, \rom48k z80 -> NoOp )

        -- case 0xA2:
        -- case 0xA3:
        -- case 0xAA:
        -- case 0xAB:
        -- case 0xB2:
        -- case 0xB3:
        -- case 0xBA:
        -- case 0xBB: inir_otir(c); break;
        -- TODO: implement outi etc
        , ( 0xA3, \rom48k z80 -> NoOp )
        , ( 0xAB, \rom48k z80 -> NoOp )
        , ( 0xB3, \rom48k z80 -> NoOp )
        , ( 0xBB, \rom48k z80 -> NoOp )

        -- RETN - end of NMI. NMIs aren't enabled on the Spectrum?
        , ( 0x45, \rom48k z80 -> NoOp )

        -- RETI (return from maskable interupt, unused on the Spectrum I'm pretty sure)
        , ( 0x4D, \rom48k z80 -> NoOp )

        -- case 0x7D: IFF|=IFF>>1; MP=PC=pop(); break;
        -- TODO: Implement ED 7D (all these are the same)
        , ( 0x55, \rom48k z80 -> NoOp )
        , ( 0x5D, \rom48k z80 -> NoOp )
        , ( 0x65, \rom48k z80 -> NoOp )
        , ( 0x6D, \rom48k z80 -> NoOp )
        , ( 0x75, \rom48k z80 -> NoOp )
        , ( 0x7D, \rom48k z80 -> NoOp )
        ]


execute_ED40 : Z80ROM -> Z80Core -> Z80Delta
execute_ED40 rom48k z80 =
    z80 |> execute_ED40485058606870 0x40 rom48k.keyboard


execute_ED42 : Z80ROM -> Z80Core -> Z80Delta
execute_ED42 rom48k z80 =
    -- case 0x42: sbc_hl(B<<8|C); break;
    let
        bc =
            z80.main |> get_bc
    in
    z80 |> sbc_hl bc


execute_ED62 : Z80ROM -> Z80Core -> Z80Delta
execute_ED62 rom48k z80 =
    -- case 0x62: sbc_hl(HL); break;
    z80 |> sbc_hl z80.main.hl


execute_ED43 : Z80ROM -> Z80Core -> Z80Delta
execute_ED43 rom48k z80 =
    -- case 0x43: MP=(v=imm16())+1; env.mem16(v,B<<8|C); time+=6; break;
    let
        v =
            z80 |> imm16 rom48k

        z80_2 =
            { z80 | pc = v.pc }

        env =
            z80_2.env |> setMem16 v.value16 (Bitwise.or (shiftLeftBy8 z80.main.b) z80.main.c)
    in
    EnvWithPc env v.pc



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


setImED46 : Z80ROM -> Z80Core -> Z80Delta
setImED46 rom48k z80 =
    --z80 |> set_im_value 0x46
    NoOp


setImED4E : Z80ROM -> Z80Core -> Z80Delta
setImED4E rom48k z80 =
    --z80 |> set_im_value 0x4E
    NoOp


execute_ED47 : Z80ROM -> Z80Core -> Z80Delta
execute_ED47 rom48k z80 =
    -- case 0x47: i(A); time++; break;
    --z80 |> set_i z80.flags.a |> add_cpu_time 1 |> Whole
    InterruptsWithCpuTime (z80 |> set_i z80.flags.a) (z80.env.time |> addCpuTimeTime 1)


execute_ED4B : Z80ROM -> Z80Core -> Z80Delta
execute_ED4B rom48k z80 =
    -- case 0x4B: MP=(v=imm16())+1; v=env.mem16(v); B=v>>>8; C=v&0xFF; time+=6; break;
    let
        v1 =
            z80 |> imm16 rom48k

        z80_1 =
            { z80 | pc = v1.pc }

        env =
            z80_1.env

        v2 =
            { env | time = v1.time } |> mem16 v1.value16 rom48k

        --x = debug_log "LD BC,(nnnn)" (v2.value |> toHexString) Nothing
    in
    --{ z80_1 | env = { env | time = v2.time } } |> set_bc v2.value |> add_cpu_time 6 |> Whole
    MainRegsWithPcAndCpuTime (z80.main |> set_bc_main v2.value16) v1.pc (v2.time |> addCpuTimeTime 6)



--ld_r_a : Z80ROM -> Z80Core -> Z80Delta
--ld_r_a rom48k z80 =
--    -- case 0x4F: r(A); time++; break;
--    NewRValue z80.flags.a


execute_ED52 : Z80ROM -> Z80Core -> Z80Delta
execute_ED52 rom48k z80 =
    -- case 0x52: sbc_hl(D<<8|E); break;
    z80 |> sbc_hl (Bitwise.or (shiftLeftBy8 z80.main.d) z80.main.e)


execute_ED53 : Z80ROM -> Z80Core -> Z80Delta
execute_ED53 rom48k z80 =
    -- case 0x53: MP=(v=imm16())+1; env.mem16(v,D<<8|E); time+=6; break;
    let
        v =
            z80 |> imm16 rom48k

        z80_1 =
            { z80 | pc = v.pc }

        env =
            z80_1.env |> setMem16 v.value16 (Bitwise.or (shiftLeftBy8 z80.main.d) z80.main.e)

        --env = case (v.value |> fromInt) of
        --  Z80Address.ROMAddress int -> z80_1.env
        --  Z80Address.RAMAddress ramAddress ->
        --    z80_1.env |> setMem16 ramAddress (Bitwise.or (shiftLeftBy8 z80.main.d) z80.main.e)
    in
    --{ z80_1 | env = env } |> add_cpu_time 6 |> Whole
    EnvWithPc (env |> addCpuTimeEnv 6) v.pc


execute_ED63 : Z80ROM -> Z80Core -> Z80Delta
execute_ED63 rom48k z80 =
    -- case 0x53: MP=(v=imm16())+1; env.mem16(v,D<<8|E); time+=6; break;
    let
        v =
            z80 |> imm16 rom48k

        z80_1 =
            { z80 | pc = v.pc }

        env =
            z80_1.env |> setMem16 v.value16 z80.main.hl

        --env = case (v.value |> fromInt) of
        --  Z80Address.ROMAddress int -> z80_1.env
        --  Z80Address.RAMAddress ramAddress ->
        --    z80_1.env |> setMem16 ramAddress (Bitwise.or (shiftLeftBy8 z80.main.d) z80.main.e)
    in
    --{ z80_1 | env = env } |> add_cpu_time 6 |> Whole
    EnvWithPc (env |> addCpuTimeEnv 6) v.pc


execute_ED5B : Z80ROM -> Z80Core -> Z80Delta
execute_ED5B rom48k z80 =
    -- case 0x5B: MP=(v=imm16())+1; v=env.mem16(v); D=v>>>8; E=v&0xFF; time+=6; break;
    let
        v1 =
            z80 |> imm16 rom48k

        --z80_1 = { z80 | pc = v1.pc }
        env =
            z80.env

        v2 =
            { env | time = v1.time } |> mem16 v1.value16 rom48k
    in
    --{ z80_1 | env = { env | time = v2.time } } |> set_de v2.value |> add_cpu_time 6 |> Whole
    MainRegsWithPcAndCpuTime (z80.main |> set_de_main v2.value16) v1.pc (v2.time |> addCpuTimeTime 6)


execute_ED6B : Z80ROM -> Z80Core -> Z80Delta
execute_ED6B rom48k z80 =
    -- case 0x6B: MP=(v=imm16())+1; HL=env.mem16(v); time+=6; break;
    let
        z80_main =
            z80.main

        v1 =
            z80 |> imm16 rom48k

        --z80_1 = { z80 | pc = v1.pc }
        env =
            z80.env

        v2 =
            { env | time = v1.time } |> mem16 v1.value16 rom48k
    in
    MainRegsWithPcAndCpuTime { z80_main | hl = v2.value16 } v1.pc (v2.time |> addCpuTimeTime 6)


execute_ED72 : Z80ROM -> Z80Core -> Z80Delta
execute_ED72 rom48k z80 =
    -- case 0x72: sbc_hl(SP); break;
    z80 |> sbc_hl z80.env.sp


execute_ED73 : Z80ROM -> Z80Core -> Z80Delta
execute_ED73 rom48k z80 =
    -- case 0x73: MP=(v=imm16())+1; env.mem16(v,SP); time+=6; break;
    let
        v =
            z80 |> imm16 rom48k

        z80_1 =
            { z80 | pc = v.pc }

        env =
            z80.env

        env2 =
            { env | time = v.time } |> setMem16 v.value16 z80_1.env.sp
    in
    --{ z80 | env = env2 } |> add_cpu_time 6 |> Whole
    EnvWithPc (env2 |> addCpuTimeEnv 6) v.pc


ld_a_i : Z80ROM -> Z80Core -> Z80Delta
ld_a_i rom48k z80 =
    -- case 0x57: ld_a_ir(IR>>>8); break;
    NewAValue (z80.interrupts.ir |> shiftRightBy8)


ld_a_r : Z80ROM -> Z80Core -> Z80Delta
ld_a_r rom48k z80 =
    -- case 0x5F: ld_a_ir(r()); break;
    NewAValue z80.r


execute_ED78 : Z80ROM -> Z80Core -> Z80Delta
execute_ED78 rom48k z80 =
    --  case 0x78: MP=(v=B<<8|C)+1; f_szh0n0p(A=env.in(v)); time+=4; break;
    let
        v =
            z80.main |> get_bc

        new_a =
            z80.env |> z80_in v rom48k.keyboard

        flags =
            z80.flags

        new_flags =
            { flags | a = new_a.value } |> f_szh0n0p new_a.value

        --env = z80.env
    in
    --{ z80 | env = { env | time = new_a.time |> add_cpu_time_time 4 } , flags = new_flags } |> Whole
    CpuTimeWithFlagsAndPc (new_a.time |> addCpuTimeTime 4) new_flags z80.pc


execute_ED7B : Z80ROM -> Z80Core -> Z80Delta
execute_ED7B rom48k z80 =
    -- case 0x7B: MP=(v=imm16())+1; SP=env.mem16(v); time+=6; break;
    let
        v =
            z80 |> imm16 rom48k

        z80_1 =
            { z80 | pc = v.pc }

        sp =
            z80_1.env |> mem16 v.value16 rom48k

        --env = z80_1.env
    in
    --{ z80_1 | env = { env | sp = sp.value, time = sp.time |> add_cpu_time_time 6 } } |> Whole
    CpuTimeWithSpAndPc (sp.time |> addCpuTimeTime 6) sp.value16 v.pc


ed_ldir : Z80ROM -> Z80Core -> Z80Delta
ed_ldir rom48k z80 =
    --0xB0 -> debug_log "LDIR" ("HL " ++ (z80.main.hl |> toHexString) ++ " BC " ++ (z80 |> get_bc |> toHexString2)) (z80 |> ldir 1 True)
    -- case 0xB0: ldir(1,true); break;
    z80 |> ldir Forwards True rom48k


ed_lddr : Z80ROM -> Z80Core -> Z80Delta
ed_lddr rom48k z80 =
    -- case 0xB8: ldir(-1,true); break;
    z80 |> ldir Backwards True rom48k


ed_cpdr : Z80ROM -> Z80Core -> Z80Delta
ed_cpdr rom48k z80 =
    -- case 0xB9: cpir(-1,true); break;
    z80 |> cpir Backwards True rom48k



-- case 0x46:
-- case 0x4E:
-- case 0x56:
-- case 0x5E:
-- case 0x66:
-- case 0x6E:
-- case 0x76:
-- case 0x7E: IM = c>>3&3; break;
--set_im_value : Int -> Z80Core -> Z80Delta
--set_im_value value z80 =
--    let
--        ed_value =
--            debugLog "set im ED" (value |> toHexString2) value
--
--        new_im =
--            ed_value |> shiftRightBy 3 |> Bitwise.and 0x03
--    in
--    z80 |> set_im_direct new_im
--setIm0x56 : Z80ROM -> Z80Core -> Z80Delta
--setIm0x56 _ z80 =
--    z80 |> set_im_value 0x56
--
--
--setIm0x5E : Z80ROM -> Z80Core -> Z80Delta
--setIm0x5E _ z80 =
--    --z80 |> set_im_value 0x5E
--    NoOp
--
--
--setImED66 : Z80ROM -> Z80Core -> Z80Delta
--setImED66 _ z80 =
--    --z80 |> set_im_value 0x66
--    NoOp
--
--
--setImED6E : Z80ROM -> Z80Core -> Z80Delta
--setImED6E _ z80 =
--    --z80 |> set_im_value 0x6E
--    NoOp
--
--
--setImED76 : Z80ROM -> Z80Core -> Z80Delta
--setImED76 _ z80 =
--    --z80 |> set_im_value 0x76
--    NoOp
--
--
--setImED7E : Z80ROM -> Z80Core -> Z80Delta
--setImED7E _ z80 =
--    --z80 |> set_im_value 0x7E
--    NoOp


execute_ED40485058606870 : Int -> Keyboard -> Z80Core -> Z80Delta
execute_ED40485058606870 value keyboard z80 =
    let
        bc =
            z80.main |> get_bc

        inval =
            z80.env |> z80_in bc keyboard

        --z80_1 =
        --    z80 |> set408bit (shiftRightBy 3 (value - 0x40)) inval.value HL
    in
    --{ z80_1 | flags = z80_1.flags |> f_szh0n0p inval.value } |> add_cpu_time 4 |> Whole
    Fszh0n0pTimeDeltaSet408Bit 4 (shiftRightBy 3 (value - 0x40)) inval.value


execute_ED48 : Z80ROM -> Z80Core -> Z80Delta
execute_ED48 rom48k z80 =
    z80 |> execute_ED40485058606870 0x48 rom48k.keyboard



--adc_hl_bc : Z80ROM -> Z80Core -> Z80Delta
--adc_hl_bc _ z80 =
--    -- case 0x4A: adc_hl(B<<8|C); break;
--    --0x4A -> z80 |> adc_hl (z80 |> get_bc)
--    z80 |> adc_hl (z80.main |> get_bc)
--adc_hl_de : Z80ROM -> Z80Core -> Z80Delta
--adc_hl_de _ z80 =
--    ---- case 0x5A: adc_hl(D<<8|E); break;
--    --0x5A -> z80 |> adc_hl (z80 |> get_de)
--    z80 |> adc_hl (z80.main |> get_de)
--
--
--adc_hl_hl : Z80ROM -> Z80Core -> Z80Delta
--adc_hl_hl _ z80 =
--    ---- case 0x6A: adc_hl(HL); break;
--    --0x6A -> z80 |> adc_hl z80.main.hl
--    z80 |> adc_hl z80.main.hl


adc_hl_sp : Z80ROM -> Z80Core -> Z80Delta
adc_hl_sp _ z80 =
    -- case 0x7A: adc_hl(SP); break;
    z80 |> adc_hl z80.env.sp


execute_ED50 : Z80ROM -> Z80Core -> Z80Delta
execute_ED50 rom48k z80 =
    z80 |> execute_ED40485058606870 0x50 rom48k.keyboard


execute_ED58 : Z80ROM -> Z80Core -> Z80Delta
execute_ED58 rom48k z80 =
    z80 |> execute_ED40485058606870 0x58 rom48k.keyboard


execute_ED60 : Z80ROM -> Z80Core -> Z80Delta
execute_ED60 rom48k z80 =
    z80 |> execute_ED40485058606870 0x60 rom48k.keyboard


execute_ED68 : Z80ROM -> Z80Core -> Z80Delta
execute_ED68 rom48k z80 =
    z80 |> execute_ED40485058606870 0x68 rom48k.keyboard


execute_ED70 : Z80ROM -> Z80Core -> Z80Delta
execute_ED70 rom48k z80 =
    z80 |> execute_ED40485058606870 0x70 rom48k.keyboard


group_ed : Z80ROM -> Z80Core -> Z80Delta
group_ed rom48k z80_0 =
    let
        --ints =
        --    z80_0.interrupts
        c =
            z80.env |> m1 z80_0.pc (Bitwise.or z80_0.interrupts.ir (Bitwise.and z80_0.r 0x7F)) rom48k

        new_r =
            z80_0.r + 1

        old_z80 =
            { z80_0 | r = new_r }

        new_pc =
            old_z80 |> inc_pc

        z80 =
            { old_z80 | pc = new_pc } |> add_cpu_time 4

        ed_func =
            group_ed_dict |> Dict.get c.value
    in
    case ed_func of
        Just f ->
            z80 |> f rom48k

        Nothing ->
            --// -------------- >8 ed
            ---- case 0x6A: adc_hl(HL); break;
            --0x6A -> z80 |> adc_hl z80.main.hl
            ---- case 0x5A: adc_hl(D<<8|E); break;
            --0x5A -> z80 |> adc_hl (z80 |> get_de)
            --else if List.member c.value [0x44, 0x4C, 0x54, 0x5C, 0x64, 0x6C, 0x74, 0x7C] then
            --   -- case 0x44:
            --   -- case 0x4C:
            --   -- case 0x54:
            --   -- case 0x5C:
            --   -- case 0x64:
            --   -- case 0x6C:
            --   -- case 0x74:
            --   -- case 0x7C: v=A; A=0; sub(v); break;
            --   let
            --      flags = z80.flags
            --      flags_1 = { flags | a = 0 } |> z80_sub flags.a
            --   in
            --      z80 |> set_flag_regs flags_1
            UnknownIntValue "group_ed" c.value



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


sbc_hl : Int -> Z80Core -> Z80Delta
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
    --{ z80 | main = { main | hl = r }, flags = { flags | ff = ff, fa = fa, fb = fb, fr = fr} } |> add_cpu_time 7 |> Whole
    FlagsWithPCMainAndCpuTime { flags | ff = ff, fa = fa, fb = fb, fr = fr } z80.pc { main | hl = r } (z80.env.time |> addCpuTimeTime 7)



--set_im_direct : Int -> Z80Core -> Z80Delta
--set_im_direct value z80 =
--    --{ z80 | interrupts = { ints | iM = value } } |> Whole
--    SetImValue value


type DirectionForLDIR
    = Forwards
    | Backwards


ldir : DirectionForLDIR -> Bool -> Z80ROM -> Z80Core -> Z80Delta
ldir incOrDec repeat rom48k z80 =
    --  private void ldir(int i, boolean r)
    let
        --v = env.mem(a = HL); HL = (char)(a+i); time += 3;
        --env.mem(a = de(), v); de((char)(a+i)); time += 5;
        main =
            z80.main

        a1 =
            z80.main.hl

        v1 =
            z80.env |> mem z80.main.hl z80.env.time rom48k

        z80_env =
            z80.env

        env_0 =
            { z80_env | time = v1.time }

        new_hl =
            case incOrDec of
                Forwards ->
                    a1 + 1 |> Bitwise.and 0xFFFF

                Backwards ->
                    a1 - 1 |> Bitwise.and 0xFFFF

        z80_1 =
            { z80 | env = env_0, main = { main | hl = new_hl } } |> add_cpu_time 3

        a2 =
            main |> get_de

        --env_1 = case a2 |> fromInt of
        --  ROMAddress int -> z80_1.env
        --  RAMAddress ramAddress ->
        --    z80_1.env |> setMem a2 v1.value
        env_1 =
            env_0 |> setMem a2 v1.value

        new_de =
            case incOrDec of
                Forwards ->
                    a2 + 1 |> char

                Backwards ->
                    a2 - 1 |> char

        z80_2 =
            { z80_1 | env = env_1, main = z80_1.main |> set_de_main new_de } |> add_cpu_time 5

        --if(Fr!=0) Fr = 1; // keep Z
        --v += A;
        --Ff = Ff&~F53 | v&F3 | v<<4&F5;
        fr =
            if z80_2.flags.fr /= 0 then
                1

            else
                z80_2.flags.fr

        v2 =
            v1.value + z80_2.flags.a

        ff =
            Bitwise.or (Bitwise.or (Bitwise.and z80_2.flags.ff (complement c_F53)) (Bitwise.and v2 c_F3)) (Bitwise.and (shiftLeftBy 4 v2) c_F5)

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
        a =
            Bitwise.and ((z80_2.main |> get_bc) - 1) 0xFFFF

        ( v, pc, time ) =
            if a /= 0 then
                if repeat then
                    ( 0x80, Bitwise.and (z80_2.pc - 2) 0xFFFF, 5 )

                else
                    ( 0x80, z80_2.pc, 0 )

            else
                ( 0, z80_2.pc, 0 )

        flags =
            z80_2.flags

        env_2 =
            z80_2.env |> addCpuTimeEnv time
    in
    { z80_2 | main = z80_2.main |> set_bc_main a, pc = pc, env = env_2, flags = { flags | fr = fr, ff = ff, fa = v, fb = v } } |> WholeCore



--  void i(int v) {IR = IR&0xFF | v<<8;}


set_i : Int -> Z80Core -> InterruptRegisters
set_i v z80 =
    let
        ir =
            Bitwise.or (Bitwise.and z80.interrupts.ir 0xFF) (shiftLeftBy8 v)

        interrupts =
            z80.interrupts
    in
    --{ z80 | interrupts = { interrupts | ir = ir } }
    { interrupts | ir = ir }



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
--ed_adc_hl : Int -> MainWithIndexRegisters -> FlagRegisters -> ( FlagRegisters, Int )
--ed_adc_hl b z80_main z80_flags =


adc_hl : Int -> Z80Core -> Z80Delta
adc_hl b z80 =
    let
        z80_main =
            z80.main

        ( flags, hl ) =
            ed_adc_hl b z80_main z80.flags
    in
    --{ z80 | main = { main | hl = r }, flags = { flags | ff = ff, fa = fa, fb = fb, fr = fr} } |> add_cpu_time 7
    --FlagsWithPCMainAndCpuTime { flags | ff = ff, fa = fa, fb = fb, fr = fr } z80.pc { main | hl = r } (z80.env.time |> addCpuTimeTime 7)
    FlagsWithPCMainAndCpuTime flags z80.pc { z80_main | hl = hl } (z80.env.time |> addCpuTimeTime 7)



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


cpir : DirectionForLDIR -> Bool -> Z80ROM -> Z80Core -> Z80Delta
cpir incOrDec repeat rom48k z80_core =
    let
        z80_flags =
            z80_core.flags

        old_a =
            z80_core.main.hl

        b =
            z80_core.env |> mem old_a z80_core.env.time rom48k

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
                            z80_core.pc - 2 |> Bitwise.and 0xFFFF

                        else
                            z80_core.pc
                in
                ( Bitwise.or 0x80 fa, Bitwise.or 0x80 fb, newish_pc )

            else
                ( fa, fb, z80_core.pc )

        old_ff =
            Bitwise.or (z80_core.flags.ff |> Bitwise.and 0xFF00) (v |> Bitwise.and (c_F53 |> complement))

        new_v =
            if (v |> Bitwise.xor b.value |> Bitwise.xor z80_core.flags.a |> Bitwise.and c_FH) /= 0 then
                v - 1

            else
                v

        ff =
            Bitwise.or old_ff ((new_v |> shiftLeftBy 4) |> Bitwise.and 0x20) |> Bitwise.or (Bitwise.and new_v 0x08)
    in
    HLBCWithFlagsAndPc hl new_bc { z80_flags | ff = ff, fa = new_fa, fb = new_fb, fr = fr } new_pc



--  private void rld()
--  {
--    int v = env.mem(HL)<<4 | A&0x0F;
--    time += 7;
--    f_szh0n0p(A = A&0xF0 | v>>>8);
--    env.mem(HL, v & 0xFF);
--    MP = HL+1;
--    time += 3;
--  }


rld : Z80ROM -> Z80Core -> Z80Delta
rld rom48k z80 =
    let
        v_lhs_1 =
            z80.env |> mem z80.main.hl z80.env.time rom48k

        v_rhs =
            Bitwise.and z80.flags.a 0x0F

        v_lhs =
            v_lhs_1.value |> shiftLeftBy 4

        v =
            Bitwise.or v_lhs v_rhs

        a1 =
            Bitwise.and z80.flags.a 0xF0

        new_a =
            Bitwise.or a1 (shiftRightBy8 v)

        flags =
            z80.flags

        new_flags =
            { flags | a = new_a } |> f_szh0n0p new_a

        --z80_1 =
        --    { z80 | flags = new_flags }
        env_0 =
            z80.env

        env_1 =
            { env_0 | time = v_lhs_1.time } |> setMem z80.main.hl (Bitwise.and v 0xFF)
    in
    --{ z80_1 | env = env_1 } |> add_cpu_time 10 |> Whole
    FlagsWithPcEnvAndCpuTime new_flags z80.pc env_1 10



--private void rrd()
--{
--  int v = env.mem(HL) | A<<8;
--  time += 7;
--  f_szh0n0p(A = A&0xF0 | v&0x0F);
--  env.mem(HL, v>>>4 & 0xFF);
--  MP = HL+1;
--  time += 3;
--}


rrd : Z80ROM -> Z80Core -> Z80Delta
rrd rom48k z80 =
    let
        v_lhs =
            z80.env |> mem z80.main.hl z80.env.time rom48k

        v_rhs =
            z80.flags.a |> shiftLeftBy8

        v =
            Bitwise.or v_lhs.value v_rhs

        a1 =
            Bitwise.and z80.flags.a 0xF0

        new_a =
            Bitwise.or a1 (v |> Bitwise.and 0x0F)

        flags =
            z80.flags

        new_flags =
            { flags | a = new_a } |> f_szh0n0p new_a

        --z80_1 =
        --    { z80 | flags = new_flags }
        env_0 =
            z80.env

        env_1 =
            { env_0 | time = v_lhs.time } |> setMem z80.main.hl (Bitwise.and (v |> shiftRightBy 4) 0xFF)
    in
    --{ z80_1 | env = env_1 } |> add_cpu_time 10 |> Whole
    FlagsWithPcEnvAndCpuTime new_flags z80.pc env_1 10


ed_ldi : Z80ROM -> Z80Core -> Z80Delta
ed_ldi rom48k z80 =
    -- case 0xA0: ldir(1,false); break;
    z80 |> ldir Forwards False rom48k


ed_cpi : Z80ROM -> Z80Core -> Z80Delta
ed_cpi rom48k z80 =
    -- case 0xA1: cpir(1,false); break;
    z80 |> cpir Forwards False rom48k


ed_ldd : Z80ROM -> Z80Core -> Z80Delta
ed_ldd rom48k z80 =
    -- case 0xA8: ldir(-1,false); break;
    z80 |> ldir Backwards False rom48k


ed_cpd : Z80ROM -> Z80Core -> Z80Delta
ed_cpd rom48k z80 =
    -- case 0xA9: cpir(-1,false); break;
    z80 |> cpir Backwards False rom48k


ed_cpir : Z80ROM -> Z80Core -> Z80Delta
ed_cpir rom48k z80 =
    -- case 0xB1: cpir(1,true); break;
    z80 |> cpir Forwards True rom48k


singleByteMainRegsED : Dict Int ( MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegsED =
    Dict.fromList
        [ --case 0x46:
          --case 0x4E:
          --case 0x56:
          --case 0x5E:
          --case 0x66:
          --case 0x6E:
          --case 0x76:
          --case 0x7E: IM = c>>3&3; break;
          ( 0x46, ( \_ -> RegChangeIm (0x46 |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        , ( 0x56, ( \_ -> RegChangeIm (0x56 |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        , ( 0x5E, ( \_ -> RegChangeIm (0x5E |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        , ( 0x66, ( \_ -> RegChangeIm (0x66 |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        , ( 0x6E, ( \_ -> RegChangeIm (0x6E |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        , ( 0x76, ( \_ -> RegChangeIm (0x76 |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        , ( 0x7E, ( \_ -> RegChangeIm (0x7E |> shiftRightBy 3 |> Bitwise.and 0x03), EightTStates ) )
        ]


singleByteFlagsED : Dict Int ( FlagRegisters -> FlagChange, InstructionDuration )
singleByteFlagsED =
    Dict.fromList
        [ ( 0x44, ( ed_44_neg, EightTStates ) )
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


ed_44_neg : FlagRegisters -> FlagChange
ed_44_neg z80_flags =
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

        flags =
            z80_flags
    in
    --FlagsWithHLRegister { flags | ff = ff, fa = fa, fb = fb, fr = fr } r
    ( { flags | ff = ff, fa = fa, fb = fb, fr = fr }, r )
