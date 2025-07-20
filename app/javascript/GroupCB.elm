module GroupCB exposing (..)

import Bitwise exposing (complement, shiftLeftBy, shiftRightBy)
import CpuTimeCTime exposing (CpuTimePcAnd16BitValue, CpuTimePcAndValue, InstructionDuration(..), addCpuTimeTime)
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..))
import RegisterChange exposing (ChangeOneRegister(..), RegisterChange(..), Shifter(..))
import SingleEnvWithMain exposing (SingleEnvMainChange(..))
import Utils exposing (BitTest(..), bitMaskFromBit, byte, char, inverseBitMaskFromBit, shiftLeftBy8, shiftRightBy8)
import Z80Change exposing (Z80Change(..))
import Z80Core exposing (Z80Core, inc_pc2, set408bitHL)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (Z80Env, addCpuTimeEnv, mem, setMem)
import Z80Flags exposing (FlagRegisters, IntWithFlags, bit, c_F53, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7, testBit)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IntWithFlagsTimeAndPC, MainWithIndexRegisters, get_ixiy_xy)



--
--  private void group_xy_cb(int xy)
--  {
--    int pc = PC;
--    int a = MP = (char)(xy + (byte)env.mem(pc));
--    time += 3;
--    int c = env.mem((char)(pc+1));
--    PC = (char)(pc+2);
--    time += 5;
--    int v = env.mem(a);
--    time += 4;
--    int o = c>>>3 & 7;


group_xy_cb : IXIY -> Z80ROM -> Z80Core -> Z80Delta
group_xy_cb ixiyhl rom48k z80 =
    let
        xy =
            get_ixiy_xy ixiyhl z80.main

        offset =
            z80.env |> mem z80.pc z80.env.time rom48k

        addr =
            char (xy + byte offset.value)

        env_1 =
            z80.env

        c =
            z80.env |> mem (char (z80.pc + 1)) (offset.time |> addCpuTimeTime 3) rom48k

        new_pc =
            z80 |> inc_pc2

        v1 =
            z80.env |> mem addr (c.time |> addCpuTimeTime 5) rom48k

        z80_3 =
            { z80 | pc = new_pc, env = { env_1 | time = v1.time |> addCpuTimeTime 4 } }

        o =
            Bitwise.and (shiftRightBy 3 c.value) 7

        cAndC0 =
            Bitwise.and c.value 0xC0

        --    switch(c&0xC0) {
        --      case 0x00: v = shifter(o, v); break;
        --      case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
        --      case 0x80: v &= ~(1<<o); break;
        --      case 0xC0: v |= 1<<o; break;
        --    }
        v2 =
            case cAndC0 of
                0x00 ->
                    let
                        flagRegs =
                            z80_3.flags

                        v_in =
                            v1.value
                    in
                    case Bitwise.and o 7 of
                        0 ->
                            flagRegs |> shifter0 v_in

                        1 ->
                            flagRegs |> shifter1 v_in

                        2 ->
                            flagRegs |> shifter2 v_in

                        3 ->
                            flagRegs |> shifter3 v_in

                        4 ->
                            flagRegs |> shifter4 v_in

                        5 ->
                            flagRegs |> shifter5 v_in

                        6 ->
                            flagRegs |> shifter6 v_in

                        _ ->
                            flagRegs |> shifter7 v_in

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
        --    env.mem(a, v);
        --    time += 3;
        z80_4 =
            { z80_3 | flags = v2.flags, env = new_env }

        --    switch(c&0x07) {
        --      case 0: B = v; break;
        --      case 1: C = v; break;
        --      case 2: D = v; break;
        --      case 3: E = v; break;
        --      case 4: HL = HL&0x00FF | v<<8; break;
        --      case 5: HL = HL&0xFF00 | v; break;
        --      case 7: A = v; break;
        --    }
        caseval =
            Bitwise.and c.value 0x07
    in
    if (caseval /= 6) && (cAndC0 /= 0x40) then
        let
            ( main, flags, env ) =
                set408bitHL caseval v2.value ( z80.main, z80.flags, z80.env )
        in
        { z80_4 | main = main, flags = flags, env = env } |> WholeCore

    else
        z80_4 |> WholeCore


singleEnvMainRegsCB : Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegsCB =
    Dict.fromList
        [ ( 0x46, ( bit_0_indirect_hl, TwelveTStates ) )
        , ( 0x4E, ( bit_1_indirect_hl, TwelveTStates ) )
        , ( 0x56, ( bit_2_indirect_hl, TwelveTStates ) )
        , ( 0x5E, ( bit_3_indirect_hl, TwelveTStates ) )
        , ( 0x66, ( bit_4_indirect_hl, TwelveTStates ) )
        , ( 0x6E, ( bit_5_indirect_hl, TwelveTStates ) )
        , ( 0x76, ( bit_6_indirect_hl, TwelveTStates ) )
        , ( 0x7E, ( bit_7_indirect_hl, TwelveTStates ) )
        ]


bit0indirectIx =
    List.range 0x40 0x47 |> List.map (\index -> ( index, ( bit_indirect_ix Bit_0, TwentyTStates ) )) |> Dict.fromList


bit1indirectIx =
    List.range 0x48 0x4F |> List.map (\index -> ( index, ( bit_indirect_ix Bit_1, TwentyTStates ) )) |> Dict.fromList


bit2indirectIx =
    List.range 0x50 0x57 |> List.map (\index -> ( index, ( bit_indirect_ix Bit_2, TwentyTStates ) )) |> Dict.fromList


bit3indirectIx =
    List.range 0x58 0x5F |> List.map (\index -> ( index, ( bit_indirect_ix Bit_3, TwentyTStates ) )) |> Dict.fromList


bit4indirectIx =
    List.range 0x60 0x67 |> List.map (\index -> ( index, ( bit_indirect_ix Bit_4, TwentyTStates ) )) |> Dict.fromList


bit5indirectIx =
    List.range 0x68 0x6F |> List.map (\index -> ( index, ( bit_indirect_ix Bit_5, TwentyTStates ) )) |> Dict.fromList


bit6indirectIx =
    List.range 0x70 0x77 |> List.map (\index -> ( index, ( bit_indirect_ix Bit_6, TwentyTStates ) )) |> Dict.fromList


bit7indirectIx =
    List.range 0x78 0x7F |> List.map (\index -> ( index, ( bit_indirect_ix Bit_7, TwentyTStates ) )) |> Dict.fromList


singleEnvMainRegsIXCB : Dict Int ( MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegsIXCB =
    bit0indirectIx
        |> Dict.union bit1indirectIx
        |> Dict.union bit2indirectIx
        |> Dict.union bit3indirectIx
        |> Dict.union bit4indirectIx
        |> Dict.union bit5indirectIx
        |> Dict.union bit6indirectIx
        |> Dict.union bit7indirectIx


bit0indirectIy : Dict Int ( MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange, InstructionDuration )
bit0indirectIy =
    List.range 0x40 0x47 |> List.map (\index -> ( index, ( bit_0_indirect_iy, TwentyTStates ) )) |> Dict.fromList


bit1indirectIy =
    List.range 0x48 0x4F |> List.map (\index -> ( index, ( bit_1_indirect_iy, TwentyTStates ) )) |> Dict.fromList


bit2indirectIy =
    List.range 0x50 0x57 |> List.map (\index -> ( index, ( bit_2_indirect_iy, TwentyTStates ) )) |> Dict.fromList


bit3indirectIy =
    List.range 0x58 0x5F |> List.map (\index -> ( index, ( bit_3_indirect_iy, TwentyTStates ) )) |> Dict.fromList


bit4indirectIy =
    List.range 0x60 0x67 |> List.map (\index -> ( index, ( bit_4_indirect_iy, TwentyTStates ) )) |> Dict.fromList


bit5indirectIy =
    List.range 0x68 0x6F |> List.map (\index -> ( index, ( bit_5_indirect_iy, TwentyTStates ) )) |> Dict.fromList


bit6indirectIy =
    List.range 0x70 0x77 |> List.map (\index -> ( index, ( bit_6_indirect_iy, TwentyTStates ) )) |> Dict.fromList


bit7indirectIy =
    List.range 0x78 0x7F |> List.map (\index -> ( index, ( bit_7_indirect_iy, TwentyTStates ) )) |> Dict.fromList


singleEnvMainRegsIYCB : Dict Int ( MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange, InstructionDuration )
singleEnvMainRegsIYCB =
    bit0indirectIy
        |> Dict.union bit1indirectIy
        |> Dict.union bit2indirectIy
        |> Dict.union bit3indirectIy
        |> Dict.union bit4indirectIy
        |> Dict.union bit5indirectIy
        |> Dict.union bit6indirectIy
        |> Dict.union bit7indirectIy


bit_0_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_0_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleBitTest Bit_0 value


bit_1_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_1_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleBitTest Bit_1 value


bit_2_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_2_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleBitTest Bit_2 value


bit_3_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_3_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleBitTest Bit_3 value


bit_4_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_4_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleBitTest Bit_4 value


bit_5_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_5_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleBitTest Bit_5 value


bit_6_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_6_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleBitTest Bit_6 value


bit_7_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_7_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            z80_env |> mem z80_main.hl z80_env.time rom48k
    in
    SingleBitTest Bit_7 value


singleByteMainRegsCB : Dict Int ( MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegsCB =
    Dict.fromList
        [ ( 0x06, ( rlc_indirect_hl, FifteenTStates ) )
        , ( 0x0E, ( rrc_indirect_hl, FifteenTStates ) )
        , ( 0x16, ( rl_indirect_hl, FifteenTStates ) )
        , ( 0x1E, ( rr_indirect_hl, FifteenTStates ) )
        , ( 0x26, ( sla_indirect_hl, FifteenTStates ) )
        , ( 0x2E, ( sra_indirect_hl, FifteenTStates ) )
        , ( 0x36, ( sll_indirect_hl, FifteenTStates ) )
        , ( 0x3E, ( srl_indirect_hl, FifteenTStates ) )

        -- reset bit0
        , ( 0x80, ( resetBbit Bit_0, EightTStates ) )
        , ( 0x81, ( resetCbit Bit_0, EightTStates ) )
        , ( 0x82, ( resetDbit Bit_0, EightTStates ) )
        , ( 0x83, ( resetEbit Bit_0, EightTStates ) )
        , ( 0x84, ( resetHbit Bit_0, EightTStates ) )
        , ( 0x85, ( resetLbit Bit_0, EightTStates ) )
        , ( 0x86, ( resetHLbit Bit_0, EightTStates ) )

        -- reset bit1
        , ( 0x88, ( resetBbit Bit_1, EightTStates ) )
        , ( 0x89, ( resetCbit Bit_1, EightTStates ) )
        , ( 0x8A, ( resetDbit Bit_1, EightTStates ) )
        , ( 0x8B, ( resetEbit Bit_1, EightTStates ) )
        , ( 0x8C, ( resetHbit Bit_1, EightTStates ) )
        , ( 0x8D, ( resetLbit Bit_1, EightTStates ) )
        , ( 0x8E, ( resetHLbit Bit_1, EightTStates ) )

        -- reset bit2
        , ( 0x90, ( resetBbit Bit_2, EightTStates ) )
        , ( 0x91, ( resetCbit Bit_2, EightTStates ) )
        , ( 0x92, ( resetDbit Bit_2, EightTStates ) )
        , ( 0x93, ( resetEbit Bit_2, EightTStates ) )
        , ( 0x94, ( resetHbit Bit_2, EightTStates ) )
        , ( 0x95, ( resetLbit Bit_2, EightTStates ) )
        , ( 0x96, ( resetHLbit Bit_2, EightTStates ) )

        -- reset bit3
        , ( 0x98, ( resetBbit Bit_3, EightTStates ) )
        , ( 0x99, ( resetCbit Bit_3, EightTStates ) )
        , ( 0x9A, ( resetDbit Bit_3, EightTStates ) )
        , ( 0x9B, ( resetEbit Bit_3, EightTStates ) )
        , ( 0x9C, ( resetHbit Bit_3, EightTStates ) )
        , ( 0x9D, ( resetLbit Bit_3, EightTStates ) )
        , ( 0x9E, ( resetHLbit Bit_3, EightTStates ) )

        -- reset bit4
        , ( 0xA0, ( resetBbit Bit_4, EightTStates ) )
        , ( 0xA1, ( resetCbit Bit_4, EightTStates ) )
        , ( 0xA2, ( resetDbit Bit_4, EightTStates ) )
        , ( 0xA3, ( resetEbit Bit_4, EightTStates ) )
        , ( 0xA4, ( resetHbit Bit_4, EightTStates ) )
        , ( 0xA5, ( resetLbit Bit_4, EightTStates ) )
        , ( 0xA6, ( resetHLbit Bit_4, EightTStates ) )

        -- reset bit5
        , ( 0xA8, ( resetBbit Bit_5, EightTStates ) )
        , ( 0xA9, ( resetCbit Bit_5, EightTStates ) )
        , ( 0xAA, ( resetDbit Bit_5, EightTStates ) )
        , ( 0xAB, ( resetEbit Bit_5, EightTStates ) )
        , ( 0xAC, ( resetHbit Bit_5, EightTStates ) )
        , ( 0xAD, ( resetLbit Bit_5, EightTStates ) )
        , ( 0xAE, ( resetHLbit Bit_5, EightTStates ) )

        -- reset bit6
        , ( 0xB0, ( resetBbit Bit_6, EightTStates ) )
        , ( 0xB1, ( resetCbit Bit_6, EightTStates ) )
        , ( 0xB2, ( resetDbit Bit_6, EightTStates ) )
        , ( 0xB3, ( resetEbit Bit_6, EightTStates ) )
        , ( 0xB4, ( resetHbit Bit_6, EightTStates ) )
        , ( 0xB5, ( resetLbit Bit_6, EightTStates ) )
        , ( 0xB6, ( resetHLbit Bit_6, EightTStates ) )

        -- reset bit7
        , ( 0xB8, ( resetBbit Bit_7, EightTStates ) )
        , ( 0xB9, ( resetCbit Bit_7, EightTStates ) )
        , ( 0xBA, ( resetDbit Bit_7, EightTStates ) )
        , ( 0xBB, ( resetEbit Bit_7, EightTStates ) )
        , ( 0xBC, ( resetHbit Bit_7, EightTStates ) )
        , ( 0xBD, ( resetLbit Bit_7, EightTStates ) )
        , ( 0xBE, ( resetHLbit Bit_7, EightTStates ) )

        -- set bit0
        , ( 0xC0, ( setBbit Bit_0, EightTStates ) )
        , ( 0xC1, ( setCbit Bit_0, EightTStates ) )
        , ( 0xC2, ( setDbit Bit_0, EightTStates ) )
        , ( 0xC3, ( setEbit Bit_0, EightTStates ) )
        , ( 0xC4, ( setHbit Bit_0, EightTStates ) )
        , ( 0xC5, ( setLbit Bit_0, EightTStates ) )
        , ( 0xC6, ( setHLbit Bit_0, EightTStates ) )

        -- set bit1
        , ( 0xC8, ( setBbit Bit_1, EightTStates ) )
        , ( 0xC9, ( setCbit Bit_1, EightTStates ) )
        , ( 0xCA, ( setDbit Bit_1, EightTStates ) )
        , ( 0xCB, ( setEbit Bit_1, EightTStates ) )
        , ( 0xCC, ( setHbit Bit_1, EightTStates ) )
        , ( 0xCD, ( setLbit Bit_1, EightTStates ) )
        , ( 0xCE, ( setHLbit Bit_1, EightTStates ) )

        -- set bit2
        , ( 0xD0, ( setBbit Bit_2, EightTStates ) )
        , ( 0xD1, ( setCbit Bit_2, EightTStates ) )
        , ( 0xD2, ( setDbit Bit_2, EightTStates ) )
        , ( 0xD3, ( setEbit Bit_2, EightTStates ) )
        , ( 0xD4, ( setHbit Bit_2, EightTStates ) )
        , ( 0xD5, ( setLbit Bit_2, EightTStates ) )
        , ( 0xD6, ( setHLbit Bit_2, EightTStates ) )

        -- set bDt3
        , ( 0xD8, ( setBbit Bit_3, EightTStates ) )
        , ( 0xD9, ( setCbit Bit_3, EightTStates ) )
        , ( 0xDA, ( setDbit Bit_3, EightTStates ) )
        , ( 0xDB, ( setEbit Bit_3, EightTStates ) )
        , ( 0xDC, ( setHbit Bit_3, EightTStates ) )
        , ( 0xDD, ( setLbit Bit_3, EightTStates ) )
        , ( 0xDE, ( setHLbit Bit_3, EightTStates ) )

        -- set bit4
        , ( 0xE0, ( setBbit Bit_4, EightTStates ) )
        , ( 0xE1, ( setCbit Bit_4, EightTStates ) )
        , ( 0xE2, ( setDbit Bit_4, EightTStates ) )
        , ( 0xE3, ( setEbit Bit_4, EightTStates ) )
        , ( 0xE4, ( setHbit Bit_4, EightTStates ) )
        , ( 0xE5, ( setLbit Bit_4, EightTStates ) )
        , ( 0xE6, ( setHLbit Bit_4, EightTStates ) )

        -- set bEt5
        , ( 0xE8, ( setBbit Bit_5, EightTStates ) )
        , ( 0xE9, ( setCbit Bit_5, EightTStates ) )
        , ( 0xEA, ( setDbit Bit_5, EightTStates ) )
        , ( 0xEB, ( setEbit Bit_5, EightTStates ) )
        , ( 0xEC, ( setHbit Bit_5, EightTStates ) )
        , ( 0xED, ( setLbit Bit_5, EightTStates ) )
        , ( 0xEE, ( setHLbit Bit_5, EightTStates ) )

        -- set bit6
        , ( 0xF0, ( setBbit Bit_6, EightTStates ) )
        , ( 0xF1, ( setCbit Bit_6, EightTStates ) )
        , ( 0xF2, ( setDbit Bit_6, EightTStates ) )
        , ( 0xF3, ( setEbit Bit_6, EightTStates ) )
        , ( 0xF4, ( setHbit Bit_6, EightTStates ) )
        , ( 0xF5, ( setLbit Bit_6, EightTStates ) )
        , ( 0xF6, ( setHLbit Bit_6, EightTStates ) )

        -- set bFt7
        , ( 0xF8, ( setBbit Bit_7, EightTStates ) )
        , ( 0xF9, ( setCbit Bit_7, EightTStates ) )
        , ( 0xFA, ( setDbit Bit_7, EightTStates ) )
        , ( 0xFB, ( setEbit Bit_7, EightTStates ) )
        , ( 0xFC, ( setHbit Bit_7, EightTStates ) )
        , ( 0xFD, ( setLbit Bit_7, EightTStates ) )
        , ( 0xFE, ( setHLbit Bit_7, EightTStates ) )
        ]


singleByteMainRegsIXCB : Dict Int ( Int -> MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegsIXCB =
    Dict.fromList
        [ ( 0x06, ( \offset z80_main -> RegisterChangeShifter Shifter0 ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x0E, ( \offset z80_main -> RegisterChangeShifter Shifter1 ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x16, ( \offset z80_main -> RegisterChangeShifter Shifter2 ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x1E, ( \offset z80_main -> RegisterChangeShifter Shifter3 ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x26, ( \offset z80_main -> RegisterChangeShifter Shifter4 ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x2E, ( \offset z80_main -> RegisterChangeShifter Shifter5 ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x36, ( \offset z80_main -> RegisterChangeShifter Shifter6 ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x3E, ( \offset z80_main -> RegisterChangeShifter Shifter7 ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )

        -- reset bit0
        --, ( 0x80, ( resetBbit Bit_0, EightTStates ) )
        --, ( 0x81, ( resetCbit Bit_0, EightTStates ) )
        --, ( 0x82, ( resetDbit Bit_0, EightTStates ) )
        --, ( 0x83, ( resetEbit Bit_0, EightTStates ) )
        --, ( 0x84, ( resetHbit Bit_0, EightTStates ) )
        --, ( 0x85, ( resetLbit Bit_0, EightTStates ) )
        , ( 0x86, ( resetIXbit Bit_0, EightTStates ) )

        -- reset bit1
        --, ( 0x88, ( resetBbit Bit_1, EightTStates ) )
        --, ( 0x89, ( resetCbit Bit_1, EightTStates ) )
        --, ( 0x8A, ( resetDbit Bit_1, EightTStates ) )
        --, ( 0x8B, ( resetEbit Bit_1, EightTStates ) )
        --, ( 0x8C, ( resetHbit Bit_1, EightTStates ) )
        --, ( 0x8D, ( resetLbit Bit_1, EightTStates ) )
        , ( 0x8E, ( resetIXbit Bit_1, EightTStates ) )

        -- reset bit2
        --, ( 0x90, ( resetBbit Bit_2, EightTStates ) )
        --, ( 0x91, ( resetCbit Bit_2, EightTStates ) )
        --, ( 0x92, ( resetDbit Bit_2, EightTStates ) )
        --, ( 0x93, ( resetEbit Bit_2, EightTStates ) )
        --, ( 0x94, ( resetHbit Bit_2, EightTStates ) )
        --, ( 0x95, ( resetLbit Bit_2, EightTStates ) )
        , ( 0x96, ( resetIXbit Bit_2, EightTStates ) )

        -- reset bit3
        --, ( 0x98, ( resetBbit Bit_3, EightTStates ) )
        --, ( 0x99, ( resetCbit Bit_3, EightTStates ) )
        --, ( 0x9A, ( resetDbit Bit_3, EightTStates ) )
        --, ( 0x9B, ( resetEbit Bit_3, EightTStates ) )
        --, ( 0x9C, ( resetHbit Bit_3, EightTStates ) )
        --, ( 0x9D, ( resetLbit Bit_3, EightTStates ) )
        , ( 0x9E, ( resetIXbit Bit_3, EightTStates ) )

        -- reset bit4
        --, ( 0xA0, ( resetBbit Bit_4, EightTStates ) )
        --, ( 0xA1, ( resetCbit Bit_4, EightTStates ) )
        --, ( 0xA2, ( resetDbit Bit_4, EightTStates ) )
        --, ( 0xA3, ( resetEbit Bit_4, EightTStates ) )
        --, ( 0xA4, ( resetHbit Bit_4, EightTStates ) )
        --, ( 0xA5, ( resetLbit Bit_4, EightTStates ) )
        , ( 0xA6, ( resetIXbit Bit_4, EightTStates ) )

        -- reset bit5
        --, ( 0xA8, ( resetBbit Bit_5, EightTStates ) )
        --, ( 0xA9, ( resetCbit Bit_5, EightTStates ) )
        --, ( 0xAA, ( resetDbit Bit_5, EightTStates ) )
        --, ( 0xAB, ( resetEbit Bit_5, EightTStates ) )
        --, ( 0xAC, ( resetHbit Bit_5, EightTStates ) )
        --, ( 0xAD, ( resetLbit Bit_5, EightTStates ) )
        , ( 0xAE, ( resetIXbit Bit_5, EightTStates ) )

        -- reset bit6
        --, ( 0xB0, ( resetBbit Bit_6, EightTStates ) )
        --, ( 0xB1, ( resetCbit Bit_6, EightTStates ) )
        --, ( 0xB2, ( resetDbit Bit_6, EightTStates ) )
        --, ( 0xB3, ( resetEbit Bit_6, EightTStates ) )
        --, ( 0xB4, ( resetHbit Bit_6, EightTStates ) )
        --, ( 0xB5, ( resetLbit Bit_6, EightTStates ) )
        , ( 0xB6, ( resetIXbit Bit_6, EightTStates ) )

        -- reset bit7
        --, ( 0xB8, ( resetBbit Bit_7, EightTStates ) )
        --, ( 0xB9, ( resetCbit Bit_7, EightTStates ) )
        --, ( 0xBA, ( resetDbit Bit_7, EightTStates ) )
        --, ( 0xBB, ( resetEbit Bit_7, EightTStates ) )
        --, ( 0xBC, ( resetHbit Bit_7, EightTStates ) )
        --, ( 0xBD, ( resetLbit Bit_7, EightTStates ) )
        , ( 0xBE, ( resetIXbit Bit_7, EightTStates ) )

        -- set bit0
        --, ( 0xC0, ( setBbit Bit_0, EightTStates ) )
        --, ( 0xC1, ( setCbit Bit_0, EightTStates ) )
        --, ( 0xC2, ( setDbit Bit_0, EightTStates ) )
        --, ( 0xC3, ( setEbit Bit_0, EightTStates ) )
        --, ( 0xC4, ( setHbit Bit_0, EightTStates ) )
        --, ( 0xC5, ( setLbit Bit_0, EightTStates ) )
        , ( 0xC6, ( setIXbit Bit_0, EightTStates ) )

        -- set bit1
        --, ( 0xC8, ( setBbit Bit_1, EightTStates ) )
        --, ( 0xC9, ( setCbit Bit_1, EightTStates ) )
        --, ( 0xCA, ( setDbit Bit_1, EightTStates ) )
        --, ( 0xCB, ( setEbit Bit_1, EightTStates ) )
        --, ( 0xCC, ( setHbit Bit_1, EightTStates ) )
        --, ( 0xCD, ( setLbit Bit_1, EightTStates ) )
        , ( 0xCE, ( setIXbit Bit_1, EightTStates ) )

        -- set bit2
        --, ( 0xD0, ( setBbit Bit_2, EightTStates ) )
        --, ( 0xD1, ( setCbit Bit_2, EightTStates ) )
        --, ( 0xD2, ( setDbit Bit_2, EightTStates ) )
        --, ( 0xD3, ( setEbit Bit_2, EightTStates ) )
        --, ( 0xD4, ( setHbit Bit_2, EightTStates ) )
        --, ( 0xD5, ( setLbit Bit_2, EightTStates ) )
        , ( 0xD6, ( setIXbit Bit_2, EightTStates ) )

        -- set bit3
        --, ( 0xD8, ( setBbit Bit_3, EightTStates ) )
        --, ( 0xD9, ( setCbit Bit_3, EightTStates ) )
        --, ( 0xDA, ( setDbit Bit_3, EightTStates ) )
        --, ( 0xDB, ( setEbit Bit_3, EightTStates ) )
        --, ( 0xDC, ( setHbit Bit_3, EightTStates ) )
        --, ( 0xDD, ( setLbit Bit_3, EightTStates ) )
        , ( 0xDE, ( setIXbit Bit_3, EightTStates ) )

        --
        ---- set bit4
        --, ( 0xE0, ( setBbit Bit_4, EightTStates ) )
        --, ( 0xE1, ( setCbit Bit_4, EightTStates ) )
        --, ( 0xE2, ( setDbit Bit_4, EightTStates ) )
        --, ( 0xE3, ( setEbit Bit_4, EightTStates ) )
        --, ( 0xE4, ( setHbit Bit_4, EightTStates ) )
        --, ( 0xE5, ( setLbit Bit_4, EightTStates ) )
        , ( 0xE6, ( setIXbit Bit_4, EightTStates ) )

        -- set bit5
        --, ( 0xE8, ( setBbit Bit_5, EightTStates ) )
        --, ( 0xE9, ( setCbit Bit_5, EightTStates ) )
        --, ( 0xEA, ( setDbit Bit_5, EightTStates ) )
        --, ( 0xEB, ( setEbit Bit_5, EightTStates ) )
        --, ( 0xEC, ( setHbit Bit_5, EightTStates ) )
        --, ( 0xED, ( setLbit Bit_5, EightTStates ) )
        , ( 0xEE, ( setIXbit Bit_5, EightTStates ) )

        -- set bit6
        --, ( 0xF0, ( setBbit Bit_6, EightTStates ) )
        --, ( 0xF1, ( setCbit Bit_6, EightTStates ) )
        --, ( 0xF2, ( setDbit Bit_6, EightTStates ) )
        --, ( 0xF3, ( setEbit Bit_6, EightTStates ) )
        --, ( 0xF4, ( setHbit Bit_6, EightTStates ) )
        --, ( 0xF5, ( setLbit Bit_6, EightTStates ) )
        , ( 0xF6, ( setIXbit Bit_6, EightTStates ) )

        -- set bit7
        --, ( 0xF8, ( setBbit Bit_7, EightTStates ) )
        --, ( 0xF9, ( setCbit Bit_7, EightTStates ) )
        --, ( 0xFA, ( setDbit Bit_7, EightTStates ) )
        --, ( 0xFB, ( setEbit Bit_7, EightTStates ) )
        --, ( 0xFC, ( setHbit Bit_7, EightTStates ) )
        --, ( 0xFD, ( setLbit Bit_7, EightTStates ) )
        , ( 0xFE, ( setIXbit Bit_7, EightTStates ) )
        ]


singleByteMainRegsIYCB : Dict Int ( Int -> MainWithIndexRegisters -> RegisterChange, InstructionDuration )
singleByteMainRegsIYCB =
    Dict.fromList
        [ ( 0x06, ( \offset z80_main -> RegisterChangeShifter Shifter0 ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x0E, ( \offset z80_main -> RegisterChangeShifter Shifter1 ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x16, ( \offset z80_main -> RegisterChangeShifter Shifter2 ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x1E, ( \offset z80_main -> RegisterChangeShifter Shifter3 ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x26, ( \offset z80_main -> RegisterChangeShifter Shifter4 ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x2E, ( \offset z80_main -> RegisterChangeShifter Shifter5 ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x36, ( \offset z80_main -> RegisterChangeShifter Shifter6 ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )
        , ( 0x3E, ( \offset z80_main -> RegisterChangeShifter Shifter7 ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF), FifteenTStates ) )

        -- reset bit0
        --, ( 0x80, ( resetBbit Bit_0, EightTStates ) )
        --, ( 0x81, ( resetCbit Bit_0, EightTStates ) )
        --, ( 0x82, ( resetDbit Bit_0, EightTStates ) )
        --, ( 0x83, ( resetEbit Bit_0, EightTStates ) )
        --, ( 0x84, ( resetHbit Bit_0, EightTStates ) )
        --, ( 0x85, ( resetLbit Bit_0, EightTStates ) )
        , ( 0x86, ( resetIYbit Bit_0, EightTStates ) )

        -- reset bit1
        --, ( 0x88, ( resetBbit Bit_1, EightTStates ) )
        --, ( 0x89, ( resetCbit Bit_1, EightTStates ) )
        --, ( 0x8A, ( resetDbit Bit_1, EightTStates ) )
        --, ( 0x8B, ( resetEbit Bit_1, EightTStates ) )
        --, ( 0x8C, ( resetHbit Bit_1, EightTStates ) )
        --, ( 0x8D, ( resetLbit Bit_1, EightTStates ) )
        , ( 0x8E, ( resetIYbit Bit_1, EightTStates ) )

        -- reset bit2
        --, ( 0x90, ( resetBbit Bit_2, EightTStates ) )
        --, ( 0x91, ( resetCbit Bit_2, EightTStates ) )
        --, ( 0x92, ( resetDbit Bit_2, EightTStates ) )
        --, ( 0x93, ( resetEbit Bit_2, EightTStates ) )
        --, ( 0x94, ( resetHbit Bit_2, EightTStates ) )
        --, ( 0x95, ( resetLbit Bit_2, EightTStates ) )
        , ( 0x96, ( resetIYbit Bit_2, EightTStates ) )

        -- reset bit3
        --, ( 0x98, ( resetBbit Bit_3, EightTStates ) )
        --, ( 0x99, ( resetCbit Bit_3, EightTStates ) )
        --, ( 0x9A, ( resetDbit Bit_3, EightTStates ) )
        --, ( 0x9B, ( resetEbit Bit_3, EightTStates ) )
        --, ( 0x9C, ( resetHbit Bit_3, EightTStates ) )
        --, ( 0x9D, ( resetLbit Bit_3, EightTStates ) )
        , ( 0x9E, ( resetIYbit Bit_3, EightTStates ) )

        -- reset bit4
        --, ( 0xA0, ( resetBbit Bit_4, EightTStates ) )
        --, ( 0xA1, ( resetCbit Bit_4, EightTStates ) )
        --, ( 0xA2, ( resetDbit Bit_4, EightTStates ) )
        --, ( 0xA3, ( resetEbit Bit_4, EightTStates ) )
        --, ( 0xA4, ( resetHbit Bit_4, EightTStates ) )
        --, ( 0xA5, ( resetLbit Bit_4, EightTStates ) )
        , ( 0xA6, ( resetIYbit Bit_4, EightTStates ) )

        -- reset bit5
        --, ( 0xA8, ( resetBbit Bit_5, EightTStates ) )
        --, ( 0xA9, ( resetCbit Bit_5, EightTStates ) )
        --, ( 0xAA, ( resetDbit Bit_5, EightTStates ) )
        --, ( 0xAB, ( resetEbit Bit_5, EightTStates ) )
        --, ( 0xAC, ( resetHbit Bit_5, EightTStates ) )
        --, ( 0xAD, ( resetLbit Bit_5, EightTStates ) )
        , ( 0xAE, ( resetIYbit Bit_5, EightTStates ) )

        -- reset bit6
        --, ( 0xB0, ( resetBbit Bit_6, EightTStates ) )
        --, ( 0xB1, ( resetCbit Bit_6, EightTStates ) )
        --, ( 0xB2, ( resetDbit Bit_6, EightTStates ) )
        --, ( 0xB3, ( resetEbit Bit_6, EightTStates ) )
        --, ( 0xB4, ( resetHbit Bit_6, EightTStates ) )
        --, ( 0xB5, ( resetLbit Bit_6, EightTStates ) )
        , ( 0xB6, ( resetIYbit Bit_6, EightTStates ) )

        -- reset bit7
        --, ( 0xB8, ( resetBbit Bit_7, EightTStates ) )
        --, ( 0xB9, ( resetCbit Bit_7, EightTStates ) )
        --, ( 0xBA, ( resetDbit Bit_7, EightTStates ) )
        --, ( 0xBB, ( resetEbit Bit_7, EightTStates ) )
        --, ( 0xBC, ( resetHbit Bit_7, EightTStates ) )
        --, ( 0xBD, ( resetLbit Bit_7, EightTStates ) )
        , ( 0xBE, ( resetIYbit Bit_7, EightTStates ) )

        -- set bit0
        --, ( 0xC0, ( setBbit Bit_0, EightTStates ) )
        --, ( 0xC1, ( setCbit Bit_0, EightTStates ) )
        --, ( 0xC2, ( setDbit Bit_0, EightTStates ) )
        --, ( 0xC3, ( setEbit Bit_0, EightTStates ) )
        --, ( 0xC4, ( setHbit Bit_0, EightTStates ) )
        --, ( 0xC5, ( setLbit Bit_0, EightTStates ) )
        , ( 0xC6, ( setIYbit Bit_0, EightTStates ) )

        -- set bit1
        --, ( 0xC8, ( setBbit Bit_1, EightTStates ) )
        --, ( 0xC9, ( setCbit Bit_1, EightTStates ) )
        --, ( 0xCA, ( setDbit Bit_1, EightTStates ) )
        --, ( 0xCB, ( setEbit Bit_1, EightTStates ) )
        --, ( 0xCC, ( setHbit Bit_1, EightTStates ) )
        --, ( 0xCD, ( setLbit Bit_1, EightTStates ) )
        , ( 0xCE, ( setIYbit Bit_1, EightTStates ) )

        -- set bit2
        --, ( 0xD0, ( setBbit Bit_2, EightTStates ) )
        --, ( 0xD1, ( setCbit Bit_2, EightTStates ) )
        --, ( 0xD2, ( setDbit Bit_2, EightTStates ) )
        --, ( 0xD3, ( setEbit Bit_2, EightTStates ) )
        --, ( 0xD4, ( setHbit Bit_2, EightTStates ) )
        --, ( 0xD5, ( setLbit Bit_2, EightTStates ) )
        , ( 0xD6, ( setIYbit Bit_2, EightTStates ) )

        -- set bDt3
        --, ( 0xD8, ( setBbit Bit_3, EightTStates ) )
        --, ( 0xD9, ( setCbit Bit_3, EightTStates ) )
        --, ( 0xDA, ( setDbit Bit_3, EightTStates ) )
        --, ( 0xDB, ( setEbit Bit_3, EightTStates ) )
        --, ( 0xDC, ( setHbit Bit_3, EightTStates ) )
        --, ( 0xDD, ( setLbit Bit_3, EightTStates ) )
        , ( 0xDE, ( setIYbit Bit_3, EightTStates ) )

        -- set bit4
        --, ( 0xE0, ( setBbit Bit_4, EightTStates ) )
        --, ( 0xE1, ( setCbit Bit_4, EightTStates ) )
        --, ( 0xE2, ( setDbit Bit_4, EightTStates ) )
        --, ( 0xE3, ( setEbit Bit_4, EightTStates ) )
        --, ( 0xE4, ( setHbit Bit_4, EightTStates ) )
        --, ( 0xE5, ( setLbit Bit_4, EightTStates ) )
        , ( 0xE6, ( setIYbit Bit_4, EightTStates ) )

        -- set bEt5
        --, ( 0xE8, ( setBbit Bit_5, EightTStates ) )
        --, ( 0xE9, ( setCbit Bit_5, EightTStates ) )
        --, ( 0xEA, ( setDbit Bit_5, EightTStates ) )
        --, ( 0xEB, ( setEbit Bit_5, EightTStates ) )
        --, ( 0xEC, ( setHbit Bit_5, EightTStates ) )
        --, ( 0xED, ( setLbit Bit_5, EightTStates ) )
        , ( 0xEE, ( setIYbit Bit_5, EightTStates ) )

        -- set bit6
        --, ( 0xF0, ( setBbit Bit_6, EightTStates ) )
        --, ( 0xF1, ( setCbit Bit_6, EightTStates ) )
        --, ( 0xF2, ( setDbit Bit_6, EightTStates ) )
        --, ( 0xF3, ( setEbit Bit_6, EightTStates ) )
        --, ( 0xF4, ( setHbit Bit_6, EightTStates ) )
        --, ( 0xF5, ( setLbit Bit_6, EightTStates ) )
        , ( 0xF6, ( setIYbit Bit_6, EightTStates ) )

        -- set bFt7
        --, ( 0xF8, ( setBbit Bit_7, EightTStates ) )
        --, ( 0xF9, ( setCbit Bit_7, EightTStates ) )
        --, ( 0xFA, ( setDbit Bit_7, EightTStates ) )
        --, ( 0xFB, ( setEbit Bit_7, EightTStates ) )
        --, ( 0xFC, ( setHbit Bit_7, EightTStates ) )
        --, ( 0xFD, ( setLbit Bit_7, EightTStates ) )
        , ( 0xFE, ( setIYbit Bit_7, EightTStates ) )
        ]


rlc_indirect_hl : MainWithIndexRegisters -> RegisterChange
rlc_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter0 z80_main.hl


rrc_indirect_hl : MainWithIndexRegisters -> RegisterChange
rrc_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter1 z80_main.hl


rl_indirect_hl : MainWithIndexRegisters -> RegisterChange
rl_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter2 z80_main.hl


rr_indirect_hl : MainWithIndexRegisters -> RegisterChange
rr_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter3 z80_main.hl


sla_indirect_hl : MainWithIndexRegisters -> RegisterChange
sla_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter4 z80_main.hl


sra_indirect_hl : MainWithIndexRegisters -> RegisterChange
sra_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter5 z80_main.hl


sll_indirect_hl : MainWithIndexRegisters -> RegisterChange
sll_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter6 z80_main.hl


srl_indirect_hl : MainWithIndexRegisters -> RegisterChange
srl_indirect_hl z80_main =
    -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    RegisterChangeShifter Shifter7 z80_main.hl


resetBbit : BitTest -> MainWithIndexRegisters -> RegisterChange
resetBbit bitMask z80_main =
    --Bitwise.and raw.value (1 |> shiftLeftBy o |> complement)
    -- case 0x80: B=B&~(1<<o); break;
    SingleRegisterChange AlterRegisterB (bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.b)


resetCbit : BitTest -> MainWithIndexRegisters -> RegisterChange
resetCbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange AlterRegisterC (bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.c)


resetDbit : BitTest -> MainWithIndexRegisters -> RegisterChange
resetDbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange AlterRegisterD (bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.d)


resetEbit : BitTest -> MainWithIndexRegisters -> RegisterChange
resetEbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange ChangeRegisterE (bitMask |> inverseBitMaskFromBit |> Bitwise.and z80_main.e)


resetHbit : BitTest -> MainWithIndexRegisters -> RegisterChange
resetHbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange ChangeRegisterH (bitMask |> inverseBitMaskFromBit |> Bitwise.and (z80_main.hl |> shiftRightBy8))


resetLbit : BitTest -> MainWithIndexRegisters -> RegisterChange
resetLbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange ChangeRegisterL (bitMask |> inverseBitMaskFromBit |> Bitwise.and (z80_main.hl |> Bitwise.and 0xFF))


resetHLbit : BitTest -> MainWithIndexRegisters -> RegisterChange
resetHLbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitReset bitMask z80_main.hl


resetIXbit : BitTest -> Int -> MainWithIndexRegisters -> RegisterChange
resetIXbit bitMask offset z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitReset bitMask ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF)


resetIYbit : BitTest -> Int -> MainWithIndexRegisters -> RegisterChange
resetIYbit bitMask offset z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitReset bitMask ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF)


setBbit : BitTest -> MainWithIndexRegisters -> RegisterChange
setBbit bitMask z80_main =
    --Bitwise.and raw.value (1 |> shiftLeftBy o |> complement)
    -- case 0x80: B=B&~(1<<o); break;
    SingleRegisterChange AlterRegisterB (bitMask |> bitMaskFromBit |> Bitwise.or z80_main.b)


setCbit : BitTest -> MainWithIndexRegisters -> RegisterChange
setCbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange AlterRegisterC (bitMask |> bitMaskFromBit |> Bitwise.or z80_main.c)


setDbit : BitTest -> MainWithIndexRegisters -> RegisterChange
setDbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange AlterRegisterD (bitMask |> bitMaskFromBit |> Bitwise.or z80_main.d)


setEbit : BitTest -> MainWithIndexRegisters -> RegisterChange
setEbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange ChangeRegisterE (bitMask |> bitMaskFromBit |> Bitwise.or z80_main.e)


setHbit : BitTest -> MainWithIndexRegisters -> RegisterChange
setHbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange ChangeRegisterH (bitMask |> bitMaskFromBit |> Bitwise.or (z80_main.hl |> shiftRightBy8))


setLbit : BitTest -> MainWithIndexRegisters -> RegisterChange
setLbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    SingleRegisterChange ChangeRegisterL (bitMask |> bitMaskFromBit |> Bitwise.or (z80_main.hl |> Bitwise.and 0xFF))


setHLbit : BitTest -> MainWithIndexRegisters -> RegisterChange
setHLbit bitMask z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitSet bitMask z80_main.hl


setIXbit : BitTest -> Int -> MainWithIndexRegisters -> RegisterChange
setIXbit bitMask offset z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitSet bitMask ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF)


setIYbit : BitTest -> Int -> MainWithIndexRegisters -> RegisterChange
setIYbit bitMask offset z80_main =
    -- case 0x81: C=C&~(1<<o); break;
    IndirectBitSet bitMask ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF)


singleByteMainAndFlagRegistersCB : Dict Int ( MainWithIndexRegisters -> FlagRegisters -> Z80Change, InstructionDuration )
singleByteMainAndFlagRegistersCB =
    Dict.fromList
        [ ( 0x00, ( rlc_b, EightTStates ) )
        , ( 0x01, ( rlc_c, EightTStates ) )
        , ( 0x02, ( rlc_d, EightTStates ) )
        , ( 0x03, ( rlc_e, EightTStates ) )
        , ( 0x04, ( rlc_h, EightTStates ) )
        , ( 0x05, ( rlc_l, EightTStates ) )
        , ( 0x08, ( rrc_b, EightTStates ) )
        , ( 0x09, ( rrc_c, EightTStates ) )
        , ( 0x0A, ( rrc_d, EightTStates ) )
        , ( 0x0B, ( rrc_e, EightTStates ) )
        , ( 0x0C, ( rrc_h, EightTStates ) )
        , ( 0x0D, ( rrc_l, EightTStates ) )
        , ( 0x10, ( rl_b, EightTStates ) )
        , ( 0x11, ( rl_c, EightTStates ) )
        , ( 0x12, ( rl_d, EightTStates ) )
        , ( 0x13, ( rl_e, EightTStates ) )
        , ( 0x14, ( rl_h, EightTStates ) )
        , ( 0x15, ( rl_l, EightTStates ) )
        , ( 0x18, ( rr_b, EightTStates ) )
        , ( 0x19, ( rr_c, EightTStates ) )
        , ( 0x1A, ( rr_d, EightTStates ) )
        , ( 0x1B, ( rr_e, EightTStates ) )
        , ( 0x1C, ( rr_h, EightTStates ) )
        , ( 0x1D, ( rr_l, EightTStates ) )
        , ( 0x20, ( sla_b, EightTStates ) )
        , ( 0x21, ( sla_c, EightTStates ) )
        , ( 0x22, ( sla_d, EightTStates ) )
        , ( 0x23, ( sla_e, EightTStates ) )
        , ( 0x24, ( sla_h, EightTStates ) )
        , ( 0x25, ( sla_l, EightTStates ) )
        , ( 0x28, ( sra_b, EightTStates ) )
        , ( 0x29, ( sra_c, EightTStates ) )
        , ( 0x2A, ( sra_d, EightTStates ) )
        , ( 0x2B, ( sra_e, EightTStates ) )
        , ( 0x2C, ( sra_h, EightTStates ) )
        , ( 0x2D, ( sra_l, EightTStates ) )
        , ( 0x30, ( sll_b, EightTStates ) )
        , ( 0x31, ( sll_c, EightTStates ) )
        , ( 0x32, ( sll_d, EightTStates ) )
        , ( 0x33, ( sll_e, EightTStates ) )
        , ( 0x34, ( sll_h, EightTStates ) )
        , ( 0x35, ( sll_l, EightTStates ) )
        , ( 0x38, ( srl_b, EightTStates ) )
        , ( 0x39, ( srl_c, EightTStates ) )
        , ( 0x3A, ( srl_d, EightTStates ) )
        , ( 0x3B, ( srl_e, EightTStates ) )
        , ( 0x3C, ( srl_h, EightTStates ) )
        , ( 0x3D, ( srl_l, EightTStates ) )
        , ( 0x40, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x41, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x42, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x43, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x44, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x45, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x48, ( bit_1_b, EightTStates ) )
        , ( 0x49, ( bit_1_c, EightTStates ) )
        , ( 0x4A, ( bit_1_d, EightTStates ) )
        , ( 0x4B, ( bit_1_e, EightTStates ) )
        , ( 0x4C, ( bit_1_h, EightTStates ) )
        , ( 0x4D, ( bit_1_l, EightTStates ) )
        , ( 0x50, ( bit_2_b, EightTStates ) )
        , ( 0x51, ( bit_2_c, EightTStates ) )
        , ( 0x52, ( bit_2_d, EightTStates ) )
        , ( 0x53, ( bit_2_e, EightTStates ) )
        , ( 0x54, ( bit_2_h, EightTStates ) )
        , ( 0x55, ( bit_2_l, EightTStates ) )
        , ( 0x58, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x59, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x5A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x5B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x5C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x5D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x60, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x61, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x62, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x63, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x64, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x65, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x68, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x69, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x6A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x6B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x6C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x6D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x70, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x71, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x72, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x73, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x74, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x75, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x78, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.b |> Z80ChangeFlags, EightTStates ) )
        , ( 0x79, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.c |> Z80ChangeFlags, EightTStates ) )
        , ( 0x7A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.d |> Z80ChangeFlags, EightTStates ) )
        , ( 0x7B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.e |> Z80ChangeFlags, EightTStates ) )
        , ( 0x7C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, EightTStates ) )
        , ( 0x7D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, EightTStates ) )
        ]


singleByteMainAndFlagRegistersIXCB : Dict Int ( MainWithIndexRegisters -> FlagRegisters -> Z80Change, InstructionDuration )
singleByteMainAndFlagRegistersIXCB =
    Dict.fromList
        [ ( 0x00, ( rlc_b, EightTStates ) )
        , ( 0x01, ( rlc_c, EightTStates ) )
        , ( 0x02, ( rlc_d, EightTStates ) )
        , ( 0x03, ( rlc_e, EightTStates ) )

        --
        ----, ( 0x04, ( rlc_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x05, ( rlc_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x08, ( rrc_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x09, ( rrc_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x0A, ( rrc_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x0B, ( rrc_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x0C, ( rrc_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x0D, ( rrc_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x10, ( rl_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x11, ( rl_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x12, ( rl_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x13, ( rl_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x14, ( rl_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x15, ( rl_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x18, ( rr_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x19, ( rr_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x1A, ( rr_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x1B, ( rr_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x1C, ( rr_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x1D, ( rr_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x20, ( sla_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x21, ( sla_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x22, ( sla_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x23, ( sla_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x24, ( sla_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x25, ( sla_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x28, ( sra_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x29, ( sra_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x2A, ( sra_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x2B, ( sra_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x2C, ( sra_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x2D, ( sra_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x30, ( sll_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x31, ( sll_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x32, ( sll_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x33, ( sll_e, PCIncrementByFour, EightTStates ) )
        --, ( 0x34, ( sll_h, PCIncrementByFour, EightTStates ) )
        --, ( 0x35, ( sll_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x38, ( srl_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x39, ( srl_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x3A, ( srl_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x3B, ( srl_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x3C, ( srl_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x3D, ( srl_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x40, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x41, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x42, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x43, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x44, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.ix |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x45, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.ix |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x48, ( bit_1_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x49, ( bit_1_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x4A, ( bit_1_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x4B, ( bit_1_e, PCIncrementByFour, EightTStates ) )
        --, ( 0x4C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_1 (z80_main.ix |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x4D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_1 (z80_main.ix |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x50, ( bit_2_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x51, ( bit_2_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x52, ( bit_2_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x53, ( bit_2_e, PCIncrementByFour, EightTStates ) )
        --, ( 0x54, ( \z80_main z80_flags -> z80_flags |> testBit Bit_2 (z80_main.ix |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x55, ( \z80_main z80_flags -> z80_flags |> testBit Bit_2 (z80_main.ix |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x58, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x59, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x5A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x5B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x5C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.ix |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x5D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.ix |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x60, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x61, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x62, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x63, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x64, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.ix |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x65, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.ix |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x68, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x69, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x6A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x6B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x6C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.ix |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x6D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.ix |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x70, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x71, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x72, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x73, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x74, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.ix |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x75, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.ix |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x78, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x79, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x7A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x7B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x7C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.ix |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x7D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.ix |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        ]


singleByteMainAndFlagRegistersIYCB : Dict Int ( MainWithIndexRegisters -> FlagRegisters -> Z80Change, InstructionDuration )
singleByteMainAndFlagRegistersIYCB =
    Dict.fromList
        [ ( 0x00, ( rlc_b, EightTStates ) )
        , ( 0x01, ( rlc_c, EightTStates ) )
        , ( 0x02, ( rlc_d, EightTStates ) )
        , ( 0x03, ( rlc_e, EightTStates ) )

        --( 0x00, ( rlc_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x01, ( rlc_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x02, ( rlc_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x03, ( rlc_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x04, ( rlc_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x05, ( rlc_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x08, ( rrc_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x09, ( rrc_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x0A, ( rrc_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x0B, ( rrc_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x0C, ( rrc_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x0D, ( rrc_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x10, ( rl_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x11, ( rl_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x12, ( rl_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x13, ( rl_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x14, ( rl_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x15, ( rl_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x18, ( rr_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x19, ( rr_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x1A, ( rr_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x1B, ( rr_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x1C, ( rr_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x1D, ( rr_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x20, ( sla_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x21, ( sla_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x22, ( sla_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x23, ( sla_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x24, ( sla_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x25, ( sla_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x28, ( sra_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x29, ( sra_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x2A, ( sra_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x2B, ( sra_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x2C, ( sra_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x2D, ( sra_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x30, ( sll_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x31, ( sll_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x32, ( sll_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x33, ( sll_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x34, ( sll_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x35, ( sll_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x38, ( srl_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x39, ( srl_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x3A, ( srl_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x3B, ( srl_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x3C, ( srl_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x3D, ( srl_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x40, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x41, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x42, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x43, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x44, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.iy |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x45, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.iy |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x48, ( bit_1_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x49, ( bit_1_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x4A, ( bit_1_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x4B, ( bit_1_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x4C, ( bit_1_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x4D, ( bit_1_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x50, ( bit_2_b, PCIncrementByFour, EightTStates ) )
        --, ( 0x51, ( bit_2_c, PCIncrementByFour, EightTStates ) )
        --, ( 0x52, ( bit_2_d, PCIncrementByFour, EightTStates ) )
        --, ( 0x53, ( bit_2_e, PCIncrementByFour, EightTStates ) )
        --
        ----, ( 0x54, ( bit_2_h, PCIncrementByFour, EightTStates ) )
        ----, ( 0x55, ( bit_2_l, PCIncrementByFour, EightTStates ) )
        --, ( 0x58, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x59, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x5A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x5B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x5C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.iy |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x5D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.iy |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x60, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x61, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x62, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x63, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x64, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.iy |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x65, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.iy |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x68, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x69, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x6A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x6B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x6C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.iy |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x6D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.iy |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x70, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x71, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x72, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x73, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x74, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.iy |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x75, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.iy |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x78, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.b |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x79, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.c |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x7A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.d |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x7B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.e |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x7C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.iy |> shiftRightBy8) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        --, ( 0x7D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.iy |> Bitwise.and 0xFF) |> Z80ChangeFlags, PCIncrementByFour, EightTStates ) )
        ]


rlc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_b z80_main z80_flags =
    z80_flags |> shifter0 z80_main.b |> FlagsWithBRegister


rlc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    --z80_flags |> shifter_c shifter0 z80_main.c
    z80_flags |> shifter0 z80_main.c |> FlagsWithCRegister


rlc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter0 z80_main.d |> FlagsWithDRegister


rlc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter0 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


rlc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter0 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


rlc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter0 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


rrc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_b z80_main z80_flags =
    z80_flags |> shifter1 z80_main.b |> FlagsWithBRegister


rrc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter1 z80_main.c |> FlagsWithCRegister


rrc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter1 z80_main.d |> FlagsWithDRegister


rrc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter1 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


rrc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter1 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


rrc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter1 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


rl_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter2 z80_main.b |> FlagsWithBRegister


rl_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter2 z80_main.c |> FlagsWithCRegister


rl_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter2 z80_main.d |> FlagsWithDRegister


rl_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter2 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


rl_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter2 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


rl_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter2 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


rr_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter3 z80_main.b |> FlagsWithBRegister


rr_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter3 z80_main.c |> FlagsWithCRegister


rr_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter3 z80_main.d |> FlagsWithDRegister


rr_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter3 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


rr_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter3 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


rr_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter3 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


sla_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter4 z80_main.b |> FlagsWithBRegister


sla_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter4 z80_main.c |> FlagsWithCRegister


sla_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter4 z80_main.d |> FlagsWithDRegister


sla_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter4 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


sla_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter4 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


sla_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter4 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


sra_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter5 z80_main.b |> FlagsWithBRegister


sra_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter5 z80_main.c |> FlagsWithCRegister


sra_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter5 z80_main.d |> FlagsWithDRegister


sra_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter5 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


sra_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter5 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


sra_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter5 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


sll_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter6 z80_main.b |> FlagsWithBRegister


sll_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter6 z80_main.c |> FlagsWithCRegister


sll_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter6 z80_main.d |> FlagsWithDRegister


sll_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter6 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


sll_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter6 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


sll_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter6 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


srl_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter7 z80_main.b |> FlagsWithBRegister


srl_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter7 z80_main.c |> FlagsWithCRegister


srl_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter7 z80_main.d |> FlagsWithDRegister


srl_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter7 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


srl_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter7 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl


srl_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter7 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl


bit_1_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_b z80_main z80_flags =
    -- case 0x40: bit(o,B); break;
    z80_flags |> testBit Bit_1 z80_main.b |> Z80ChangeFlags


bit_1_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_c z80_main z80_flags =
    z80_flags |> testBit Bit_1 z80_main.c |> Z80ChangeFlags


bit_1_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_d z80_main z80_flags =
    z80_flags |> testBit Bit_1 z80_main.d |> Z80ChangeFlags


bit_1_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_e z80_main z80_flags =
    z80_flags |> testBit Bit_1 z80_main.e |> Z80ChangeFlags


bit_1_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_h z80_main z80_flags =
    z80_flags |> testBit Bit_1 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags


bit_1_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_l z80_main z80_flags =
    z80_flags |> testBit Bit_1 (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags


bit_2_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_b z80_main z80_flags =
    -- case 0x40: bit(o,B); break;
    z80_flags |> testBit Bit_2 z80_main.b |> Z80ChangeFlags


bit_2_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_c z80_main z80_flags =
    -- case 0x41: bit(o,C); break;
    z80_flags |> testBit Bit_2 z80_main.c |> Z80ChangeFlags


bit_2_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_d z80_main z80_flags =
    -- case 0x42: bit(o,D); break;
    z80_flags |> testBit Bit_2 z80_main.d |> Z80ChangeFlags


bit_2_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_e z80_main z80_flags =
    z80_flags |> testBit Bit_2 z80_main.e |> Z80ChangeFlags


bit_2_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_h z80_main z80_flags =
    z80_flags |> testBit Bit_2 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags


bit_2_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_l z80_main z80_flags =
    z80_flags |> testBit Bit_2 (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags


bit_indirect_ix : BitTest -> MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_indirect_ix bittest z80_main offset rom48k z80_env =
    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
    let
        value =
            z80_env |> mem ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
    in
    SingleBitTest bittest value



--bit_1_indirect_ix : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
--bit_1_indirect_ix z80_main offset rom48k z80_env =
--    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
--    let
--        value =
--            z80_env |> mem ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
--    in
--    SingleBitTest Bit_1 value
--
--
--bit_2_indirect_ix : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
--bit_2_indirect_ix z80_main offset rom48k z80_env =
--    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
--    let
--        value =
--            z80_env |> mem ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
--    in
--    SingleBitTest Bit_2 value
--
--
--bit_3_indirect_ix : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
--bit_3_indirect_ix z80_main offset rom48k z80_env =
--    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
--    let
--        value =
--            z80_env |> mem ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
--    in
--    SingleBitTest Bit_3 value
--
--
--bit_4_indirect_ix : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
--bit_4_indirect_ix z80_main offset rom48k z80_env =
--    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
--    let
--        value =
--            z80_env |> mem ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
--    in
--    SingleBitTest Bit_4 value
--
--
--bit_5_indirect_ix : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
--bit_5_indirect_ix z80_main offset rom48k z80_env =
--    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
--    let
--        value =
--            z80_env |> mem ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
--    in
--    SingleBitTest Bit_5 value
--
--
--bit_6_indirect_ix : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
--bit_6_indirect_ix z80_main offset rom48k z80_env =
--    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
--    let
--        value =
--            z80_env |> mem ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
--    in
--    SingleBitTest Bit_6 value
--
--
--bit_7_indirect_ix : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
--bit_7_indirect_ix z80_main offset rom48k z80_env =
--    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
--    let
--        value =
--            z80_env |> mem ((z80_main.ix + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
--    in
--    SingleBitTest Bit_7 value


bit_0_indirect_iy : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_0_indirect_iy z80_main offset rom48k z80_env =
    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
    let
        value =
            z80_env |> mem ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
    in
    SingleBitTest Bit_0 value


bit_1_indirect_iy : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_1_indirect_iy z80_main offset rom48k z80_env =
    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
    let
        value =
            z80_env |> mem ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
    in
    SingleBitTest Bit_1 value


bit_2_indirect_iy : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_2_indirect_iy z80_main offset rom48k z80_env =
    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
    let
        value =
            z80_env |> mem ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
    in
    SingleBitTest Bit_2 value


bit_3_indirect_iy : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_3_indirect_iy z80_main offset rom48k z80_env =
    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
    let
        value =
            z80_env |> mem ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
    in
    SingleBitTest Bit_3 value


bit_4_indirect_iy : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_4_indirect_iy z80_main offset rom48k z80_env =
    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
    let
        value =
            z80_env |> mem ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
    in
    SingleBitTest Bit_4 value


bit_5_indirect_iy : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_5_indirect_iy z80_main offset rom48k z80_env =
    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
    let
        value =
            z80_env |> mem ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
    in
    SingleBitTest Bit_5 value


bit_6_indirect_iy : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_6_indirect_iy z80_main offset rom48k z80_env =
    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
    let
        value =
            z80_env |> mem ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
    in
    SingleBitTest Bit_6 value


bit_7_indirect_iy : MainWithIndexRegisters -> Int -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_7_indirect_iy z80_main offset rom48k z80_env =
    --case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
    let
        value =
            z80_env |> mem ((z80_main.iy + byte offset) |> Bitwise.and 0xFFFF) z80_env.time rom48k
    in
    SingleBitTest Bit_7 value
