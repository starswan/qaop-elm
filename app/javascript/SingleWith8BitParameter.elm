module SingleWith8BitParameter exposing (..)

import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..), ShortDelay(..))
import Dict exposing (Dict)
import Utils exposing (byte)
import Z80Flags exposing (FlagFunc(..), FlagRegisters, jump_c, jump_nc, jump_nz, jump_z)
import Z80Registers exposing (CoreRegister(..))
import Z80Types exposing (MainWithIndexRegisters)


type Single8BitChange
    = NewRegister CoreRegister Int
    | Z80In Int
    | Z80Out Int
    | NewARegister Int


singleWith8BitParam : Dict Int ( Int -> Single8BitChange, InstructionDuration )
singleWith8BitParam =
    Dict.fromList
        [ ( 0x06, ( ld_b_n, SevenTStates ) )
        , ( 0x0E, ( ld_c_n, SevenTStates ) )
        , ( 0x16, ( ld_d_n, SevenTStates ) )
        , ( 0x1E, ( ld_e_n, SevenTStates ) )
        , ( 0x3E, ( ld_a_n, SevenTStates ) )
        , ( 0xD3, ( out_n_a, ElevenTStates ) )
        , ( 0xDB, ( in_a_n, ElevenTStates ) )
        ]


maybeRelativeJump : Dict Int ( Int -> Int -> JumpChange, InstructionDuration )
maybeRelativeJump =
    Dict.fromList
        [ ( 0x18, ( jr_n, TwelveTStates ) )
        , ( 0x10, ( djnz, FourTStates ) )
        , ( 0x20, ( jr_nz_d, SevenTStates ) )
        , ( 0x28, ( jr_z_d, SevenTStates ) )
        , ( 0x30, ( jr_nc_d, SevenTStates ) )
        , ( 0x38, ( jr_c_d, SevenTStates ) )
        , ( 0xC6, ( add_a_n, SevenTStates ) )
        , ( 0xCE, ( adc_n, SevenTStates ) )
        , ( 0xD6, ( sub_n, SevenTStates ) )
        , ( 0xDE, ( sbc_a_n, SevenTStates ) )
        , ( 0xE6, ( and_n, SevenTStates ) )
        , ( 0xEE, ( xor_n, SevenTStates ) )
        , ( 0xF6, ( or_n, SevenTStates ) )
        , ( 0xFE, ( cp_n, SevenTStates ) )
        ]


type JumpChange
    = ActualJump Int
    | FlagJump FlagFunc Int
    | ConditionalJump Int ShortDelay (FlagRegisters -> Bool)
    | DJNZ Int ShortDelay


applySimple8BitChange : CoreRegister -> Int -> MainWithIndexRegisters -> MainWithIndexRegisters
applySimple8BitChange change int z80_main =
    case change of
        RegisterB ->
            { z80_main | b = int }

        RegisterC ->
            { z80_main | c = int }

        RegisterD ->
            { z80_main | d = int }

        RegisterE ->
            { z80_main | e = int }


ld_b_n : Int -> Single8BitChange
ld_b_n param =
    -- case 0x06: B=imm8(); break;
    --{ z80 | env = new_b.env, pc = new_b.pc }|> set_b new_b.value
    NewRegister RegisterB param


ld_c_n : Int -> Single8BitChange
ld_c_n param =
    -- case 0x0E: C=imm8(); break;
    --{ z80 | env = new_c.env, pc = new_c.pc, main = { z80_main | c = new_c.value } }
    NewRegister RegisterC param


ld_d_n : Int -> Single8BitChange
ld_d_n param =
    -- case 0x16: D=imm8(); break;
    NewRegister RegisterD param


ld_e_n : Int -> Single8BitChange
ld_e_n param =
    -- case 0x1E: E=imm8(); break;
    NewRegister RegisterE param


jr_n : Int -> Int -> JumpChange
jr_n param pc =
    -- case 0x18: MP=PC=(char)(PC+1+(byte)env.mem(PC)); time+=8; break;
    -- This is just an inlined jr() call
    --z80 |> set_pc dest |> add_cpu_time 8
    --ActualJump (byte param)
    ActualJump (pc + 2 + byte param |> Bitwise.and 0xFFFF)


jr_nz_d : Int -> Int -> JumpChange
jr_nz_d param pc =
    -- case 0x20: if(Fr!=0) jr(); else imm8(); break;
    --if z80_flags.fr /= 0 then
    --    ActualJump (byte param)
    --
    --else
    --    NoJump
    ConditionalJump (pc + 2 + byte param) FiveExtraTStates jump_nz


jr_z_d : Int -> Int -> JumpChange
jr_z_d param pc =
    -- case 0x28: if(Fr==0) jr(); else imm8(); break;
    --if z80_flags.fr == 0 then
    --    ActualJump (byte param)
    --
    --else
    --    NoJump
    ConditionalJump (pc + 2 + byte param) FiveExtraTStates jump_z


jr_nc_d : Int -> Int -> JumpChange
jr_nc_d param pc =
    -- case 0x30: if((Ff&0x100)==0) jr(); else imm8(); break;
    --if Bitwise.and z80_flags.ff 0x0100 == 0 then
    --    ActualJump (byte param)
    --
    --else
    --    NoJump
    ConditionalJump (pc + 2 + byte param) FiveExtraTStates jump_nc


jr_c_d : Int -> Int -> JumpChange
jr_c_d param pc =
    -- case 0x38: if((Ff&0x100)!=0) jr(); else imm8(); break;
    --if Bitwise.and z80_flags.ff 0x0100 /= 0 then
    --    ActualJump (byte param)
    --
    --else
    --    NoJump
    ConditionalJump (pc + 2 + byte param) FiveExtraTStates jump_c


djnz : Int -> Int -> JumpChange
djnz param pc =
    --case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
    --if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
    --PC=(char)v;} break;
    --let
    --    d =
    --        byte param
    --
    --    b =
    --        Bitwise.and (z80_main.b - 1) 0xFF
    --
    --    ( time, jump ) =
    --        if b /= 0 then
    --            ( 9, Just d )
    --
    --        else
    --            ( 4, Nothing )
    --in
    --RelativeJumpWithTimeOffset (NewBRegister b) jump time
    DJNZ (pc + 2 + byte param) FiveExtraTStates


add_a_n : Int -> Int -> JumpChange
add_a_n param _ =
    -- case 0xC6: add(imm8()); break;
    --let
    --    flags =
    --        z80_flags |> z80_add param
    --in
    --FlagJump flags
    FlagJump AddA param


adc_n : Int -> Int -> JumpChange
adc_n param _ =
    -- case 0xCE: adc(imm8()); break;
    --let
    --    flags =
    --        z80_flags |> adc param
    --in
    --FlagJump flags
    FlagJump AdcA param


sub_n : Int -> Int -> JumpChange
sub_n param _ =
    -- case 0xD6: sub(imm8()); break;
    --let
    --    flags =
    --        z80_flags |> z80_sub param
    --in
    --FlagJump flags
    FlagJump SubA param


sbc_a_n : Int -> Int -> JumpChange
sbc_a_n param _ =
    -- case 0xDE: sbc(imm8()); break;
    --z80_flags |> sbc param |> FlagJump
    FlagJump SbcA param


and_n : Int -> Int -> JumpChange
and_n param _ =
    -- case 0xE6: and(imm8()); break;
    --let
    --    flags =
    --        z80_flags |> z80_and param
    --in
    --FlagJump flags
    FlagJump AndA param


xor_n : Int -> Int -> JumpChange
xor_n param _ =
    -- case 0xEE: xor(imm8()); break;
    --let
    --    flags =
    --        z80_flags |> z80_xor param
    --in
    --FlagJump flags
    FlagJump XorA param


or_n : Int -> Int -> JumpChange
or_n param _ =
    -- case 0xF6: or(imm8()); break;
    --let
    --    flags =
    --        z80_flags |> z80_or param
    --in
    --FlagJump flags
    FlagJump OrA param


cp_n : Int -> Int -> JumpChange
cp_n param _ =
    -- case 0xFE: cp(imm8()); break;
    --let
    --    flags =
    --        z80_flags |> z80_cp param
    --in
    --FlagJump flags
    FlagJump CpA param


ld_a_n : Int -> Single8BitChange
ld_a_n param =
    -- case 0x3E: A=imm8(); break;
    --FlagJump { z80_flags | a = param }
    NewARegister param


out_n_a : Int -> Single8BitChange
out_n_a param =
    -- case 0xD3: env.out(v=imm8()|A<<8,A); MP=v+1&0xFF|v&0xFF00; time+=4; break;
    --let
    --    portNum =
    --        Bitwise.or param (shiftLeftBy8 z80_flags.a)
    --in
    --Z80Out portNum z80_flags.a
    Z80Out param


in_a_n : Int -> Single8BitChange
in_a_n param =
    -- case 0xDB: MP=(v=imm8()|A<<8)+1; A=env.in(v); time+=4; break;
    --let
    --    portNum =
    --        Bitwise.or param (shiftLeftBy8 z80_flags.a)
    --in
    --Z80In portNum
    Z80In param
