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
    | FlagJump FlagFunc Int


singleWith8BitParam : Dict Int ( Int -> Single8BitChange, InstructionDuration )
singleWith8BitParam =
    Dict.fromList
        [ ( 0x06, ( ld_b_n, SevenTStates ) )
        , ( 0x0E, ( ld_c_n, SevenTStates ) )
        , ( 0x16, ( ld_d_n, SevenTStates ) )
        , ( 0x1E, ( ld_e_n, SevenTStates ) )
        , ( 0x3E, ( ld_a_n, SevenTStates ) )
        , ( 0xC6, ( add_a_n, SevenTStates ) )
        , ( 0xCE, ( adc_n, SevenTStates ) )
        , ( 0xD3, ( out_n_a, ElevenTStates ) )
        , ( 0xD6, ( sub_n, SevenTStates ) )
        , ( 0xDB, ( in_a_n, ElevenTStates ) )
        , ( 0xDE, ( sbc_a_n, SevenTStates ) )
        , ( 0xE6, ( and_n, SevenTStates ) )
        , ( 0xEE, ( xor_n, SevenTStates ) )
        , ( 0xF6, ( or_n, SevenTStates ) )
        , ( 0xFE, ( cp_n, SevenTStates ) )
        ]


maybeRelativeJump : Dict Int ( Int -> JumpChange, InstructionDuration )
maybeRelativeJump =
    Dict.fromList
        [ ( 0x10, ( djnz, FourTStates ) )
        , ( 0x18, ( jr_n, TwelveTStates ) )
        , ( 0x20, ( jr_nz_d, SevenTStates ) )
        , ( 0x28, ( jr_z_d, SevenTStates ) )
        , ( 0x30, ( jr_nc_d, SevenTStates ) )
        , ( 0x38, ( jr_c_d, SevenTStates ) )
        ]


type JumpChange
    = ActualJumpOffset Int
    | ConditionalJumpOffset Int ShortDelay (FlagRegisters -> Bool)
    | DJNZOffset Int ShortDelay


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
    NewRegister RegisterB param


ld_c_n : Int -> Single8BitChange
ld_c_n param =
    -- case 0x0E: C=imm8(); break;
    NewRegister RegisterC param


ld_d_n : Int -> Single8BitChange
ld_d_n param =
    -- case 0x16: D=imm8(); break;
    NewRegister RegisterD param


ld_e_n : Int -> Single8BitChange
ld_e_n param =
    -- case 0x1E: E=imm8(); break;
    NewRegister RegisterE param


jr_n : Int -> JumpChange
jr_n param =
    -- case 0x18: MP=PC=(char)(PC+1+(byte)env.mem(PC)); time+=8; break;
    ActualJumpOffset (byte param)


jr_nz_d : Int -> JumpChange
jr_nz_d param =
    -- case 0x20: if(Fr!=0) jr(); else imm8(); break;
    ConditionalJumpOffset (byte param) FiveExtraTStates jump_nz


jr_z_d : Int -> JumpChange
jr_z_d param =
    -- case 0x28: if(Fr==0) jr(); else imm8(); break;
    ConditionalJumpOffset (byte param) FiveExtraTStates jump_z


jr_nc_d : Int -> JumpChange
jr_nc_d param =
    -- case 0x30: if((Ff&0x100)==0) jr(); else imm8(); break;
    ConditionalJumpOffset (byte param) FiveExtraTStates jump_nc


jr_c_d : Int -> JumpChange
jr_c_d param =
    -- case 0x38: if((Ff&0x100)!=0) jr(); else imm8(); break;
    ConditionalJumpOffset (byte param) FiveExtraTStates jump_c


djnz : Int -> JumpChange
djnz param =
    --case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
    --if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
    --PC=(char)v;} break;
    DJNZOffset (byte param) FiveExtraTStates


add_a_n : Int -> Single8BitChange
add_a_n param =
    -- case 0xC6: add(imm8()); break;
    FlagJump AddA param


adc_n : Int -> Single8BitChange
adc_n param =
    -- case 0xCE: adc(imm8()); break;
    FlagJump AdcA param


sub_n : Int -> Single8BitChange
sub_n param =
    -- case 0xD6: sub(imm8()); break;
    FlagJump SubA param


sbc_a_n : Int -> Single8BitChange
sbc_a_n param =
    -- case 0xDE: sbc(imm8()); break;
    FlagJump SbcA param


and_n : Int -> Single8BitChange
and_n param =
    -- case 0xE6: and(imm8()); break;
    FlagJump AndA param


xor_n : Int -> Single8BitChange
xor_n param =
    -- case 0xEE: xor(imm8()); break;
    FlagJump XorA param


or_n : Int -> Single8BitChange
or_n param =
    -- case 0xF6: or(imm8()); break;
    FlagJump OrA param


cp_n : Int -> Single8BitChange
cp_n param =
    -- case 0xFE: cp(imm8()); break;
    FlagJump CpA param


ld_a_n : Int -> Single8BitChange
ld_a_n param =
    -- case 0x3E: A=imm8(); break;
    NewARegister param


out_n_a : Int -> Single8BitChange
out_n_a param =
    -- case 0xD3: env.out(v=imm8()|A<<8,A); MP=v+1&0xFF|v&0xFF00; time+=4; break;
    Z80Out param


in_a_n : Int -> Single8BitChange
in_a_n param =
    -- case 0xDB: MP=(v=imm8()|A<<8)+1; A=env.in(v); time+=4; break;
    Z80In param
