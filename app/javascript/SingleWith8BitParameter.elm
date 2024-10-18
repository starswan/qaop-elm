module SingleWith8BitParameter exposing (..)

import Bitwise
import Dict exposing (Dict)
import Utils exposing (byte)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (MainWithIndexRegisters, Z80)


singleWith8BitParam : Dict Int (Int -> Single8BitChange)
singleWith8BitParam =
    Dict.fromList
        [ ( 0x06, ld_b_n )
        , ( 0x0E, ld_c_n )
        , ( 0x16, ld_d_n )
        , ( 0x1E, ld_e_n )
        ]


doubleWithRegisters : Dict Int (MainWithIndexRegisters -> Int -> DoubleWithRegisterChange)
doubleWithRegisters =
    Dict.fromList
        [ ( 0x10, djnz )
        ]


maybeRelativeJump : Dict Int (Int -> FlagRegisters -> JumpChange)
maybeRelativeJump =
    Dict.fromList
        [ ( 0x18, jr_n )
        , ( 0x20, jr_nz_d )
        , ( 0x28, jr_z_d )
        , ( 0x30, jr_nc_d )
        , ( 0x38, jr_c_d )
        ]


type Single8BitChange
    = NewBRegister Int
    | NewCRegister Int
    | NewDRegister Int
    | NewERegister Int


type DoubleWithRegisterChange
    = RelativeJumpWithTimeOffset Single8BitChange (Maybe Int) Int


type alias JumpChange =
    { jump : Maybe Int
    }


applySimple8BitChange : Single8BitChange -> MainWithIndexRegisters -> MainWithIndexRegisters
applySimple8BitChange change z80_main =
    case change of
        NewBRegister int ->
            { z80_main | b = int }

        NewCRegister int ->
            { z80_main | c = int }

        NewDRegister int ->
            { z80_main | d = int }

        NewERegister int ->
            { z80_main | e = int }


ld_b_n : Int -> Single8BitChange
ld_b_n param =
    -- case 0x06: B=imm8(); break;
    --{ z80 | env = new_b.env, pc = new_b.pc }|> set_b new_b.value
    NewBRegister param


ld_c_n : Int -> Single8BitChange
ld_c_n param =
    -- case 0x0E: C=imm8(); break;
    --{ z80 | env = new_c.env, pc = new_c.pc, main = { z80_main | c = new_c.value } }
    NewCRegister param


ld_d_n : Int -> Single8BitChange
ld_d_n param =
    -- case 0x16: D=imm8(); break;
    NewDRegister param


ld_e_n : Int -> Single8BitChange
ld_e_n param =
    -- case 0x1E: E=imm8(); break;
    NewERegister param


djnz : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
djnz z80_main param =
    --case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
    --if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
    --PC=(char)v;} break;
    let
        d =
            byte param

        b =
            Bitwise.and (z80_main.b - 1) 0xFF

        ( time, jump ) =
            if b /= 0 then
                ( 9, Just d )

            else
                ( 4, Nothing )
    in
    RelativeJumpWithTimeOffset (NewBRegister b) jump time


jr_n : Int -> FlagRegisters -> JumpChange
jr_n param _ =
    -- case 0x18: MP=PC=(char)(PC+1+(byte)env.mem(PC)); time+=8; break;
    -- This is just an inlined jr() call
    --z80 |> set_pc dest |> add_cpu_time 8
    { jump = Just (byte param) }


jr_nz_d : Int -> FlagRegisters -> JumpChange
jr_nz_d param z80_flags =
    -- case 0x20: if(Fr!=0) jr(); else imm8(); break;
    if z80_flags.fr /= 0 then
        { jump = Just (byte param) }

    else
        { jump = Nothing }


jr_z_d : Int -> FlagRegisters -> JumpChange
jr_z_d param z80_flags =
    -- case 0x28: if(Fr==0) jr(); else imm8(); break;
    if z80_flags.fr == 0 then
        { jump = Just (byte param) }

    else
        { jump = Nothing }


jr_nc_d : Int -> FlagRegisters -> JumpChange
jr_nc_d param z80_flags =
    -- case 0x30: if((Ff&0x100)==0) jr(); else imm8(); break;
    if Bitwise.and z80_flags.ff 0x0100 == 0 then
        { jump = Just (byte param) }

    else
        { jump = Nothing }


jr_c_d : Int -> FlagRegisters -> JumpChange
jr_c_d param z80_flags =
    -- case 0x38: if((Ff&0x100)!=0) jr(); else imm8(); break;
    if Bitwise.and z80_flags.ff 0x0100 /= 0 then
        { jump = Just (byte param) }

    else
        { jump = Nothing }
