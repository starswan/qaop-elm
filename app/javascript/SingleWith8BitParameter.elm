module SingleWith8BitParameter exposing (..)

import Bitwise
import Dict exposing (Dict)
import Utils exposing (byte)
import Z80Types exposing (MainWithIndexRegisters, Z80)


singleWith8BitParam : Dict Int (Int -> Single8BitChange)
singleWith8BitParam =
    Dict.fromList
        [ ( 0x06, ld_b_n )
        , ( 0x0E, ld_c_n )
        , ( 0x16, ld_d_n )
        ]


doubleWithRegisters : Dict Int (MainWithIndexRegisters -> Int -> DoubleWithRegisterChange)
doubleWithRegisters =
    Dict.fromList
        [ ( 0x10, djnz )
        ]

relativeJump: Dict Int (Int -> DoubleWithRegisterChange)
relativeJump =
    Dict.fromList
    [
    (0x18, jr_n)
    ]


type Single8BitChange
    = NewBRegister Int
    | NewCRegister Int
    | NewDRegister Int


type DoubleWithRegisterChange
    = RelativeJumpWithTimeOffset (Maybe Single8BitChange) (Maybe Int) Int

applySimple8BitChange : Single8BitChange -> MainWithIndexRegisters -> MainWithIndexRegisters
applySimple8BitChange change z80_main =
    case change of
        NewBRegister int ->
            { z80_main | b = int }

        NewCRegister int ->
            { z80_main | c = int }

        NewDRegister int ->
            { z80_main | d = int }


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
    RelativeJumpWithTimeOffset (Just (NewBRegister b)) jump time

jr_n : Int -> DoubleWithRegisterChange
jr_n param =
    -- case 0x18: MP=PC=(char)(PC+1+(byte)env.mem(PC)); time+=8; break;
    -- This is just an inlined jr() call
    --z80 |> set_pc dest |> add_cpu_time 8
    RelativeJumpWithTimeOffset Nothing (Just param) 8


