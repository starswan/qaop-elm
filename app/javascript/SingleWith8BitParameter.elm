module SingleWith8BitParameter exposing (..)

import Bitwise
import CpuTimeCTime exposing (InstructionDuration(..))
import Dict exposing (Dict)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..))
import Utils exposing (byte, shiftLeftBy8)
import Z80Flags exposing (FlagFunc(..), FlagRegisters, adc, sbc, z80_add, z80_and, z80_cp, z80_or, z80_sub, z80_xor)
import Z80Types exposing (MainWithIndexRegisters)


singleWith8BitParam : Dict Int ( Int -> Single8BitChange, MediumPCIncrement, InstructionDuration )
singleWith8BitParam =
    Dict.fromList
        [ ( 0x06, ( ld_b_n, IncreaseByTwo, SevenTStates ) )
        , ( 0x0E, ( ld_c_n, IncreaseByTwo, SevenTStates ) )
        , ( 0x16, ( ld_d_n, IncreaseByTwo, SevenTStates ) )
        , ( 0x1E, ( ld_e_n, IncreaseByTwo, SevenTStates ) )
        ]


doubleWithRegisters : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, MediumPCIncrement, InstructionDuration )
doubleWithRegisters =
    Dict.fromList
        [ --  another 5 if jump actually taken
          ( 0x10, ( djnz, IncreaseByTwo, EightTStates ) )
        , ( 0x26, ( ld_h_n, IncreaseByTwo, SevenTStates ) )
        , ( 0x2E, ( ld_l_n, IncreaseByTwo, SevenTStates ) )
        , ( 0x36, ( ld_indirect_hl_n, IncreaseByTwo, TenTStates ) )
        ]


doubleWithRegistersIX : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, MediumPCIncrement, InstructionDuration )
doubleWithRegistersIX =
    Dict.fromList
        [ ( 0x26, ( ld_ix_h_n, IncreaseByThree, ElevenTStates ) )
        , ( 0x2E, ( ld_ix_l_n, IncreaseByThree, ElevenTStates ) )
        , ( 0x34, ( inc_indirect_ix, IncreaseByThree, TwentyThreeTStates ) )
        , ( 0x35, ( dec_indirect_ix, IncreaseByThree, TwentyThreeTStates ) )
        , ( 0x46, ( ld_b_indirect_ix, IncreaseByThree, SevenTStates ) )
        , ( 0x4E, ( ld_c_indirect_ix, IncreaseByThree, SevenTStates ) )
        , ( 0x56, ( ld_d_indirect_ix, IncreaseByThree, SevenTStates ) )
        , ( 0x5E, ( ld_e_indirect_ix, IncreaseByThree, SevenTStates ) )
        , ( 0x66, ( \z80_main param -> NewHRegisterIndirect (z80_main.ix + byte param), IncreaseByThree, NineteenTStates ) )
        , ( 0x6E, ( \z80_main param -> NewLRegisterIndirect (z80_main.ix + byte param), IncreaseByThree, NineteenTStates ) )
        , ( 0x86, ( \z80_main param -> FlagOpIndexedIndirect AddA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0x8E, ( \z80_main param -> FlagOpIndexedIndirect AdcA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0x96, ( \z80_main param -> FlagOpIndexedIndirect SubA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0x9E, ( \z80_main param -> FlagOpIndexedIndirect SbcA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0xA6, ( \z80_main param -> FlagOpIndexedIndirect AndA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0xAE, ( \z80_main param -> FlagOpIndexedIndirect XorA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0xB6, ( \z80_main param -> FlagOpIndexedIndirect OrA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0xBE, ( \z80_main param -> FlagOpIndexedIndirect CpA z80_main.ix param, IncreaseByThree, NineteenTStates ) )
        , ( 0x7E, ( ld_a_indirect_ix, IncreaseByThree, NineteenTStates ) )
        ]


doubleWithRegistersIY : Dict Int ( MainWithIndexRegisters -> Int -> DoubleWithRegisterChange, MediumPCIncrement, InstructionDuration )
doubleWithRegistersIY =
    Dict.fromList
        [ ( 0x26, ( ld_iy_h_n, IncreaseByThree, ElevenTStates ) )
        , ( 0x2E, ( ld_iy_l_n, IncreaseByThree, ElevenTStates ) )
        , ( 0x34, ( inc_indirect_iy, IncreaseByThree, TwentyThreeTStates ) )
        , ( 0x35, ( dec_indirect_iy, IncreaseByThree, TwentyThreeTStates ) )
        , ( 0x46, ( ld_b_indirect_iy, IncreaseByThree, SevenTStates ) )
        , ( 0x4E, ( ld_c_indirect_iy, IncreaseByThree, SevenTStates ) )
        , ( 0x56, ( ld_d_indirect_iy, IncreaseByThree, SevenTStates ) )
        , ( 0x5E, ( ld_e_indirect_iy, IncreaseByThree, SevenTStates ) )
        , ( 0x66, ( \z80_main param -> NewHRegisterIndirect (z80_main.iy + byte param), IncreaseByThree, NineteenTStates ) )
        , ( 0x6E, ( \z80_main param -> NewLRegisterIndirect (z80_main.iy + byte param), IncreaseByThree, NineteenTStates ) )
        , ( 0x86, ( \z80_main param -> FlagOpIndexedIndirect AddA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0x8E, ( \z80_main param -> FlagOpIndexedIndirect AdcA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0x96, ( \z80_main param -> FlagOpIndexedIndirect SubA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0x9E, ( \z80_main param -> FlagOpIndexedIndirect SbcA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0xA6, ( \z80_main param -> FlagOpIndexedIndirect AndA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0xAE, ( \z80_main param -> FlagOpIndexedIndirect XorA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0xB6, ( \z80_main param -> FlagOpIndexedIndirect OrA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0xBE, ( \z80_main param -> FlagOpIndexedIndirect CpA z80_main.iy param, IncreaseByThree, NineteenTStates ) )
        , ( 0x7E, ( ld_a_indirect_iy, IncreaseByThree, NineteenTStates ) )
        ]


maybeRelativeJump : Dict Int ( Int -> FlagRegisters -> JumpChange, InstructionDuration )
maybeRelativeJump =
    Dict.fromList
        [ ( 0x18, ( jr_n, TwelveTStates ) )
        , ( 0x20, ( jr_nz_d, SevenTStates ) )
        , ( 0x28, ( jr_z_d, SevenTStates ) )
        , ( 0x30, ( jr_nc_d, SevenTStates ) )
        , ( 0x38, ( jr_c_d, SevenTStates ) )
        , ( 0x3E, ( ld_a_n, SevenTStates ) )
        , ( 0xC6, ( add_a_n, SevenTStates ) )
        , ( 0xCE, ( adc_n, SevenTStates ) )
        , ( 0xD6, ( sub_n, SevenTStates ) )
        , ( 0xDE, ( sbc_a_n, SevenTStates ) )
        , ( 0xE6, ( and_n, SevenTStates ) )
        , ( 0xEE, ( xor_n, SevenTStates ) )
        , ( 0xF6, ( or_n, SevenTStates ) )
        , ( 0xFE, ( cp_n, SevenTStates ) )
        ]


type Single8BitChange
    = NewBRegister Int
    | NewCRegister Int
    | NewDRegister Int
    | NewERegister Int


type DoubleWithRegisterChange
    = RelativeJumpWithTimeOffset Single8BitChange (Maybe Int) Int
    | DoubleRegChangeStoreIndirect Int Int
    | NewHLRegisterValue Int
    | NewIXRegisterValue Int
    | NewIYRegisterValue Int
    | NewARegisterIndirect Int
    | NewBRegisterIndirect Int
    | NewCRegisterIndirect Int
    | NewDRegisterIndirect Int
    | NewERegisterIndirect Int
    | NewHRegisterIndirect Int
    | NewLRegisterIndirect Int
    | IndexedIndirectIncrement Int Int
    | IndexedIndirectDecrement Int Int
    | FlagOpIndexedIndirect FlagFunc Int Int


type JumpChange
    = ActualJump Int
    | NoJump
    | FlagJump FlagRegisters


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


ld_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_h_n z80_main param =
    -- case 0x26: HL=HL&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF) |> NewHLRegisterValue


ld_ix_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_ix_h_n z80_main param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.ix 0xFF) |> NewIXRegisterValue


ld_iy_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_iy_h_n z80_main param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.iy 0xFF) |> NewIYRegisterValue


ld_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_l_n z80_main param =
    -- case 0x2E: HL=HL&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.hl 0xFF00) |> NewHLRegisterValue


ld_ix_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_ix_l_n z80_main param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.ix 0xFF00) |> NewIXRegisterValue


ld_iy_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_iy_l_n z80_main param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.iy 0xFF00) |> NewIYRegisterValue


ld_b_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_b_indirect_ix z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    NewBRegisterIndirect address


ld_a_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_a_indirect_ix z80_main param =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
    NewARegisterIndirect address


ld_a_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_a_indirect_iy z80_main param =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
    NewARegisterIndirect address


ld_b_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_b_indirect_iy z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    NewBRegisterIndirect address


ld_c_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_c_indirect_ix z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    NewCRegisterIndirect address


ld_c_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_c_indirect_iy z80_main param =
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    NewCRegisterIndirect address


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


ld_indirect_hl_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_indirect_hl_n z80_main param =
    -- case 0x36: env.mem(HL,imm8()); time+=3; break;
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    DoubleRegChangeStoreIndirect z80_main.hl param


jr_n : Int -> FlagRegisters -> JumpChange
jr_n param _ =
    -- case 0x18: MP=PC=(char)(PC+1+(byte)env.mem(PC)); time+=8; break;
    -- This is just an inlined jr() call
    --z80 |> set_pc dest |> add_cpu_time 8
    ActualJump (byte param)


jr_nz_d : Int -> FlagRegisters -> JumpChange
jr_nz_d param z80_flags =
    -- case 0x20: if(Fr!=0) jr(); else imm8(); break;
    if z80_flags.fr /= 0 then
        ActualJump (byte param)

    else
        NoJump


jr_z_d : Int -> FlagRegisters -> JumpChange
jr_z_d param z80_flags =
    -- case 0x28: if(Fr==0) jr(); else imm8(); break;
    if z80_flags.fr == 0 then
        ActualJump (byte param)

    else
        NoJump


jr_nc_d : Int -> FlagRegisters -> JumpChange
jr_nc_d param z80_flags =
    -- case 0x30: if((Ff&0x100)==0) jr(); else imm8(); break;
    if Bitwise.and z80_flags.ff 0x0100 == 0 then
        ActualJump (byte param)

    else
        NoJump


jr_c_d : Int -> FlagRegisters -> JumpChange
jr_c_d param z80_flags =
    -- case 0x38: if((Ff&0x100)!=0) jr(); else imm8(); break;
    if Bitwise.and z80_flags.ff 0x0100 /= 0 then
        ActualJump (byte param)

    else
        NoJump


add_a_n : Int -> FlagRegisters -> JumpChange
add_a_n param z80_flags =
    -- case 0xC6: add(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        --env_1 = z80.env
        --z80_1 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
        flags =
            z80_flags |> z80_add param
    in
    --{ z80_1 | flags = flags }
    FlagJump flags


adc_n : Int -> FlagRegisters -> JumpChange
adc_n param z80_flags =
    -- case 0xCE: adc(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        flags =
            z80_flags |> adc param

        --env_1 = z80.env
    in
    --{z80 | pc = v.pc, env = { env_1 | time = v.time }, flags = flags }
    FlagJump flags


sub_n : Int -> FlagRegisters -> JumpChange
sub_n param z80_flags =
    -- case 0xD6: sub(imm8()); break;
    let
        --v = imm8 z80.pc z80.env.time rom48k z80.env.ram
        flags =
            z80_flags |> z80_sub param

        --env_1 = z80.env
    in
    --{ z80 | flags = flags, env = { env_1 | time = v.time }, pc = v.pc }
    FlagJump flags


sbc_a_n : Int -> FlagRegisters -> JumpChange
sbc_a_n param z80_flags =
    -- case 0xDE: sbc(imm8()); break;
    z80_flags |> sbc param |> FlagJump


and_n : Int -> FlagRegisters -> JumpChange
and_n param z80_flags =
    -- case 0xE6: and(imm8()); break;
    let
        --a =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        --
        --env_1 =
        --    z80.env
        --
        --z80_1 =
        --    { z80 | env = { env_1 | time = a.time }, pc = a.pc }
        flags =
            z80_flags |> z80_and param
    in
    --{ z80_1 | flags = flags }
    FlagJump flags


xor_n : Int -> FlagRegisters -> JumpChange
xor_n param z80_flags =
    -- case 0xEE: xor(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        --env_1 = z80.env
        --z80_1 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
        flags =
            z80_flags |> z80_xor param
    in
    --{ z80_1 | flags = flags }
    FlagJump flags


or_n : Int -> FlagRegisters -> JumpChange
or_n param z80_flags =
    -- case 0xF6: or(imm8()); break;
    let
        --a = imm8 z80.pc z80.env.time rom48k z80.env.ram
        --env_1 = z80.env
        --z80_1 = { z80 | env = { env_1 | time = a.time }, pc = a.pc }
        flags =
            z80_flags |> z80_or param
    in
    --{ z80_1 | flags = flags }
    FlagJump flags


cp_n : Int -> FlagRegisters -> JumpChange
cp_n param z80_flags =
    -- case 0xFE: cp(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        flags =
            z80_flags |> z80_cp param

        --env_1 =
        --    z80.env
    in
    --{ z80 | flags = flags, env = { env_1 | time = v.time }, pc = v.pc }
    FlagJump flags


ld_a_n : Int -> FlagRegisters -> JumpChange
ld_a_n param z80_flags =
    -- case 0x3E: A=imm8(); break;
    --let
    --v =
    --    imm8 z80.pc z80.env.time rom48k z80.env.ram
    --new_z80 = { z80 | env = v.env, pc = v.pc }
    --in
    --{ new_z80 | flags = { z80_flags | a = v.value } }
    --CpuTimeWithFlagsAndPc v.time { z80_flags | a = v.value } v.pc
    FlagJump { z80_flags | a = param }


inc_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
inc_indirect_ix z80_main param =
    -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement z80_main.ix param


inc_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
inc_indirect_iy z80_main param =
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectIncrement z80_main.iy param


dec_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
dec_indirect_ix z80_main param =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement z80_main.ix param


dec_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
dec_indirect_iy z80_main param =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    IndexedIndirectDecrement z80_main.iy param


ld_d_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_d_indirect_ix z80_main param =
    --case 0x56: D=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    NewDRegisterIndirect address



--    NewDIndexedIndirect z80_main.ix param


ld_e_indirect_ix : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_e_indirect_ix z80_main param =
    --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.ix + byte param
    in
    NewERegisterIndirect address



--NewEIndexedIndirect z80_main.ix param


ld_d_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_d_indirect_iy z80_main param =
    --case 0x56: D=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    NewDRegisterIndirect address



--NewDIndexedIndirect z80_main.iy param


ld_e_indirect_iy : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_e_indirect_iy z80_main param =
    --case 0x5E: E=env.mem(getd(xy)); time+=3; break;
    let
        address =
            z80_main.iy + byte param
    in
    NewERegisterIndirect address
