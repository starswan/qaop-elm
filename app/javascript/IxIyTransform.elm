module IxIyTransform exposing (..)

import Bitwise
import Dict exposing (Dict)
import RegisterChange exposing (RegisterChange(..))
import Utils exposing (shiftRightBy8)
import Z80Flags exposing (FlagFunc(..))
import Z80Types exposing (MainWithIndexRegisters)


ixSingleByteMain : Dict Int (MainWithIndexRegisters -> RegisterChange)
ixSingleByteMain =
    Dict.fromList
        [ -- case 0x23: xy=(char)(xy+1); time+=2; break;
          ( 0xDD23, \z80_main -> ChangeRegisterIX (Bitwise.and (z80_main.ix + 1) 0xFFFF) )
        , ( 0xDD2B, dec_ix )
        , ( 0xDD44, ld_b_ix_h )
        , ( 0xDD45, ld_b_ix_l )
        , ( 0xDD4C, ld_c_ix_h )
        , ( 0xDD4D, ld_c_ix_l )
        , ( 0xDD54, ld_d_ix_h )
        , ( 0xDD55, ld_d_ix_l )
        , ( 0xDD5C, ld_e_ix_h )
        , ( 0xDD5D, ld_e_ix_l )
        , ( 0xDD60, \z80_main -> ChangeRegisterIXH z80_main.b )
        , ( 0xDD61, \z80_main -> ChangeRegisterIXH z80_main.c )
        , ( 0xDD62, \z80_main -> ChangeRegisterIXH z80_main.d )
        , ( 0xDD63, \z80_main -> ChangeRegisterIXH z80_main.e )
        , ( 0xDD65, \z80_main -> ChangeRegisterIXH (Bitwise.and z80_main.ix 0xFF) )
        , ( 0xDD68, \z80_main -> ChangeRegisterIXL z80_main.b )
        , ( 0xDD69, \z80_main -> ChangeRegisterIXL z80_main.c )
        , ( 0xDD6A, \z80_main -> ChangeRegisterIXL z80_main.d )
        , ( 0xDD6C, \z80_main -> ChangeRegisterIXL (z80_main.ix |> shiftRightBy8) )
        , ( 0xDD6B, \z80_main -> ChangeRegisterIXL z80_main.e )
        , ( 0xDD7C, \z80_main -> ChangeRegisterA (z80_main.ix |> shiftRightBy8) )
        , ( 0xDD7D, \z80_main -> ChangeRegisterA (z80_main.ix |> Bitwise.and 0xFF) )
        , ( 0xDD84, \z80_main -> SingleEnvFlagFunc AddA (z80_main.ix |> shiftRightBy8) )
        , ( 0xDD85, \z80_main -> SingleEnvFlagFunc AddA (z80_main.ix |> Bitwise.and 0xFF) )

        -- case 0x8C: adc(xy>>>8); break;
        , ( 0xDD8C, \z80_main -> SingleEnvFlagFunc AdcA (z80_main.ix |> shiftRightBy8) )
        , ( 0xDD8D, \z80_main -> SingleEnvFlagFunc AdcA (z80_main.ix |> Bitwise.and 0xFF) )
        , ( 0xDD94, \z80_main -> SingleEnvFlagFunc SubA (z80_main.ix |> shiftRightBy8) )
        , ( 0xDD95, \z80_main -> SingleEnvFlagFunc SubA (z80_main.ix |> Bitwise.and 0xFF) )
        , ( 0xDD9C, \z80_main -> SingleEnvFlagFunc SbcA (z80_main.ix |> shiftRightBy8) )
        , ( 0xDD9D, \z80_main -> SingleEnvFlagFunc SbcA (z80_main.ix |> Bitwise.and 0xFF) )
        , ( 0xDDA4, \z80_main -> SingleEnvFlagFunc AndA (z80_main.ix |> shiftRightBy8) )
        , ( 0xDDA5, \z80_main -> SingleEnvFlagFunc AndA (z80_main.ix |> Bitwise.and 0xFF) )
        , ( 0xDDAC, \z80_main -> SingleEnvFlagFunc XorA (z80_main.ix |> shiftRightBy8) )
        , ( 0xDDAD, \z80_main -> SingleEnvFlagFunc XorA (z80_main.ix |> Bitwise.and 0xFF) )
        , ( 0xDDB4, \z80_main -> SingleEnvFlagFunc OrA (z80_main.ix |> shiftRightBy8) )
        , ( 0xDDB5, \z80_main -> SingleEnvFlagFunc OrA (z80_main.ix |> Bitwise.and 0xFF) )
        , ( 0xDDBC, \z80_main -> SingleEnvFlagFunc CpA (z80_main.ix |> shiftRightBy8) )
        , ( 0xDDBD, \z80_main -> SingleEnvFlagFunc CpA (z80_main.ix |> Bitwise.and 0xFF) )
        , ( 0xDDF9, \z80_main -> RegChangeNewSP z80_main.ix )
        ]


iySingleByteMain : Dict Int (MainWithIndexRegisters -> RegisterChange)
iySingleByteMain =
    Dict.fromList
        [ -- case 0x23: xy=(char)(xy+1); time+=2; break;
          ( 0xFD23, \z80_main -> ChangeRegisterIY (Bitwise.and (z80_main.iy + 1) 0xFFFF) )
        , ( 0xFD2B, dec_iy )
        , ( 0xFD44, ld_b_iy_h )
        , ( 0xFD45, ld_b_iy_l )
        , ( 0xFD4C, ld_c_iy_h )
        , ( 0xFD4D, ld_c_iy_l )
        , ( 0xFD54, ld_d_iy_h )
        , ( 0xFD55, ld_d_iy_l )
        , ( 0xFD5C, ld_e_iy_h )
        , ( 0xFD5D, ld_e_iy_l )
        , ( 0xFD60, \z80_main -> ChangeRegisterIYH z80_main.b )
        , ( 0xFD61, \z80_main -> ChangeRegisterIYH z80_main.c )
        , ( 0xFD62, \z80_main -> ChangeRegisterIYH z80_main.d )
        , ( 0xFD63, \z80_main -> ChangeRegisterIYH z80_main.e )
        , ( 0xFD65, \z80_main -> ChangeRegisterIYH (Bitwise.and z80_main.iy 0xFF) )
        , ( 0xFD68, \z80_main -> ChangeRegisterIYL z80_main.b )
        , ( 0xFD69, \z80_main -> ChangeRegisterIYL z80_main.c )
        , ( 0xFD6A, \z80_main -> ChangeRegisterIYL z80_main.d )
        , ( 0xFD6B, \z80_main -> ChangeRegisterIYL z80_main.e )
        , ( 0xFD6C, \z80_main -> ChangeRegisterIYL (z80_main.iy |> shiftRightBy8) )
        , ( 0xFD7C, \z80_main -> ChangeRegisterA (z80_main.iy |> shiftRightBy8) )
        , ( 0xFD7D, \z80_main -> ChangeRegisterA (z80_main.iy |> Bitwise.and 0xFF) )
        , ( 0xFD84, \z80_main -> SingleEnvFlagFunc AddA (z80_main.iy |> shiftRightBy8) )
        , ( 0xFD85, \z80_main -> SingleEnvFlagFunc AddA (z80_main.iy |> Bitwise.and 0xFF) )

        -- case 0x8C: adc(xy>>>8); break;
        , ( 0xFD8C, \z80_main -> SingleEnvFlagFunc AdcA (z80_main.iy |> shiftRightBy8) )
        , ( 0xFD8D, \z80_main -> SingleEnvFlagFunc AdcA (z80_main.iy |> Bitwise.and 0xFF) )
        , ( 0xFD94, \z80_main -> SingleEnvFlagFunc SubA (z80_main.iy |> shiftRightBy8) )
        , ( 0xFD95, \z80_main -> SingleEnvFlagFunc SubA (z80_main.iy |> Bitwise.and 0xFF) )
        , ( 0xFD9C, \z80_main -> SingleEnvFlagFunc SbcA (z80_main.iy |> shiftRightBy8) )
        , ( 0xFD9D, \z80_main -> SingleEnvFlagFunc SbcA (z80_main.iy |> Bitwise.and 0xFF) )
        , ( 0xFDA4, \z80_main -> SingleEnvFlagFunc AndA (z80_main.iy |> shiftRightBy8) )
        , ( 0xFDA5, \z80_main -> SingleEnvFlagFunc AndA (z80_main.iy |> Bitwise.and 0xFF) )
        , ( 0xFDAC, \z80_main -> SingleEnvFlagFunc XorA (z80_main.iy |> shiftRightBy8) )
        , ( 0xFDAD, \z80_main -> SingleEnvFlagFunc XorA (z80_main.iy |> Bitwise.and 0xFF) )
        , ( 0xFDB4, \z80_main -> SingleEnvFlagFunc OrA (z80_main.iy |> shiftRightBy8) )
        , ( 0xFDB5, \z80_main -> SingleEnvFlagFunc OrA (z80_main.iy |> Bitwise.and 0xFF) )
        , ( 0xFDBC, \z80_main -> SingleEnvFlagFunc CpA (z80_main.iy |> shiftRightBy8) )
        , ( 0xFDBD, \z80_main -> SingleEnvFlagFunc CpA (z80_main.iy |> Bitwise.and 0xFF) )
        , ( 0xFDF9, \z80_main -> RegChangeNewSP z80_main.iy )
        ]


dec_ix : MainWithIndexRegisters -> RegisterChange
dec_ix z80_main =
    -- case 0x2B: xy=(char)(xy-1); time+=2; break;
    let
        new_xy =
            Bitwise.and (z80_main.ix - 1) 0xFFFF
    in
    ChangeRegisterIX new_xy


dec_iy : MainWithIndexRegisters -> RegisterChange
dec_iy z80_main =
    -- case 0x2B: xy=(char)(xy-1); time+=2; break;
    let
        new_xy =
            Bitwise.and (z80_main.iy - 1) 0xFFFF
    in
    ChangeRegisterIY new_xy


ld_b_ix_h : MainWithIndexRegisters -> RegisterChange
ld_b_ix_h z80_main =
    -- case 0x44: B=HL>>>8; break;
    -- case 0x44: B=xy>>>8; break;
    --z80 |> set_b (get_h ixiyhl z80.main)
    ChangeRegisterB (shiftRightBy8 z80_main.ix)


ld_c_ix_h : MainWithIndexRegisters -> RegisterChange
ld_c_ix_h z80_main =
    -- case 0x44: B=HL>>>8; break;
    -- case 0x44: B=xy>>>8; break;
    --z80 |> set_b (get_h ixiyhl z80.main)
    ChangeRegisterC (shiftRightBy8 z80_main.ix)


ld_d_ix_h : MainWithIndexRegisters -> RegisterChange
ld_d_ix_h z80_main =
    --    -- case 0x54: D=HL>>>8; break;
    ChangeRegisterD (shiftRightBy8 z80_main.ix)


ld_e_ix_h : MainWithIndexRegisters -> RegisterChange
ld_e_ix_h z80_main =
    -- case 0x5C: E=HL>>>8; break;
    ChangeRegisterE (shiftRightBy8 z80_main.ix)


ld_b_iy_h : MainWithIndexRegisters -> RegisterChange
ld_b_iy_h z80_main =
    -- case 0x44: B=HL>>>8; break;
    -- case 0x44: B=xy>>>8; break;
    --z80 |> set_b (get_h ixiyhl z80.main)
    ChangeRegisterB (shiftRightBy8 z80_main.iy)


ld_c_iy_h : MainWithIndexRegisters -> RegisterChange
ld_c_iy_h z80_main =
    -- case 0x44: B=HL>>>8; break;
    -- case 0x44: B=xy>>>8; break;
    --z80 |> set_b (get_h ixiyhl z80.main)
    ChangeRegisterC (shiftRightBy8 z80_main.iy)


ld_d_iy_h : MainWithIndexRegisters -> RegisterChange
ld_d_iy_h z80_main =
    --    -- case 0x54: D=HL>>>8; break;
    ChangeRegisterD (shiftRightBy8 z80_main.iy)


ld_e_iy_h : MainWithIndexRegisters -> RegisterChange
ld_e_iy_h z80_main =
    -- case 0x5C: E=HL>>>8; break;
    ChangeRegisterE (shiftRightBy8 z80_main.iy)


ld_b_ix_l : MainWithIndexRegisters -> RegisterChange
ld_b_ix_l z80_main =
    -- case 0x45: B=HL&0xFF; break;
    -- case 0x45: B=xy&0xFF; break;
    --  z80 |> set_b (get_l ixiyhl z80.main)
    ChangeRegisterB (Bitwise.and z80_main.ix 0xFF)


ld_c_ix_l : MainWithIndexRegisters -> RegisterChange
ld_c_ix_l z80_main =
    -- case 0x45: B=HL&0xFF; break;
    -- case 0x45: B=xy&0xFF; break;
    --  z80 |> set_b (get_l ixiyhl z80.main)
    ChangeRegisterC (Bitwise.and z80_main.ix 0xFF)


ld_d_ix_l : MainWithIndexRegisters -> RegisterChange
ld_d_ix_l z80_main =
    --    -- case 0x55: D=HL&0xFF; break;
    ChangeRegisterD (Bitwise.and z80_main.ix 0xFF)


ld_e_ix_l : MainWithIndexRegisters -> RegisterChange
ld_e_ix_l z80_main =
    -- case 0x5D: E=HL&0xFF; break;
    ChangeRegisterE (Bitwise.and z80_main.ix 0xFF)


ld_b_iy_l : MainWithIndexRegisters -> RegisterChange
ld_b_iy_l z80_main =
    -- case 0x45: B=HL&0xFF; break;
    -- case 0x45: B=xy&0xFF; break;
    --  z80 |> set_b (get_l ixiyhl z80.main)
    ChangeRegisterB (Bitwise.and z80_main.iy 0xFF)


ld_c_iy_l : MainWithIndexRegisters -> RegisterChange
ld_c_iy_l z80_main =
    -- case 0x45: B=HL&0xFF; break;
    -- case 0x45: B=xy&0xFF; break;
    --  z80 |> set_b (get_l ixiyhl z80.main)
    ChangeRegisterC (Bitwise.and z80_main.iy 0xFF)


ld_d_iy_l : MainWithIndexRegisters -> RegisterChange
ld_d_iy_l z80_main =
    --    -- case 0x55: D=HL&0xFF; break;
    ChangeRegisterD (Bitwise.and z80_main.iy 0xFF)


ld_e_iy_l : MainWithIndexRegisters -> RegisterChange
ld_e_iy_l z80_main =
    -- case 0x5D: E=HL&0xFF; break;
    ChangeRegisterE (Bitwise.and z80_main.iy 0xFF)
