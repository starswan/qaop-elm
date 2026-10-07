module IXIYChange exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime)
import Utils exposing (BitTest, bitMaskFromBit, clearBit, inverseBitMaskFromBit, setBit, shiftLeftBy8, shiftRightBy8)
import Z80Change exposing (Shifter(..))
import Z80Core exposing (CoreChange(..), RareCoreChange(..), Z80Core)
import Z80Env exposing (setMem)
import Z80Flags exposing (FlagRegisters, IntWithFlags, c_F53, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7, testBit)
import Z80Mem exposing (getMem8)
import Z80Registers exposing (ChangeMainRegister(..), ChangeSingle(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


type IXIYChange
    = IndirectBitTest BitTest (MainWithIndexRegisters -> Int)
    | RegisterIndirectWithShifter Shifter ChangeMainRegister Int
    | RegisterChangeIndexShifter Shifter Int
    | FlagsIndirectWithShifter Shifter Int
    | ResetBitIndirectWithCopy BitTest ChangeMainRegister (MainWithIndexRegisters -> Int)
    | IndirectBitReset BitTest (MainWithIndexRegisters -> Int)
    | ResetBitIndirectA BitTest (MainWithIndexRegisters -> Int)
    | SetBitIndirectWithCopy BitTest ChangeMainRegister (MainWithIndexRegisters -> Int)
    | IndirectBitSet BitTest (MainWithIndexRegisters -> Int)
    | SetBitIndirectA BitTest (MainWithIndexRegisters -> Int)
    | TransformMainRegistersCB (MainWithIndexRegisters -> MainWithIndexRegisters)
    | SingleRegisterChange ChangeSingle (MainWithIndexRegisters -> Int)
    | IXIYFlagChangeFunc (FlagRegisters -> FlagRegisters)


applyIXIYChange : CpuTimeCTime -> IXIYChange -> Z80ROM -> Z80Core -> CoreChange
applyIXIYChange clockTime z80changeData rom48k z80_core =
    case z80changeData of
        IndirectBitTest bitTest address_func ->
            -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
            let
                mp_address =
                    z80_core.main |> address_func

                ( value, newTime ) =
                    z80_core.env |> getMem8 mp_address clockTime rom48k

                new_flags =
                    z80_core.flags |> testBit bitTest value
            in
            { new_flags
                | ff = new_flags.ff |> Bitwise.and (Bitwise.complement c_F53) |> Bitwise.or (mp_address |> shiftRightBy8 |> Bitwise.and c_F53)
            }
                |> FlagsOnly

        TransformMainRegistersCB f ->
            z80_core.main |> f |> MainOnly

        IXIYFlagChangeFunc f ->
            f z80_core.flags |> FlagsOnly

        RegisterChangeIndexShifter shifter raw_addr ->
            z80_core |> applyShifter shifter (raw_addr |> Bitwise.and 0xFFFF) clockTime rom48k

        IndirectBitReset bitMask addr_f ->
            let
                old_env =
                    z80_core.env

                addr =
                    z80_core.main |> addr_f

                ( value, newTime ) =
                    old_env |> getMem8 addr clockTime rom48k

                new_value =
                    bitMask |> inverseBitMaskFromBit |> Bitwise.and value
            in
            SetMem8 addr new_value

        IndirectBitSet bitMask addr_f ->
            let
                raw_addr =
                    z80_core.main |> addr_f

                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                ( value, newTime ) =
                    z80_core.env |> getMem8 addr clockTime rom48k

                new_value =
                    bitMask |> bitMaskFromBit |> Bitwise.or value
            in
            SetMem8 addr new_value

        SingleRegisterChange changeOneRegister int_f ->
            let
                z80_main =
                    z80_core.main

                int =
                    z80_main |> int_f
            in
            case changeOneRegister of
                ChangeSingleH ->
                    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 int) } |> MainOnly

                ChangeSingleL ->
                    { z80_main | hl = Bitwise.or (Bitwise.and z80_main.hl 0xFF00) int } |> MainOnly

        RegisterIndirectWithShifter shifterFunc changeOneRegister raw_addr ->
            let
                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                ( input, newTime ) =
                    z80_core.env |> getMem8 addr clockTime rom48k

                value =
                    case shifterFunc of
                        Shifter0 ->
                            shifter0 input z80_core.flags

                        Shifter1 ->
                            shifter1 input z80_core.flags

                        Shifter2 ->
                            shifter2 input z80_core.flags

                        Shifter3 ->
                            shifter3 input z80_core.flags

                        Shifter4 ->
                            shifter4 input z80_core.flags

                        Shifter5 ->
                            shifter5 input z80_core.flags

                        Shifter6 ->
                            shifter6 input z80_core.flags

                        Shifter7 ->
                            shifter7 input z80_core.flags

                main =
                    z80_core.main

                new_main =
                    case changeOneRegister of
                        ChangeMainB ->
                            { main | b = value.value }

                        ChangeMainC ->
                            { main | c = value.value }

                        ChangeMainD ->
                            { main | d = value.value }

                        ChangeMainE ->
                            { main | e = value.value }

                        ChangeMainH ->
                            { main | hl = Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_core.main.hl 0xFF) }

                        ChangeMainL ->
                            { main | hl = Bitwise.or value.value (Bitwise.and z80_core.main.hl 0xFF00) }

                old_env =
                    z80_core.env

                ( env_2, newNew ) =
                    old_env |> setMem addr value.value newTime
            in
            { z80_core | main = new_main, flags = value.flags, env = env_2 } |> CoreOnly |> RareChange

        SetBitIndirectWithCopy bitTest changeOneRegister addr_f ->
            let
                old_env =
                    z80_core.env

                raw_addr =
                    z80_core.main |> addr_f

                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                ( input, newTime ) =
                    old_env |> getMem8 addr clockTime rom48k

                value =
                    input |> setBit bitTest

                main =
                    z80_core.main

                new_main =
                    case changeOneRegister of
                        ChangeMainB ->
                            { main | b = value }

                        ChangeMainC ->
                            { main | c = value }

                        ChangeMainD ->
                            { main | d = value }

                        ChangeMainE ->
                            { main | e = value }

                        ChangeMainH ->
                            { main | hl = Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80_core.main.hl 0xFF) }

                        ChangeMainL ->
                            { main | hl = Bitwise.or value (Bitwise.and z80_core.main.hl 0xFF00) }

                ( env_2, newTime2 ) =
                    old_env |> setMem addr value newTime
            in
            { z80_core | main = new_main, env = env_2 } |> CoreOnly |> RareChange

        ResetBitIndirectWithCopy bitTest changeOneRegister addr_f ->
            let
                old_env =
                    z80_core.env

                raw_addr =
                    z80_core.main |> addr_f

                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                ( input, newTime1 ) =
                    old_env |> getMem8 addr clockTime rom48k

                value =
                    input |> clearBit bitTest

                main =
                    z80_core.main

                new_main =
                    case changeOneRegister of
                        ChangeMainB ->
                            { main | b = value }

                        ChangeMainC ->
                            { main | c = value }

                        ChangeMainD ->
                            { main | d = value }

                        ChangeMainE ->
                            { main | e = value }

                        ChangeMainH ->
                            { main | hl = Bitwise.or (value |> shiftLeftBy8) (Bitwise.and z80_core.main.hl 0xFF) }

                        ChangeMainL ->
                            { main | hl = Bitwise.or value (Bitwise.and z80_core.main.hl 0xFF00) }

                ( env_2, newTime ) =
                    old_env |> setMem addr value newTime1
            in
            { z80_core | main = new_main, env = env_2 } |> CoreOnly |> RareChange

        FlagsIndirectWithShifter shifterFunc raw_addr ->
            let
                address =
                    raw_addr |> Bitwise.and 0xFFFF

                ( value, newTime ) =
                    z80_core.env |> getMem8 address clockTime rom48k

                result =
                    case shifterFunc of
                        Shifter0 ->
                            z80_core.flags |> shifter0 value

                        Shifter1 ->
                            z80_core.flags |> shifter1 value

                        Shifter2 ->
                            z80_core.flags |> shifter2 value

                        Shifter3 ->
                            z80_core.flags |> shifter3 value

                        Shifter4 ->
                            z80_core.flags |> shifter4 value

                        Shifter5 ->
                            z80_core.flags |> shifter5 value

                        Shifter6 ->
                            z80_core.flags |> shifter6 value

                        Shifter7 ->
                            z80_core.flags |> shifter7 value

                newFlags =
                    result.flags
            in
            SetMem8Flags address { value = result.value, flags = { newFlags | a = result.value } }

        SetBitIndirectA bitTest addr_f ->
            let
                raw_addr =
                    z80_core.main |> addr_f

                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                ( input, newTime ) =
                    z80_core.env |> getMem8 addr clockTime rom48k

                value =
                    input |> setBit bitTest

                flags =
                    z80_core.flags
            in
            SetMem8Flags addr { value = value, flags = { flags | a = value } }

        ResetBitIndirectA bitTest addr_f ->
            let
                raw_addr =
                    z80_core.main |> addr_f

                addr =
                    raw_addr |> Bitwise.and 0xFFFF

                ( input, newTime ) =
                    z80_core.env |> getMem8 addr clockTime rom48k

                value =
                    input |> clearBit bitTest

                flags =
                    z80_core.flags
            in
            SetMem8Flags addr { value = value, flags = { flags | a = value } }


applyShifter : Shifter -> Int -> CpuTimeCTime -> Z80ROM -> Z80Core -> CoreChange
applyShifter shifterFunc addr cpu_time rom48k z80 =
    let
        ( value, newTime ) =
            z80.env |> getMem8 addr cpu_time rom48k

        result : IntWithFlags
        result =
            case shifterFunc of
                Shifter0 ->
                    z80.flags |> shifter0 value

                Shifter1 ->
                    z80.flags |> shifter1 value

                Shifter2 ->
                    z80.flags |> shifter2 value

                Shifter3 ->
                    z80.flags |> shifter3 value

                Shifter4 ->
                    z80.flags |> shifter4 value

                Shifter5 ->
                    z80.flags |> shifter5 value

                Shifter6 ->
                    z80.flags |> shifter6 value

                Shifter7 ->
                    z80.flags |> shifter7 value
    in
    SetMem8Flags addr result
