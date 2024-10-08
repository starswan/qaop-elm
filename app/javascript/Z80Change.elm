module Z80Change exposing (..)

import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (Z80)


type Z80Change
    = CRegister Int
      --| BRegister Int
    | OnlyFlags FlagRegisters
    | BCRegister Int Int
    | DERegister Int Int
    | ERegister Int
    | FlagsWithBRegister FlagRegisters Int
    | FlagsWithCRegister FlagRegisters Int
    | FlagsWithDRegister FlagRegisters Int
    | FlagsWithERegister FlagRegisters Int


applyZ80Change : Z80Change -> Z80 -> Z80
applyZ80Change change z80 =
    case change of
        CRegister int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | c = int } }

        OnlyFlags flagRegisters ->
            { z80 | flags = flagRegisters }

        BCRegister b_value c_value ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | b = b_value, c = c_value } }

        FlagsWithBRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | b = int } }

        FlagsWithCRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | c = int } }

        DERegister d_value e_value ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | d = d_value, e = e_value } }

        ERegister int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | e = int } }

        FlagsWithDRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | d = int } }

        FlagsWithERegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | e = int } }
