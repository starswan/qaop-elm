module Z80Change exposing (..)

import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (Z80)


type Z80Change
    = FlagsWithBRegister FlagRegisters Int
    | FlagsWithCRegister FlagRegisters Int
    | FlagsWithDRegister FlagRegisters Int
    | FlagsWithERegister FlagRegisters Int
    | HLRegister Int
    | FlagsWithHLRegister FlagRegisters Int
    | Z80RegisterB Int
    | Z80RegisterC Int


type FlagChange
    = OnlyFlags FlagRegisters
    | FlagChangeB Int
    | FlagChangeC Int


applyZ80Change : Z80Change -> Z80 -> Z80
applyZ80Change change z80 =
    case change of
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

        HLRegister int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | hl = int } }

        FlagsWithHLRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | hl = int } }

        Z80RegisterB int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | b = int } }

        Z80RegisterC int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | c = int } }
