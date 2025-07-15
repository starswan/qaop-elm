module Z80Change exposing (..)

import Z80Core exposing (Z80Core)
import Z80Env exposing (setMem)
import Z80Flags exposing (FlagRegisters, IntWithFlags)


type Z80Change
    = FlagsWithBRegister IntWithFlags
    | FlagsWithCRegister IntWithFlags
    | FlagsWithDRegister IntWithFlags
    | FlagsWithERegister FlagRegisters Int
    | FlagsWithHLRegister FlagRegisters Int
    | FlagsWithIXRegister FlagRegisters Int
    | FlagsWithIYRegister FlagRegisters Int
    | Z80RegisterB Int
    | Z80RegisterC Int
    | Z80ChangeFlags FlagRegisters
    | Z80ChangeSetIndirect Int Int
    | JustIXRegister Int
    | JustIYRegister Int


type FlagChange
    = OnlyFlags FlagRegisters
    | FlagChangeB Int
    | FlagChangeC Int
    | FlagChangeD Int
    | FlagChangeE Int
    | FlagChangeH Int
    | FlagChangeL Int
    | ReturnWithPop
    | EmptyFlagChange
    | FlagChangePush Int


applyZ80Change : Z80Change -> Z80Core -> Z80Core
applyZ80Change change z80 =
    case change of
        FlagsWithBRegister intWithFlags ->
            let
                main =
                    z80.main
            in
            { z80 | flags = intWithFlags.flags, main = { main | b = intWithFlags.value } }

        FlagsWithCRegister intWithFlags ->
            let
                main =
                    z80.main
            in
            { z80 | flags = intWithFlags.flags, main = { main | c = intWithFlags.value } }

        FlagsWithDRegister intWithFlags ->
            let
                main =
                    z80.main
            in
            { z80 | flags = intWithFlags.flags, main = { main | d = intWithFlags.value } }

        FlagsWithERegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | e = int } }

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

        Z80ChangeFlags flagRegisters ->
            { z80 | flags = flagRegisters }

        Z80ChangeSetIndirect addr int ->
            let
                env =
                    z80.env |> setMem addr int
            in
            { z80 | env = env }

        FlagsWithIXRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | ix = int } }

        FlagsWithIYRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | iy = int } }

        JustIXRegister int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | ix = int } }

        JustIYRegister int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | iy = int } }
