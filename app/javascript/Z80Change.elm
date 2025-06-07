module Z80Change exposing (..)

import Z80Byte exposing (Z80Byte)
import Z80Core exposing (Z80Core)
import Z80Env exposing (setMem)
import Z80Flags exposing (FlagRegisters, IntWithFlags, Z80ByteWithFlags)
import Z80Word exposing (Z80Word)


type Z80Change
    = FlagsWithBRegister Z80ByteWithFlags
    | FlagsWithCRegister Z80ByteWithFlags
    | FlagsWithDRegister Z80ByteWithFlags
    | FlagsWithERegister FlagRegisters Z80Byte
    | FlagsWithHLRegister FlagRegisters Z80Word
    | FlagsWithIXRegister FlagRegisters Z80Word
    | FlagsWithIYRegister FlagRegisters Z80Word
    | Z80RegisterB Z80Byte
    | Z80RegisterC Z80Byte
    | Z80ChangeFlags FlagRegisters
    | Z80ChangeSetIndirect Z80Word Z80Byte


type FlagChange
    = OnlyFlags FlagRegisters
    | FlagChangeB Z80Byte
    | FlagChangeC Z80Byte
    | FlagChangeD Z80Byte
    | FlagChangeE Z80Byte
    | FlagChangeH Z80Byte
    | FlagChangeL Z80Byte
    | ReturnWithPop
    | EmptyFlagChange
    | FlagChangePush Z80Word


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
