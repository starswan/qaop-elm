module CBB0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (executeSingleInstruction)
import Z80Env exposing (mem, setMem)
import Z80Rom

suite : Test
suite =
   let
       addr = 0x5800
       sp = 0xF765
       hl = 0x1234
       old_z80 = Z80.constructor
       old_z80env = old_z80.env
       z80main = old_z80.main
       z80 = { old_z80 | pc = addr, env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }
       flags = z80.flags
       z80env = z80.env
       z80rom = Z80Rom.constructor
   in
       describe "Bit instructions (CB)"
         [
            test "0xFD 0xCB nn 0xBE RES 7, (IY + n) -ve" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0xCB
                               |> setMem (addr + 2) 0xFE
                               |> setMem (addr + 3) 0xBE
                               |> setMem 0xA07E 0xFF
                  new_z80 = executeSingleInstruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | iy=0xA080, hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0xA07E new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 4), 0x7F) (new_z80.pc, mem_value.value)
         ]
