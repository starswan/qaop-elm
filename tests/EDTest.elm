module EDTest exposing (..)

import Bitwise exposing (shiftRightBy)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (mem, mem16, set_mem, set_mem16)

suite : Test
suite =
   let
       addr = 30000
       sp = 0x8765
       old_z80 = Z80.constructor
       old_z80env = old_z80.env
       z80 = { old_z80 | pc = addr, env = { old_z80env | sp = sp } }
       flags = z80.flags
       z80env = z80.env
       z80main = z80.main
   in
   describe "Z80.execute_instruction" -- Nest as many descriptions as you like.
      [
         describe "ED instructions"
         [
            test "0xED 0x6F RLD" <|
            \_ ->
               let
                  new_env = z80env |> set_mem addr 0xED
                                   |> set_mem (addr + 1) 0x6F
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x47 } }
               in
                  Expect.equal ((addr + 2), 0x40) (new_z80.pc, new_z80.flags.a)
         ]
      ]
