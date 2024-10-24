module Z80Test exposing (..)

import Bitwise exposing (shiftRightBy)
import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (mem, mem16, setMem, setMem16)
import Z80Rom

suite : Test
suite =
   let
       addr = 30000
       old_z80 = Z80.constructor
       z80 = { old_z80 | pc = addr }
       flags = z80.flags
       z80env = z80.env
       z80main = z80.main
       z80rom = Z80Rom.constructor
   in
   describe "Z80.execute_instruction" -- Nest as many descriptions as you like.
      [
         describe "16 bit load immediate"
         [
            test "0x21 - LD HL, nn" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x21
                               |> setMem (addr + 1) 0xC6
                               |> setMem (addr + 2) 0x15
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 3), 0x15C6) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x21 - LD IY, nn" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0x21
                               |> setMem (addr + 2) 0x05
                               |> setMem (addr + 3) 0x07
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 4), 0x0705) (new_z80.pc, new_z80.main.iy)
         ],
         describe "ADD HL, 16-bit"
         [
            test "0x29 ADD HL,HL" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x29
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x4334 } }
               in
                  Expect.equal ((addr + 1), 0x8668) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x29 ADD IX, IX" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction z80rom { z80 | env = z80env |> setMem addr 0xDD |> setMem (addr + 1) 0x29,
                                                             main = { z80main | ix = 0x05, b = 0x01, c = 0x02, hl = 0x3445 } }
               in
                  Expect.equal ((addr + 2), 0x3445, 0x000A) (z80_after_01.pc, z80_after_01.main.hl, z80_after_01.main.ix)
            ,test "0x39 ADD HL,SP" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x39
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x4321 },
                                                        main = { z80main | hl = 0x1234 } }
               in
                  Expect.equal ((addr + 1), 0x5555) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x39 ADD IY,SP" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0x39
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x4321 },
                                                        main = { z80main | iy = 0x1234,hl = 0x4234 } }
               in
                  Expect.equal ((addr + 2), 0x5555) (new_z80.pc, new_z80.main.iy)
         ],
         describe "DEC 16 bit"
         [
            test "0x2B DEC HL" <|
            \_ ->
               let
                  new_env = z80env
                            |> setMem addr 0x2B
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x64FF) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x2B DEC IX" <|
            \_ ->
               let
                  new_env = z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0x2B
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | ix = 0,hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x6500, 0xFFFF) (new_z80.pc, new_z80.main.hl, new_z80.main.ix)
            ,test "0x3B DEC SP" <|
            \_ ->
               let
                  new_env = z80env
                            |> setMem addr 0x3B
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8756 },
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x8755) (new_z80.pc, new_z80.env.sp)
         ],
         describe "INC 8 bit"
         [
            test "0x24 INC H" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x24
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | iy = 0x6545, hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x6645, 0x6545) (new_z80.pc, new_z80.main.hl, new_z80.main.iy)
            ,test "0xFD 0x24 INC IYH" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0x24
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | iy = 0x6545, hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x6545, 0x6645) (new_z80.pc, new_z80.main.hl, new_z80.main.iy)
            ,test "0x25 DEC H" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x25
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x6445) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x25 DEC IXH" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0x25
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | ix = 0x0045, hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x6545, 0xFF45) (new_z80.pc, new_z80.main.hl, new_z80.main.ix)
            ,test "0x2C INC L" <|
             \_ ->
                let
                   new_env = z80env
                             |> setMem addr 0x2C
                   new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                         main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
                in
                   Expect.equal ((addr + 1), 0x6501) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x2C INC IYL" <|
             \_ ->
                let
                   new_env = z80env
                             |> setMem addr 0xFD
                             |> setMem (addr + 1) 0x2C
                   new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                         main = { z80main | iy = 0x6500, hl = 0x6500 }, flags = { flags | a = 0x39 } }
                in
                   Expect.equal ((addr + 2), 0x6500, 0x6501) (new_z80.pc, new_z80.main.hl, new_z80.main.iy)
            ,test "0x2D DEC L" <|
            \_ ->
               let
                  new_env = z80env
                            |> setMem addr 0x2D
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x65FF) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x2D DEC IYL" <|
            \_ ->
               let
                  new_env = z80env
                            |> setMem addr 0xFD
                            |> setMem (addr + 1) 0x2D
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | iy = 0x6500, hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x65FF, 0x6500) (new_z80.pc, new_z80.main.iy, new_z80.main.hl)
            ,test "0x3C INC A" <|
             \_ ->
                let
                   new_env = z80env
                             |> setMem addr 0x3C
                   new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                         main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
                in
                   Expect.equal ((addr + 1), 0x3A) (new_z80.pc, new_z80.flags.a)
         ],
         describe "DEC 8 bit"
         [
            test "DEC A - 0x3D" <|
            \_ ->
               let
                   new_env = z80env
                             |> setMem addr 0x3D
                   new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                         main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
                in
                   Expect.equal ((addr + 1), 0x38) (new_z80.pc, new_z80.flags.a)
         ],
         describe "LD 8-bit,n"
         [
            test "0x26 - LD H,n" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x26
                               |> setMem (addr + 1) 0x05
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 } }
               in
                  Expect.equal ((addr + 2), 0x0545) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x26 - LD IYH,n" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0x26
                               |> setMem (addr + 2) 0x05
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | iy = 0x6545, hl = 0x6545 } }
               in
                  Expect.equal ((addr + 3), 0x6545, 0x0545) (new_z80.pc, new_z80.main.hl, new_z80.main.iy)
            ,test "0x2E LD L,n" <|
            \_ ->
               let
                  new_env = z80env
                            |> setMem addr 0x2E
                            |> setMem (addr + 1) 0x34
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x6534) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x2E LD IXL,n" <|
            \_ ->
               let
                  new_env = z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0x2E
                            |> setMem (addr + 2) 0x34
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | ix = 0x6500, hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 3), 0x6534, 0x6500) (new_z80.pc, new_z80.main.ix, new_z80.main.hl)
            ,test "LD A,n - 0x3E" <|
            \_ ->
               let
                  new_env = z80env
                            |> setMem addr 0x3E
                            |> setMem (addr + 1) 0x78
                  z80_after_01 = execute_instruction z80rom { z80 | env = new_env,
                                                             main = { z80main | d = 0x45, e = 0x00 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x78) (z80_after_01.pc, z80_after_01.flags.a)
         ],
         describe "0x20 - JR NZ,n"
         [
            test "Dont jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x20
                               |> setMem (addr + 1) 0x05
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | a = 0x39 } }
               in
                  Expect.equal (addr + 2) new_z80.pc
            ,test "Jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x20
                               |> setMem (addr + 1) 0x05
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | a = 0x39, fr = 1 } }
               in
                  Expect.equal (addr + 7) new_z80.pc
            ,test "Jump backwards" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x20
                               |> setMem (addr + 1) 0xFB
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | a = 0x39, fr = 1 } }
               in
                  Expect.equal (addr - 3) new_z80.pc
         ],
         describe "0x22 - LD (nn), HL"
         [
            test "Do it" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x22
                               |> setMem (addr + 1) 0x77
                               |> setMem (addr + 2) 0x55
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x5D9F }, flags = { flags | a = 0x39 } }
                  mem_value = new_z80.env |> mem16 0x5577 z80rom
               in
                  Expect.equal ((addr + 3), 0x5D9F) (new_z80.pc, mem_value.value)
         ],
         describe "16 bit Increment"
         [
            test "0x23 INC HL" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x23
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x6546) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x23 INC IX" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0x23
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | ix = 0xFFFF, hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x6545, 0) (new_z80.pc, new_z80.main.hl, new_z80.main.ix)
         ],
         describe "0x27 - DAA"
         [
            test "No idea how to do this..." <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x27
                               |> setMem (addr + 1) 0x05
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x39) (new_z80.pc, new_z80.flags.a)
         ],
         describe "0x28 JR Z, n"
         [
            test "Dont jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x28
                               |> setMem (addr + 1) 0x05
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | fr = 1 } }
               in
                  Expect.equal (addr + 2) new_z80.pc
            ,test "Jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x28
                               |> setMem (addr + 1) 0x05
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | a = 0x39, fr = 0 } }
               in
                  Expect.equal (addr + 7) new_z80.pc
         ],
         describe "load reg indirect"
         [
            test "0x2A LD HL,(nn)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x2A
                               |> setMem (addr + 1) 0x34
                               |> setMem (addr + 2) 0x54
                               |> setMem16 0x5434 0x8723
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x4334 } }
               in
                  Expect.equal ((addr + 3), 0x8723) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x2A LD IY,(nn)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0x2A
                               |> setMem (addr + 2) 0x34
                               |> setMem (addr + 3) 0x54
                               |> setMem16 0x5434 0x8723
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | iy = 0x4334, hl = 0x4334 } }
               in
                  Expect.equal ((addr + 4), 0x8723) (new_z80.pc, new_z80.main.iy)
            ,test "0x3A LD A,(nn)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x3A
                               |> setMem (addr + 1) 0x20
                               |> setMem (addr + 2) 0x70
                               |> setMem 0x7020 0x87
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x4334 } }
               in
                  Expect.equal ((addr + 3), 0x87) (new_z80.pc, new_z80.flags.a)
         ],
         describe "0x2F CPL"
         [
            test "Do it" <|
            \_ ->
               let
                  new_env = z80env
                            |> setMem addr 0x2F
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0xC6) (new_z80.pc, new_z80.flags.a)
         ],
         describe "0x30 JR NC, n"
         [
            test "Dont jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x30
                               |> setMem (addr + 1) 0x05
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | ff = 0x100 } }
               in
                  Expect.equal (addr + 2) new_z80.pc
            ,test "Jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x30
                               |> setMem (addr + 1) 0x05
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | a = 0x39, ff = 0xFF } }
               in
                  Expect.equal (addr + 7) new_z80.pc
         ],
         describe "0x31 - LD SP, nn"
         [
            test "Do it" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x31
                               |> setMem (addr + 1) 0x05
                               |> setMem (addr + 2) 0x07
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 3), 0x0705) (new_z80.pc, new_z80.env.sp)
         ],
         describe "0x32 - LD (nn), A"
         [
            test "Do it" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x32
                               |> setMem (addr + 1) 0x77
                               |> setMem (addr + 2) 0x55
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x5577 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 3), 0x39) (new_z80.pc, mem_value.value)
         ],
         describe "0x33 INC SP"
         [
            test "Do it" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x33
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x8766) (new_z80.pc, new_z80.env.sp)
         ],
         describe "16 bit indirect"
         [
            test "0x34 INC (HL)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x34
                               |> setMem 0x6545 0x78
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 1), 0x79) (new_z80.pc, mem_value.value)
            ,test "0xDD 0x34 INC (IX)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0x34
                               |> setMem (addr + 2) 0xFF
                               |> setMem 0x6544 0x78
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | ix = 0x6545, hl = 0x2545 }, flags = { flags | a = 0x39 } }
                  mem_value =  mem 0x6544 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 3), 0x79) (new_z80.pc, mem_value.value)
            ,test "0x35 DEC (HL)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x35
                               |> setMem 0x6545 0x78
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 1), 0x77, 119) (new_z80.pc, mem_value.value, new_z80.flags.fr)
            ,test "0x35 DEC (HL) going to zero" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x35
                               |> setMem 0x6545 0x01
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 1), 0x00, 0) (new_z80.pc, mem_value.value, new_z80.flags.fr)
            ,test "0xFD 0x35 DEC (IY + n)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0x35
                               |> setMem (addr + 2) 0x01
                               |> setMem 0x6546 0x78
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | iy = 0x6545, hl = 0x2545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6546 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 3), 0x77, 0x2545) (new_z80.pc, mem_value.value, new_z80.main.hl)
         ],
         describe "Indirect indexed load"
         [
            test "0x36 LD (HL),n" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x36
                               |> setMem (addr + 1) 0xA5
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 2), 0xA5) (new_z80.pc, mem_value.value)
            ,test "0xDD 0x36 LD (IX + m),n" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0x36
                               |> setMem (addr + 2) 0x00
                               |> setMem (addr + 3) 0xA5
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | ix = 0x6545, hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 4), 0xA5) (new_z80.pc, mem_value.value)
         ],
         describe "Flags"
         [
            test "0x37 SCF" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x37
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | ff = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x100) (new_z80.pc, (Bitwise.and new_z80.flags.ff 0x100))
           ,test "0x3F CCF" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x3F
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | ff = 0x100 } }
               in
                  Expect.equal ((addr + 1), 0) (new_z80.pc, (Bitwise.and new_z80.flags.ff 0x100))
         ],
         describe "0x38 JR C, n"
         [
            test "Dont jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x38
                               |> setMem (addr + 1) 0x05
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | ff = 0xFF } }
               in
                  Expect.equal (addr + 2) new_z80.pc
            ,test "Jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x38
                               |> setMem (addr + 1) 0x05
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        flags = { flags | a = 0x39, ff = 0x100 } }
               in
                  Expect.equal (addr + 7) new_z80.pc
         ],
         describe "8 bit loads"
         [
            test "0x41 LD B,C" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x41
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, c = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.main.b)
            ,test "0x44 LD B,H" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x44
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, c = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x65) (new_z80.pc, new_z80.main.b)
            ,test "0xDD 0x44 LD B,IXH" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0x44
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | ix = 0x2398, hl = 0x6545, c = 0x76 } }
               in
                  Expect.equal (addr + 2, 0x23) (new_z80.pc, new_z80.main.b)
            ,test "0x48 LD C,B" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x48
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, b = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.main.c)
            ,test "0x53 LD D,E" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x53
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, e = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.main.d)
            ,test "0x5A LD E,D" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x5A
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, d = 0x34 } }
               in
                  Expect.equal (addr + 1, 0x34) (new_z80.pc, new_z80.main.e)
            ,test "0x5E LD E, (HL)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x5E
                               |> setMem 0x6545 0x27
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, d = 0x34 } }
               in
                  Expect.equal (addr + 1, 0x27) (new_z80.pc, new_z80.main.e)
            ,test "0x60 LD H,B" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x60
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, b = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x7645) (new_z80.pc, new_z80.main.hl)
            ,test "0x6F LD L,A" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x6F
                  new_z80 = execute_instruction z80rom { z80 | env = new_env, flags = { flags | a = 0x6F },
                                                        main = { z80main | hl = 0x6545 } }
               in
                  Expect.equal (addr + 1, 0x656F) (new_z80.pc, new_z80.main.hl)
            ,test "0x87 ADD A,A" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x87
                  new_z80 = execute_instruction z80rom { z80 | env = new_env, flags = { flags | a = 0x02 },
                                                        main = { z80main | hl = 0x6545 } }
               in
                  Expect.equal (addr + 1, 0x04) (new_z80.pc, new_z80.flags.a)
            ,test "0x78 LD A,B" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x78
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, b = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.flags.a)
            ,test "0xFD 0x60 LD IYH,B" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0x60
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | iy = 0x6545, hl = 0x6545, b = 0x76 } }
               in
                  Expect.equal (addr + 2, 0x7645, 0x6545) (new_z80.pc, new_z80.main.iy, new_z80.main.hl)
            ,test "0x66 LD H,(HL)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x66
                               |> setMem 0x6545 0x78
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x7845) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x66 LD H,(IX+m)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0x66
                               |> setMem (addr + 2) 0x02
                               |> setMem 0x6545 0x78
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | ix = 0x6543, hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 3), 0x7845, 0x6543) (new_z80.pc, new_z80.main.hl, new_z80.main.ix)
            ,test "0x70 LD (HL),B" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x70
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 1), 0xA5) (new_z80.pc, mem_value.value)
            ,test "0xFD 0x70 LD (IY+m), B" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0x70
                               |> setMem (addr + 2) 0x02
                               |> setMem 0x6545 0x78
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x2545, iy = 0x6543, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 3), 0xA5) (new_z80.pc, mem_value.value)
            ,test "0x74 LD (HL),H" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x74
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 1), 0x65) (new_z80.pc, mem_value.value)
            ,test "0xFD 0x74 LD (IY+m),H" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0x74
                               |> setMem (addr + 2) 0x02
                               |> setMem 0x6545 0x78
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | iy = 0x6543, hl = 0x2545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 3), 0x25) (new_z80.pc, mem_value.value)
            ,test "0xDD 0x74 LD (IX+m),H" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0x74
                               |> setMem (addr + 2) 0x02
                               |> setMem 0x6545 0x78
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | ix = 0x6543, hl = 0x2545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time  z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 3), 0x25) (new_z80.pc, mem_value.value)
         ],
         describe "0xB8 - -xBF CP"
         [
            test "0xBC CP H greater" <|
            \_ ->
               let
                  new_z80 = execute_instruction z80rom { z80 | env = z80env |> setMem addr 0xBC,
                                                        main = { z80main | hl = 0x0245 },
                                                        flags = { flags | a = 0x06 } }
               in
                  Expect.equal { pc = (addr + 1), fa = 6, fb = -3, ff = 4, fr = 4 }
                  { pc = new_z80.pc, fa = new_z80.flags.fa, fb =  new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            ,test "0xBC CP H less" <|
            \_ ->
               let
                  new_z80 = execute_instruction z80rom { z80 | env = z80env |> setMem addr 0xBC,
                                                        main = { z80main | hl = 0x0645 },
                                                        flags = { flags | a = 0x02 } }
               in
                  Expect.equal { pc = (addr + 1), fa = 2, fb = -7, ff = -44, fr = 252 }
                  { pc = new_z80.pc, fa = new_z80.flags.fa, fb =  new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            ,test "0xBC CP H equal" <|
            \_ ->
               let
                  new_z80 = execute_instruction z80rom { z80 | env = z80env |> setMem addr 0xBC,
                                                        main = { z80main | hl = 0x0645 },
                                                        flags = { flags | a = 0x06 } }
               in
                  Expect.equal { pc = (addr + 1), fa = 6, fb = -7, ff = 0, fr = 0 }
                  { pc = new_z80.pc, fa = new_z80.flags.fa, fb =  new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
         ],
         describe "CALL(0xCD) and RET(C9)"
         [
            test "Call followed by return" <|
            \_ ->
               let
                  stackp = 0xF000
                  start = 0x5000
                  new_env = z80env
                               |> setMem start 0xC9
                               |> setMem (start + 1) 0xCD
                               |> setMem (start + 2) (Bitwise.and start 0xFF)
                               |> setMem (start + 3) (shiftRightBy 8 start)
                  z80_1 = { z80 | env = { new_env | sp = stackp + 2 }, pc = (start + 1),
                                  flags = { flags | a = 0x30 } } |> execute_instruction z80rom
                  mem_value = z80_1.env |> mem16 stackp z80rom
                  z80_2 = z80_1 |> execute_instruction z80rom
               in
                  Expect.equal
                  {addr=start, sp1=stackp, stacked=start + 4, addr4=start + 4, sp2=stackp + 2}
                  {addr=z80_1.pc, sp1=z80_1.env.sp, stacked=mem_value.value, addr4=z80_2.pc, sp2=z80_2.env.sp}
         ],
         describe "ADD A, n (0xC6)"
         [
            test "doit" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xC6
                               |> setMem (addr + 1) 0x16
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0xFF77 },
                                                        main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }, flags = { flags | a = 0x60 } }
               in
                  Expect.equal {pc=(addr + 2), a=0x76}
                  {pc=new_z80.pc, a=new_z80.flags.a}
         ],
         describe "EXX (0xD9)"
         [
            test "doit" <|
            \_ ->
               let
                  alt = z80.alt_main
                  new_env = z80env
                               |> setMem addr 0xD9
                               |> setMem (addr + 1) 0x16
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0xFF77 },
                                                        alt_main = { alt | hl = 0x4040, b = 0x67, c = 0x34, d = 0x12, e = 0x81 },
                                                        main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 } }
               in
                  Expect.equal {pc=(addr + 1), hl=0x4040}  {pc=new_z80.pc, hl=new_z80.main.hl}
         ],
         describe "IX things"
         [
            test "0xDD 0xCB nn 0xC6 SET 0, (IX + n)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0xCB
                               |> setMem (addr + 2) 0x06
                               |> setMem (addr + 3) 0xC6
                               |> setMem 0xA086 0x10
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | ix=0xA080, hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0xA086 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 4), 0x11) (new_z80.pc, mem_value.value)
            ,test "0xFD 0xCB nn 0x9E RES 3, (IY + n) -ve" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0xCB
                               |> setMem (addr + 2) 0xFE
                               |> setMem (addr + 3) 0x9E
                               |> setMem 0xA07E 0xFF
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | iy=0xA080, hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0xA07E new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 4), 0xF7) (new_z80.pc, mem_value.value)
            ,test "0xDD 0xCB nn 0x66 BIT 4, (IX + n) (SET)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0xCB
                               |> setMem (addr + 2) 0x02
                               |> setMem (addr + 3) 0x66
                               |> setMem 0xA082 0x10
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | ix=0xA080, hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 4), 0x10) (new_z80.pc, new_z80.flags.fr)
            ,test "0xDD 0xCB nn 0x66 BIT 4, (IX + n) (CLEAR)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0xCB
                               |> setMem (addr + 2) 0x02
                               |> setMem (addr + 3) 0x66
                               |> setMem 0xA082 0xEF
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | ix=0xA080, hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 4), 0x00) (new_z80.pc, new_z80.flags.fr)
         ]
      ]
