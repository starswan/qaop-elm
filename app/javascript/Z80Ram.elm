module Z80Ram exposing (..)

import Z80Memory exposing (Z80Memory, getValue)
import Z80Screen exposing (Z80Screen)
type alias Z80Ram =
    {
        screen: Z80Screen,
        non_screen: Z80Memory
    }

constructor: Z80Ram
constructor =
    let
       ram = List.repeat (49152 - 6912) 0
    in
        Z80Ram Z80Screen.constructor (Z80Memory.constructor ram)

getRamValue: Int -> Z80Ram -> Int
getRamValue addr z80ram =
  let
      ram_addr = addr - 6912
  in
      if ram_addr >= 0 then
         z80ram.non_screen |> getValue ram_addr
      else
         z80ram.screen.screen |> getValue addr

setRamValue: Int -> Int -> Z80Ram-> Z80Ram
setRamValue addr value z80ram =
   let
      ram_addr = addr - 6912
   in
      if ram_addr >= 0 then
         { z80ram | non_screen = z80ram.non_screen |> Z80Memory.set_value ram_addr value }
      else
         { z80ram | screen = z80ram.screen |> Z80Screen.set_value addr value }