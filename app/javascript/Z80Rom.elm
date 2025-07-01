module Z80Rom exposing (..)

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode exposing (Decoder, Step(..), andThen, loop, map, succeed, unsignedInt8)
import Dict exposing (Dict)
import Keyboard exposing (Keyboard)
import Utils exposing (listToDict, toHexString)
import Z80Debug exposing (debugTodo)
import Z80Ram exposing (Z80Ram)


type alias Z80ROM =
    { rom48k : Dict Int Int
    , keyboard : Keyboard
    , z80ram : Z80Ram
    }


constructor : Z80ROM
constructor =
    let
        rom48k =
            List.range 0 16384

        rom_list =
            List.indexedMap Tuple.pair rom48k

        rom_dict =
            Dict.fromList rom_list
    in
    Z80ROM rom_dict Keyboard.constructor Z80Ram.constructor


getROMValue : Int -> Z80ROM -> Int
getROMValue addr z80rom =
    --case z80rom of
    --    Z80ROM z80dict _ ->
    case Dict.get addr z80rom.rom48k of
        Just a ->
            a

        Nothing ->
            debugTodo "getROMValue" (String.fromInt addr) -1



--make_spectrum_rom : Array Int -> Z80ROM
--make_spectrum_rom romdata =
--    let
--        romDict =
--            listToDict (Array.toList romdata)
--    in
--    Z80ROM romDict


parseRomFile : Bytes -> Maybe Z80ROM
parseRomFile bytes =
    Bytes.Decode.decode romDecoder bytes


romDecoder : Decoder Z80ROM
romDecoder =
    array_decoder 16384 unsignedInt8 |> andThen grabRomDecoder


grabRomDecoder : Array Int -> Decoder Z80ROM
grabRomDecoder romData =
    succeed (Z80ROM (romData |> Array.toList |> listToDict) Keyboard.constructor Z80Ram.constructor)


array_decoder : Int -> Decoder Int -> Decoder (Array Int)
array_decoder size decoder =
    loop ( size, Array.empty ) (arrayStep decoder)


arrayStep : Decoder Int -> ( Int, Array Int ) -> Decoder (Step ( Int, Array Int ) (Array Int))
arrayStep decoder ( n, xs ) =
    if n <= 0 then
        succeed (Done xs)

    else
        map (\x -> Loop ( n - 1, Array.push x xs )) decoder


romRoutineNames =
    Dict.fromList
        [ ( 0x08, "ERROR-1" )

        -- This is jumped to from ERROR-1
        --, ( 0x53, "ERROR-2" )
        --, ( 0x0E9B, "CL-ADDR" )
        -- way too busy to log accesses
        --, ( 0x10, "PRINT-A-1" )
        --, ( 0x4D, "0x004D" )
        --, ( 0x38, "MASK-INT" )
        --, ( 0x48, "KEY-INT" )
        --, ( 0x11DC, "RAM-FILL" )
        --, ( 0x15F2, "PRINT-A-2" )
        --, ( 0x0AD9, "PO-ABLE" )
        --, ( 0x0B24, "PO-ANY" )
        --, ( 0x0B65, "PO-CHAR" )
        --        , ( 0x02C6, "K-ST-LOOP" )
        --, ( 0x15FE, "0x15FE" )
        --, ( 0x15E1, "0x15E1" )
        --, ( 0x15FF, "0x15FF" )
        --, ( 0x15F7, "CALL-SUB" )
        --, ( 0x10A8, "KEY-INPUT" )
        --, ( 0x15DE, "WAIT-KEY-1" )
        --, ( 0x386E, "PATCH-1?" )
        --, ( 0x02C2, "0x02C2" )
        --, ( 0x3879, "PATCH-2?" )
        --, ( 0x02D1, "K-CH-SET" )
        --, ( 0x15E6, "INPUT-AD" )
        --, ( 0x02BF, "KEYBOARD" )
        --, ( 0x0B7F, "PR-ALL" )
        --, ( 0x0BB7, "PR-ALL-4" )
        --, ( 0x0BC1, "PR-ALL-5" )
        --, ( 0x02DB, "0x02DB" )
        --, ( 0x031E, "K-TEST" )
        --, ( 0x02AB, "KEY-DONE" )
        --, ( 0x162C, "CALL-JUMP" )
        --, ( 0x1615, "CHAN-FLAG" )
        --, ( 0x1601, "CHAN-OPEN" )
        --, ( 0x0D76, "PO-CHAR-3" )
        --, ( 0x11E2, "RAM-READ" )
        --, ( 0x1610, "CHAN-OP-1" )
        --, ( 0x0DD9, "CL-SET" )
        --, ( 0x0BB6, "PR-ALL-3" )
        --, ( 0x0B93, "PR-ALL-1" )
        --, ( 0x0BA4, "PR-ALL-2" )
        --, ( 0x0DEE, "CL-SET-1" )
        --, ( 0x028E, "KEY-SCAN" )
        --, ( 0x16DC, "INDEXER" )
        --, ( 0x1615, "CHAN-FLAG" )
        --, ( 0x0B76, "PO-CHAR-3" )
        --, ( 0x0BDB, "PO-ATTR" )
        --, ( 0x387C, "PATCH-3?" )
        --, ( 0x0C22, "PO-EACH" )
        --, ( 0x09F4, "PRINT-OUT" )
        --, ( 0x0C3B, "PO-SAVE" )
        --, ( 0x15D4, "WAIT-KEY" )
        --, ( 0x18E1, "OUT-CURS" )
        --, ( 0x02A1, "KEY-BITS" )
        --, ( 0x029F, "KEY-3KEYS" )
        --, ( 0x196C, "OUT-CH-3" )
        --, ( 0x0C0A, "PO-MSG" )
        --, ( 0x0B03, "PO-FETCH" )
        --, ( 0x19D5, "NEXT-0-3" )
        --, ( 0x19B8, "NEXT-ONE" )
        --, ( 0x167F, "PTR-DONE" )
        --, ( 0x0EE7, "PRB-BYTES" )
        --, ( 0x2D1B, "NUMERIC" )
        --, ( 0x1F54, "BREAK-KEY" )
        , ( 0x08F9, "ME-OLD-VP" )
        , ( 0x0AFC, "PO-ST-PR" )
        , ( 0x0B1D, "PO-F-PR" )

        --, ( 0x03D4, "BE-IX+0" )
        --, ( 0x03D6, "BE-H&L-LP" )
        --, ( 0x03F2, "BE-AGAIN" )
        , ( 0x0EDF, "CLEAR-PRB" )
        , ( 0x046C, "REPORT-B" )
        , ( 0x12A2, "MAIN-EXEC" )
        , ( 0x16B0, "SET-MIN" )
        , ( 0x11EF, "RAM-DONE" )
        , ( 0x0F0C, "COPY-L-2" )
        , ( 0x03B5, "BEEPER" )

        --, ( 0x1F05, "TEST-ROOM" )
        , ( 0x110D, "KEY-NEXT" )

        --, ( 0x1664, "POINTERS" )
        , ( 0x12AC, "MAIN-2" )

        --, ( 0x187D, "OUT-LINE2" )
        --, ( 0x1937, "OUT-CHAR" )
        , ( 0x0F2C, "EDITOR" )
        , ( 0x12A9, "MAIN-1" )
        , ( 0x0EDF, "CLEAR-PRB" )
        , ( 0x111D, "ED-COPY" )

        --, ( 0x0D4D, "TEMPS" )
        --, ( 0x0C55, "PO-SCR" )
        , ( 0x0BD3, "PR-ALL-6" )

        --, ( 0x1195, "SET-DE" )
        --, ( 0x0D6B, "CLS" )
        --, ( 0x16C5, "SET-STK" )
        , ( 0x11CB, "START/NEW" )
        , ( 0x0767, "LD-LOOK-H" )
        , ( 0x0802, "LD-BLOCK" )
        , ( 0x0808, "LD-CONTRL" )
        , ( 0x0556, "LD-BYTES" )
        , ( 0x053F, "SA/LD-RET" )
        , ( 0x0554, "SA/LD-END" )
        , ( 0x0552, "REPORT-D" )
        , ( 0x0806, "REPORT-R" )
        , ( 0x0767, "LD-LOOK-H" )
        , ( 0x07CB, "VR-CONTRL" )
        ]



-- Look up the name of a Spectrum ROM subroutine


subName : Int -> String
subName addr =
    case Dict.get addr romRoutineNames of
        Just name ->
            name ++ " (" ++ (addr |> toHexString) ++ ")"

        Nothing ->
            addr |> toHexString
