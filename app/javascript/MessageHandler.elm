module MessageHandler exposing (..)

import Bytes exposing (Bytes, Endianness(..))
import Http exposing (Error, Expect, Metadata, Response)
import Tapfile exposing (Tapfile, parseTapFile)
import Z80Rom exposing (Z80ROM, parseRomFile)


bytesToTap : Response Bytes -> Result Error (List Tapfile)
bytesToTap httpResponse =
    case httpResponse of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata body ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ metadata body ->
            --let
            -- would be nice to parse headers - but we seem to get a
            -- gzipped body size not an actual size which makes things tough
            --x = debug_log "got tap headers" metadata.headers Nothing
            --length = metadata.headers |> Dict.get "content-length" |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0
            -- This seems to be the gzip size which isn't that useful
            --z = debug_log "received bytes of size" (body |> width) Nothing
            --in
            Ok (body |> parseTapFile)


bytesToRom : Response Bytes -> Result Error (Maybe Z80ROM)
bytesToRom httpResponse =
    case httpResponse of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata body ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ metadata body ->
            --let
            -- would be nice to parse headers - but we seem to get a
            -- gzipped body size not an actual size which makes things tough
            --x = debug_log "got tap headers" metadata.headers Nothing
            --length = metadata.headers |> Dict.get "content-length" |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0
            -- This seems to be the gzip size which isn't that useful
            --z = debug_log "received bytes of size" (body |> width) Nothing
            --in
            Ok (body |> parseRomFile)



--list_decoder : Int -> Decoder Int -> Decoder (List Int)
--list_decoder size decoder =
--   loop (size, []) (listStep decoder)
--listStep : Decoder Int -> (Int, List Int) -> Decoder (Step (Int, List Int) (List Int))
--listStep decoder (n, xs) =
--   if n <= 0 then
--     succeed (Done xs)
--   else
--     map (\x -> Loop (n - 1, x :: xs)) decoder
