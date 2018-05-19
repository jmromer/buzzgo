module Entry exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


markEntryWithId : List Entry -> Int -> List Entry
markEntryWithId entries id =
    let
        markEntry e =
            if e.id == id then
                { e | marked = (not e.marked) }
            else
                e
    in
        List.map markEntry entries


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4 Entry
        (field "id" Decode.int)
        (field "phrase" Decode.string)
        (field "points" Decode.int)
        (succeed False)


getEntries : (Result Http.Error (List Entry) -> msg) -> String -> Cmd msg
getEntries msg url =
    (Decode.list entryDecoder)
        |> Http.get url
        |> Http.send msg
