module FormData exposing (ErrorString, FormData, Parsers, init)

import Dict exposing (Dict)


type alias ErrorString =
    String


type alias Parsers k a comparable =
    { fromKey : k -> comparable
    , toKey : comparable -> k
    , toData : List ( k, String ) -> ( Maybe a, List ( k, ErrorString ) )
    }


type FormData k a comparable
    = FormData
        { raw : RawData comparable
        , data : Maybe a
        , errors : Errors comparable
        , parsers : Parsers k a comparable
        , submitting : Bool
        }


type alias RawData comparable =
    Dict comparable String


type alias Errors comparable =
    Dict comparable ErrorString


init : Parsers k a comparable -> List ( k, String ) -> FormData k a comparable
init parsers raw =
    let
        ( data, errors ) =
            parsers.toData raw
    in
    FormData
        { raw = Dict.fromList (List.map (Tuple.mapFirst parsers.fromKey) raw)
        , data = data
        , errors = Dict.fromList (List.map (Tuple.mapFirst parsers.fromKey) errors)
        , parsers = parsers
        , submitting = False
        }
