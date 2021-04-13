module FormData exposing
    ( Config
    , FormData
    , init
    , onInput
    )

import Dict exposing (Dict)


{-| Parse, don't validate
-}
type alias Config k a err comparable =
    { fromKey : k -> comparable
    , toKey : comparable -> k
    , parseData : List ( k, String ) -> ( Maybe a, List ( k, err ) )
    }


type FormData k a err comparable
    = FormData
        { raw : RawData comparable
        , data : Maybe a
        , errors : Errors comparable err
        , config : Config k a err comparable
        , submitting : Bool
        }


{-| Internally, we'd just store whatever the user input as String
-}
type alias RawData comparable =
    Dict comparable String


type alias Errors comparable err =
    Dict comparable err


init : Config k a err comparable -> List ( k, String ) -> FormData k a err comparable
init config newRaw =
    let
        ( newData, newErrors ) =
            config.parseData newRaw
    in
    FormData
        { raw = Dict.fromList (List.map (Tuple.mapFirst config.fromKey) newRaw)
        , data = newData
        , errors = Dict.fromList (List.map (Tuple.mapFirst config.fromKey) newErrors)
        , config = config
        , submitting = False
        }


onInput : k -> String -> FormData k a err comparable -> FormData k a err comparable
onInput k v (FormData ({ config } as formdata)) =
    let
        newRaw =
            Dict.insert (formdata.config.fromKey k) v formdata.raw

        ( newData, newErrors ) =
            Dict.toList newRaw
                |> List.map (Tuple.mapFirst config.toKey)
                |> config.parseData
    in
    FormData
        { formdata
            | raw = newRaw
            , data = newData
            , errors = Dict.fromList (List.map (Tuple.mapFirst config.fromKey) newErrors)
        }


parsed : FormData k a err comparable -> Maybe a
parsed (FormData formdata) =
    formdata.data


error : k -> FormData k a err comparable -> Maybe err
error k (FormData ({ config } as formdata)) =
    Dict.get (config.fromKey k) formdata.errors
