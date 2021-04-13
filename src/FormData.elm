module FormData exposing
    ( init, Config, FormData
    , checked, error, inputValue
    , setValue, toggleValue, parsed
    )

{-| Manage the state of a form


## Types

@docs init, Config, FormData


## View

@docs checked, error, inputValue


## Update

@docs setValue, toggleValue, parsed

-}

import Dict exposing (Dict)
import Set exposing (Set)


{-| `FormData` stores input values in `Dict` and thus need the programmer
to provide functions to parse, don't validate, the raw user input
-}
type alias Config k a err comparable =
    { fromKey : k -> comparable
    , toKey : comparable -> k
    , parseData : List ( k, String ) -> ( Maybe a, List ( k, err ) )
    }


{-| The type that holds all the state.

    type Model =
        { userForm = FormData UserFields User String String
        }

-}
type FormData k a err comparable
    = FormData
        { raw : RawData comparable
        , data : Maybe a
        , errors : Errors comparable err
        , config : Config k a err comparable
        }


{-| Internally, we'd just store whatever the user input as String
-}
type alias RawData comparable =
    Dict comparable String


type alias Errors comparable err =
    Dict comparable err


{-| The types parsed by Config will determine what types we are managing here
-}
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
        }


{-| For handling `onInput`; updates the state based on incoming user input.
The incoming value is stored as-is

    update msg model =
        case msg of
            InputEmail v ->
                ( { model | userForm = FormData.setValue Email v model.userForm }
                , Cmd.none
                )

-}
setValue : k -> String -> FormData k a err comparable -> FormData k a err comparable
setValue k v (FormData ({ config } as formdata)) =
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


toMulti : String -> Set String
toMulti =
    String.split "\u{000D}\u{000D}" >> Set.fromList


fromMulti : a -> Set comparable -> (a -> Set comparable -> Set String) -> String
fromMulti v set addremove =
    set
        |> addremove v
        |> Set.toList
        |> String.join "\u{000D}\u{000D}"


{-| For handling `onCheck`; stores multiple values for a single `k`

NOTE: we use \\r\\r as delimiter to manage multiple values

-}
toggleValue : k -> String -> Bool -> FormData k a err comparable -> FormData k a err comparable
toggleValue k v bool (FormData ({ config } as formdata)) =
    let
        index =
            formdata.config.fromKey k

        newRaw =
            case ( bool, Maybe.map toMulti (Dict.get index formdata.raw) ) of
                ( True, Just list ) ->
                    Dict.insert index (fromMulti v list Set.insert) formdata.raw

                ( True, Nothing ) ->
                    Dict.insert index v formdata.raw

                ( False, Just list ) ->
                    case fromMulti v list Set.remove of
                        "" ->
                            Dict.remove index formdata.raw

                        newV ->
                            Dict.insert index newV formdata.raw

                ( False, Nothing ) ->
                    formdata.raw

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


{-| What's the data we've parsed from user input, if any?

This value can be used to determine if we should enable or disable the submit button

    button [ disabled (parsed == Nothing) ] [ text "Submit" ]

Ultimately, we'll need a `Just a` from here to pass to other parts of our system,
e.g. encoding into json to submit to the database

-}
parsed : FormData k a err comparable -> Maybe a
parsed (FormData formdata) =
    formdata.data


{-| What's the error for form field `k`, if any?

    -- we can tag an error message next to our form fields
    descriptionField =
        div []
            [ textarea [] []
            , maybeErrorMessage (FormData.error LongDescription formData)
            ]

    -- a helper function like this can help
    maybeErrorMessage maybeStr =
        case maybeStr of
            Just s ->
                p [ class "error" ] [ text s ]

            Nothing ->
                text ""

-}
error : k -> FormData k a err comparable -> Maybe err
error k (FormData ({ config } as formdata)) =
    Dict.get (config.fromKey k) formdata.errors


{-| What's `value` that we should set for form element of field `k`, if any?

    input [ value (FormData.inputValue ShortBio formData) ] []

-}
inputValue : k -> FormData k a err comparable -> String
inputValue k (FormData ({ config } as formdata)) =
    Dict.get (config.fromKey k) formdata.raw
        |> Maybe.withDefault ""


{-| Is `v` the current option chosen for field `k`?

    input
        [ checked (FormData.checked Hobby Soccer formData)
        , onCheck (HobbyChecked Soccer)
        ]
        []

-}
checked : k -> String -> FormData k a err comparable -> Bool
checked k v (FormData ({ config } as formdata)) =
    Maybe.map toMulti (Dict.get (formdata.config.fromKey k) formdata.raw)
        |> Maybe.map (Set.member v)
        |> Maybe.withDefault False
