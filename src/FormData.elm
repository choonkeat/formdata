module FormData exposing
    ( init, Config, FormData
    , checked, error, inputValue
    , onInput, parsed
    -- , onCheck
    -- , submitting
    )

{-| Manage the state of a form


## Types

@docs init, Config, FormData


## View

@docs checked, error, inputValue


## Update

@docs onInput, parsed

-}

import Dict exposing (Dict)


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
        , submitting : Bool
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
        , submitting = False
        }


{-| Updates the state based on incoming user input. The incoming value is stored as-is

    update msg model =
        case msg of
            InputEmail v ->
                ( { model | userForm = FormData.onInput Email v model.userForm }
                , Cmd.none
                )

-}
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


onCheck : k -> String -> Bool -> FormData k a err comparable -> FormData k a err comparable
onCheck k v bool (FormData ({ config } as formdata)) =
    -- might need to change `type alias RawData comparable = Dict comparable (Set String)` ?
    Debug.todo "onCheck v membership for field k"


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
    -- could change depending on implementation of `onCheck`
    Dict.get (config.fromKey k) formdata.raw
        |> Maybe.map ((==) v)
        |> Maybe.withDefault False
