module FormData exposing
    ( FormData, init, Data(..), onSubmit, Errors, errorsFrom, errorAt
    , value, onInput
    , isChecked, onCheck
    , parse
    , onVisited, hadVisited, visitedErrors
    )

{-| Using `Dict.Any` and a few helper functions to manage the state of a form

    type alias Model =
        { userForm : FormData UserFields User
        }

    type alias User =
        { firstname : String
        , hobbies : List Hobby
        }

    type UserFields
        = Firstname
        | Hobbies Hobby

    type Hobby = Soccer | Football | Basketball

    stringHobby : Hobby -> String
    stringHobby hobby =
        case hobby of
            Soccer -> "soccer"
            Football -> "football"
            Basketball -> "basketball"

    stringUserFields : UserFields -> String
    stringUserFields field =
        case field of
            Firstname ->
                "firstname"
            Hobbies h ->
                "hobbies " ++ stringHobby h

    model : Model
    model =
        { userForm = FormData.init stringUserFields []
        }

    currentForm : FormData UserFields User
    currentForm =
        model.userForm
            |> onInput Firstname "Alice"
            |> onCheck (Hobbies Soccer) True
            |> onCheck (Hobbies Football) True
            |> onCheck (Hobbies Soccer) False
            |> onCheck (Hobbies Basketball) True

    currentForm
    --> FormData.init stringUserFields
    -->    [ (Firstname, "Alice"), (Hobbies Basketball, ""), (Hobbies Football, "") ]


    parseDontValidate : List (UserFields, String) -> ( Maybe User, List ( Maybe UserFields, String ) )
    parseDontValidate keyValueList =
        let
            initalUserWithErrors =
                ( { firstname = "", hobbies = [] }
                , [ ( Just Firstname, "cannot be blank")
                  , ( Nothing, "must choose one hobby" )
                  ]
                )
            --
            buildUserErrs (k, s) (partUser, partErrs) =
                case k of
                    Firstname ->
                        ( { partUser | firstname = s }
                        , if s /= "" then
                              List.filter (\(maybeK, _) -> maybeK /= Just k) partErrs
                          else
                              partErrs
                        )
                    Hobbies h ->
                        ( { partUser | hobbies = h :: partUser.hobbies }
                        , List.filter (\(maybeK, _) -> maybeK /= Nothing) partErrs
                        )
            --
            (value, errs) =
                List.foldl buildUserErrs initalUserWithErrors keyValueList
        in
        if [] == errs then
            (Just { value | hobbies = List.reverse value.hobbies }, [])
        else
            (Nothing, errs)

    model.userForm
        |> FormData.parse parseDontValidate
    --> ( Nothing , errorsFrom stringUserFields [ ( Just Firstname, "cannot be blank"), ( Nothing, "must choose one hobby") ] )

    model.userForm
        |> FormData.onInput Firstname "Alice"
        |> FormData.parse parseDontValidate
    --> ( Nothing , errorsFrom stringUserFields [ ( Nothing, "must choose one hobby") ] )

    model.userForm
        |> FormData.onInput Firstname "Alice"
        |> FormData.onCheck (Hobbies Football) True
        |> FormData.parse parseDontValidate
    --> ( Just  { firstname = "Alice", hobbies = [Football] }, errorsFrom stringUserFields [] )


## Types

@docs FormData, init, Data, onSubmit, Errors, errorsFrom, errorAt


## Input

@docs value, onInput


## Checkbox

@docs isChecked, onCheck


## Parse, don't validate

@docs parse


## Extra

in order to _not_ show errors which user hasn't attempted, it would be nice
to track which inputs were visited

@docs onVisited, hadVisited, visitedErrors

-}

import Dict exposing (Dict)
import Dict.Any exposing (AnyDict)


{-| The type that holds all the state.
-}
type FormData k a
    = FormData
        { raw : AnyDict String k String
        , fromKey : k -> String
        , visited : List (Maybe k)
        , submitting : Bool
        }


{-| The 3 states your data can be in
-}
type Data a
    = Invalid
    | Valid a
    | Submitting a


{-| Before submitting, we should try to obtain a value (and a list of errors) from our FormData

    ( maybeUser, errors ) =
        FormData.parse parseDontValidate model.userForm

If we get `Nothing`, disable the submit button. Otherwise, wire up the `onSubmit` handler

    button
        [ case maybeUser of
            Just user ->
                onSubmit (Save user)

            Nothing ->
                disabled True
        ]
        [ text "Submit" ]

If there's an error, show it

    div
        [ input [ onInput (OnInput Name), type_ "text", placeholder "Name" ] []
        , case List.head (List.filter (\( k, v ) -> k == Just Name) errors) of
            Just ( _, err ) ->
                p [] [ small [] [ text err ] ]

            Nothing ->
                text ""
        ]

-}
parse : (List ( k, String ) -> ( Maybe a, List ( Maybe k, err ) )) -> FormData k a -> ( Data a, Errors k err )
parse function ((FormData { raw, fromKey }) as formdata) =
    let
        validData =
            if isSubmitting formdata then
                Submitting

            else
                Valid
    in
    function (Dict.Any.toList raw)
        |> Tuple.mapBoth
            (\maybeData -> Maybe.withDefault Invalid (Maybe.map validData maybeData))
            (\errList -> errorsFrom fromKey errList)


{-| The types parsed by Config will determine what types we are managing here

    (k -> String)

**Note that it's important to make sure every key is turned to different comparable.** Otherwise keys would conflict and overwrite each other.

-}
init : (k -> String) -> List ( k, String ) -> FormData k a
init fromKey raw =
    FormData
        { raw = Dict.Any.fromList fromKey raw
        , fromKey = fromKey
        , visited = []
        , submitting = False
        }


{-| For handling `onInput`; updates the state based on incoming user input.
The incoming value is stored as-is

    view formdata =
        input
            [ value (FormData.value Firstname formdata)
            , onInput (OnUserFormInput Firstname)
            ]
            []

    update msg model =
        case msg of
            OnUserFormInput k s ->
                ( { model | userForm = formdata.onInput k s model.userForm }
                , Cmd.none
                )

-}
onInput : k -> String -> FormData k a -> FormData k a
onInput k string (FormData formdata) =
    FormData { formdata | raw = Dict.Any.insert k string formdata.raw }


{-| What's the current `value` that we should set for form element of field `k`

    view =
        input
            [ value (FormData.value Firstname formdata) ]
            []

-}
value : k -> FormData k a -> String
value k (FormData formdata) =
    Dict.Any.get k formdata.raw
        |> Maybe.withDefault ""


{-| For handling `onCheck`; stores multiple values for a single `k`

    view formdata =
        label []
            [ text "Hobbies "
            , input
                [ checked (FormData.isChecked (Hobbies Basketball) formdata)
                , onCheck (OnUserFormCheck (Hobbies Basketball))
                , type_ "checkbox"
                ]
                []
            ]

    update msg model =
        case msg of
            OnUserFormCheck k b ->
                ( { model | userForm = FormData.onCheck k b model.userForm }
                , Cmd.none
                )

-}
onCheck : k -> Bool -> FormData k a -> FormData k a
onCheck k bool ((FormData formdata) as originalFormData) =
    case ( bool, Dict.Any.member k formdata.raw ) of
        ( True, True ) ->
            originalFormData

        ( True, False ) ->
            FormData { formdata | raw = Dict.Any.insert k "" formdata.raw }

        ( False, False ) ->
            originalFormData

        ( False, True ) ->
            FormData { formdata | raw = Dict.Any.remove k formdata.raw }


{-| Is `String` a chosen option for field `k`?

    view formdata =
        label []
            [ text "Hobbies "
            , input
                [ checked (FormData.isChecked (Hobbies Basketball) formdata)
                , type_ "checkbox"
                ]
                []
            ]

-}
isChecked : k -> FormData k a -> Bool
isChecked k (FormData formdata) =
    Dict.Any.member k formdata.raw


{-| can be used with `onBlur` to track which inputs were visited

    view formdata =
        input
            [ onBlur (OnUserFormBlur Firstname)
            ]
            []

    update msg model =
        case msg of
            OnUserFormBlur k ->
                ( { model | userForm = formdata.onVisited k model.userForm }
                , Cmd.none
                )

-}
onVisited : Maybe k -> FormData k a -> FormData k a
onVisited k ((FormData formdata) as nochange) =
    if List.member k formdata.visited then
        nochange

    else
        FormData { formdata | visited = k :: formdata.visited }


{-| inquire which inputs were visited
-}
hadVisited : Maybe k -> FormData k a -> Bool
hadVisited k (FormData formdata) =
    List.member k formdata.visited



--


{-| Holds error values for faster lookup
-}
type Errors k err
    = Errors (AnyDict String k err) (Maybe err)


{-| Builds Errors value from List; used inside FormData.parse
-}
errorsFrom : (k -> String) -> List ( Maybe k, err ) -> Errors k err
errorsFrom fromKey list =
    let
        partition =
            \( maybek, err ) ( tuples, errs ) ->
                case maybek of
                    Just k ->
                        ( ( k, err ) :: tuples, errs )

                    Nothing ->
                        ( tuples, err :: errs )

        ( justs, nothings ) =
            List.foldl partition ( [], [] ) list
    in
    Errors (Dict.Any.fromList fromKey justs) (List.head nothings)


{-| Lookup if a Maybe k has error
-}
errorAt : Maybe k -> Errors k err -> Maybe err
errorAt maybek (Errors anydict maybeErr) =
    case maybek of
        Just k ->
            Dict.Any.get k anydict

        Nothing ->
            maybeErr


{-| Filter Errors value against the visited tracker
-}
visitedErrors : FormData k a -> Errors k err -> Errors k err
visitedErrors (FormData formdata) (Errors anydict maybeErr) =
    let
        newDict =
            Dict.Any.filter (\k v -> List.member (Just k) formdata.visited) anydict

        newMaybeErr =
            if List.member Nothing formdata.visited then
                maybeErr

            else
                Nothing
    in
    Errors newDict newMaybeErr



--


{-| Toggles whether the FormData is submitting
-}
onSubmit : Bool -> FormData k a -> FormData k a
onSubmit bool (FormData formdata) =
    FormData { formdata | submitting = bool }


{-| Checks if our FormData is submitting
-}
isSubmitting : FormData k a -> Bool
isSubmitting (FormData { submitting }) =
    submitting
