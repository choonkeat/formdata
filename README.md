# FormData

Parse, don't validate form data.

# State

Before submitting, we should try to obtain a value (and a list of errors) from our `FormData`
value, using a `parseDontValidate` function we supply

```elm
( dataUser, errors ) =
    FormData.parse parseDontValidate model.userForm
        -- recommended, but not enforced, to only show errors for visited fields
        |> Tuple.mapSecond (FormData.visitedErrors model.userForm)
```

We do a `case dataUser of` to decide how we want to manage our submit button

```elm
let
    ( submitAttr, submitLabel ) =
        case dataUser of
            FormData.Invalid ->
                ( disabled True, "Submit" )

            FormData.Valid user ->
                ( onClick (Save user), "Submit" )

            FormData.Submitting user ->
                ( disabled True, "Submitting..." )
in
button [ submitAttr ] [ text submitLabel ]
```

If there's an error, show it alongside the input field

```elm
div []
    [ input
        [ onInput (OnInput Name)
        , onBlur (OnBlur (Just Name))
        , value (FormData.value Name model.userForm)
        , type_ "text"
        , placeholder "Name"
        ]
        []
    , case FormData.errorAt (Just Name) errors of
        Just err ->
            small [] [ text err ]

        Nothing ->
            text ""
    ]
```

Our `update` function hands off most of the work to `FormData`

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInput k string ->
            ( { model | userForm = FormData.onInput k string model.userForm }
            , Cmd.none
            )

        OnBlur k ->
            ( { model | userForm = FormData.onVisited k model.userForm }
            , Cmd.none
            )

        OnCheck k bool ->
            ( { model | userForm = FormData.onCheck k bool model.userForm }
            , Cmd.none
            )

        Save user ->
            ( { model | userForm = FormData.onSubmit True model.userForm }
            , Process.sleep 3000 -- TODO: perform some real work
                |> Task.perform (always Saved)
            )

        Saved ->
            ( { model | userForm = FormData.onSubmit False model.userForm }
            , Cmd.none
            )
```
