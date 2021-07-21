# FormData

Parse, don't validate form data.

# State

Before submitting, we should try to obtain a value (and a list of errors) from our `FormData`
value, using a `parseDontValidate` function we supply

```elm
( maybeUser, errors ) =
    FormData.parse parseDontValidate model.userForm
        -- recommended, but not enforced, to only show errors for visited fields
        |> Tuple.mapSecond (FormData.visitedErrors model.userForm)
```

If we get `maybeUser = Nothing`, disable the submit button. Otherwise, wire up the `onSubmit` handler

```elm
button
    [ case maybeUser of
        Just user ->
            onSubmit (Save user)

        Nothing ->
            disabled True
    ]
    [ text "Submit" ]
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
```
