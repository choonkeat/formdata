# FormData

Parse, don't validate form data.

# State

Before submitting, we should try to obtain a value (and a list of errors) from our FormData

```elm
( maybeUser, errors ) =
    FormData.parse parseDontValidate model.userForm
```

If we get `Nothing`, disable the submit button. Otherwise, wire up the `onSubmit` handler

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

If there's an error, show it

```elm
div [ input [ onInput (OnInput Name), type_ "text", placeholder "Name" ] []
    , case List.head (List.filter (\( k, v ) -> k == Just Name) errors) of
        Just ( _, err ) ->
            p [] [ small [] [ text err ] ]

        Nothing ->
            text ""
    ]
```
