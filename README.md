# FormData

Parse, don't validate form data.

### TLDR

- `FormData` holds the form state and handles user input events
- Our job is only to supply the function `parseDontValidate : List ( k, String ) -> ( Maybe a, List ( Maybe k, err ) )`
    - Given a list of key value from the form
    - Return a tuple of `Maybe a` (our actual value, e.g. `Maybe User`) and input field error messages

# How it works

For each form field we wire up the standard events (`OnInput`, `OnBlur`, and `OnCheck`; not 1-msg-per-input anymore!) and show any `error` for that field alongside its input

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
        Just error ->
            small [] [ text error ]

        Nothing ->
            text ""
    ]
```

And our `update` function hand off form state update to `FormData` helper functions

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
    
        -- FormData standard wiring
        
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

        -- Our own form submit handling

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

Before working with form submission, we try to obtain our custom type value (and a list of errors) from our `FormData`
value using a `parseDontValidate` function we supply

```elm
let
    ( dataUser, errors ) =
        FormData.parse parseDontValidate model.userForm
            |> Tuple.mapSecond (FormData.visitedErrors model.userForm)
```

Do a `case dataUser of` to manage our `form` element and submit `button` attributes

```elm
    ( formAttr, submitButtonAttr, submitButtonLabel ) =
        case dataUser of
            FormData.Invalid ->
                ( disabled True, disabled True, "Submit" )

            FormData.Valid user ->
                ( onSubmit (Save user), onClick (Save user), "Submit" )

            FormData.Submitting user ->
                ( disabled True, disabled True, "Submitting..." )
in
form [ formAttr ]
    [ -- form fields elided (see above)
      button [ submitButtonAttr ] [ text submitButtonLabel ]
    ]
```
