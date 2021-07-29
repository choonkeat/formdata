# FormData

Parse, don't validate form data.

### TLDR

- `FormData` holds the form state; a wrapper for [`Dict.Any`](https://package.elm-lang.org/packages/turboMaCk/any-dict/latest/Dict-Any)
- Handles user input with [2-4 msg per form](https://github.com/choonkeat/formdata#update) regardless of how many fields there are
- TODO: supply the function
    - Given a list of key-value from the form
    - Return a tuple of `Maybe a` (our actual value, e.g. `Maybe User`) and input field error messages
    ```elm
    parseDontValidate : List ( k, String ) -> ( Maybe a, List ( Maybe k, err ) )
    ```

# How it works

### Model

Instead of using your data type, wrap it with `FormData`

```diff
  type alias User =
      { name : String
        -- ... more fields
      }

  type alias Model =
-     { userForm : User
+     { userForm : FormData FormField User
      }

  init flags =
-     ( { userForm = User "" -- ... more fields
+     ( { userForm = FormData.init fieldToString []
        }
      , Cmd.none
      )
```

for type safety, use a custom type to enumerate the form fields

```elm
type FormField = Name | Age | Location -- ... more fields

fieldToString : FormField -> String
fieldToString k =
    case k of
        Name ->
            "name"

        -- ... more fields
```

not recommended, but you can use `String` to manage form fields too

```elm
type alias FormField = String

fieldToString = identity
```

### View

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

### Update

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

### Submit

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
                ( onSubmit (Save user), disabled False, "Submit" )

            FormData.Submitting user ->
                ( disabled True, disabled True, "Submitting..." )
in
form [ formAttr ]
    [ -- form fields elided (see above)
      button [ submitButtonAttr ] [ text submitButtonLabel ]
    ]
```

# Example

See the [example code](https://github.com/choonkeat/formdata/blob/main/example/src/Main.elm) running at [https://elm-formdata.netlify.app](https://elm-formdata.netlify.app)
or https://ellie-app.com/dRsLRCGJLCBa1
