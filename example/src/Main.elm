module Main exposing (..)

import Browser
import FormData exposing (FormData)
import Html exposing (Html, a, button, div, fieldset, form, input, label, li, option, p, pre, select, small, sup, text, ul)
import Html.Attributes exposing (checked, class, disabled, href, name, placeholder, rel, selected, style, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onFocus, onInput, onSubmit)
import Process
import Task


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { userForm : FormData FormField User
    }


type alias User =
    { name : String
    , age : Int
    , location : String
    , hobbies : List Hobby
    }


type alias Flags =
    {}


type Msg
    = OnInput FormField String
    | OnBlur (Maybe FormField)
    | OnCheck FormField Bool
    | Save User
    | Saved


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { userForm =
            FormData.init stringFormField []
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        ( dataUser, errors ) =
            FormData.parse parseDontValidate model.userForm
                -- recommended, but not enforced, to only show errors for visited fields
                |> Tuple.mapSecond (FormData.visitedErrors model.userForm)

        ( formAttr, submitButtonAttr, submitButtonLabel ) =
            case dataUser of
                FormData.Invalid ->
                    ( disabled True, disabled True, "Submit" )

                FormData.Valid user ->
                    ( onSubmit (Save user), onClick (Save user), "Submit" )

                FormData.Submitting user ->
                    ( disabled True, disabled True, "Submitting..." )
    in
    div []
        [ Html.node "link"
            [ rel "stylesheet"
            , href "https://cdn.jsdelivr.net/npm/@exampledev/new.css@1.1.2/new.min.css"
            ]
            []
        , Html.node "style"
            []
            [ text ".error { color: red; }"
            , text "small { display: block; margin-top: -0.5em; font-size: 0.7em; }"
            , text "ul { font-size: 0.8em; }"
            , text "label.checkbox, label.radio { display: block; }"
            ]
        , ul []
            [ li [] [ text "Name and Hobbies are required" ]
            , li [] [ text "Age is optional but has to be a positive number" ]
            , li []
                [ text "Error messages will only appear after you've answered a field with an invalid value"
                , ul []
                    [ li [] [ text "will NOT appear before you start" ]
                    , li [] [ text "will NOT appear until you've answered a field" ]
                    ]
                ]
            , li [] [ text "Once the form is valid, the Submit button will be enabled" ]
            , li []
                [ text "When you submit, the Submit button will be disabled until submission completes"
                , ul []
                    [ li [] [ text "in this demo, it's just a sleep of 3 seconds" ] ]
                ]
            ]
        , form [ formAttr ]
            [ fieldset []
                [ p []
                    [ label []
                        [ text "Hobbies"
                        , sup [] [ text "*" ]
                        , p []
                            [ label [ class "checkbox" ]
                                [ input
                                    [ onCheck (OnCheck (Hobbies Soccer))
                                    , onClick (OnBlur Nothing)
                                    , type_ "checkbox"
                                    , checked (FormData.isChecked (Hobbies Soccer) model.userForm)
                                    ]
                                    []
                                , text " Soccer "
                                ]
                            , label [ class "checkbox" ]
                                [ input
                                    [ onCheck (OnCheck (Hobbies Basketball))
                                    , onClick (OnBlur Nothing)
                                    , type_ "checkbox"
                                    , checked (FormData.isChecked (Hobbies Basketball) model.userForm)
                                    ]
                                    []
                                , text " Basketball "
                                ]
                            , label [ class "checkbox" ]
                                [ input
                                    [ onCheck (OnCheck (Hobbies Crochet))
                                    , onClick (OnBlur Nothing)
                                    , type_ "checkbox"
                                    , checked (FormData.isChecked (Hobbies Crochet) model.userForm)
                                    ]
                                    []
                                , text " Crochet "
                                ]
                            , case FormData.errorAt Nothing errors of
                                Just err ->
                                    small [ class "error" ] [ text err ]

                                Nothing ->
                                    text ""
                            ]
                        ]
                    ]
                , p []
                    [ label []
                        [ text "Name"
                        , sup [] [ text "*" ]
                        , p []
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
                                    small [ class "error" ] [ text err ]

                                Nothing ->
                                    text ""
                            ]
                        ]
                    ]
                , p []
                    [ label []
                        [ text "Age"
                        , p []
                            [ input
                                [ onInput (OnInput Age)
                                , onBlur (OnBlur (Just Age))
                                , value (FormData.value Age model.userForm)
                                , type_ "number"
                                , placeholder "Age"
                                ]
                                []
                            , case FormData.errorAt (Just Age) errors of
                                Just err ->
                                    small [ class "error" ] [ text err ]

                                Nothing ->
                                    text ""
                            ]
                        ]
                    ]
                , p []
                    [ label []
                        [ text "Location"
                        , p []
                            [ select
                                [ onInput (OnInput Location)
                                , onBlur (OnBlur (Just Location))
                                ]
                                [ option [ value "" ] [ text " -- Location -- " ]
                                , option [ selected (FormData.value Location model.userForm == "Singapore") ] [ text "Singapore" ]
                                , option [ selected (FormData.value Location model.userForm == "US") ] [ text "US" ]
                                ]
                            ]
                        ]
                    ]
                , p []
                    [ label []
                        [ text "Location"
                        , p []
                            [ label [ class "radio" ]
                                [ input
                                    [ onInput (OnInput Location)
                                    , value "Singapore"
                                    , type_ "radio"
                                    , checked (FormData.value Location model.userForm == "Singapore")
                                    , name "Location"
                                    ]
                                    []
                                , text " Singapore "
                                ]
                            , label [ class "radio" ]
                                [ input
                                    [ onInput (OnInput Location)
                                    , value "US"
                                    , type_ "radio"
                                    , checked (FormData.value Location model.userForm == "US")
                                    , name "Location"
                                    ]
                                    []
                                , text " US "
                                ]
                            , small [] [ text "the model value backing this Location field is the same as the <select> Location field above" ]
                            ]
                        ]
                    ]
                , p []
                    [ button [ submitButtonAttr ] [ text submitButtonLabel ]
                    ]
                ]
            ]
        , pre []
            [ text ("FormData.parse parseDontValidate model.userForm\n--> " ++ Debug.toString ( dataUser, errors ))
            ]
        , pre []
            [ text ("model.userForm\n--> " ++ Debug.toString model)
            ]
        , a [ href "https://github.com/choonkeat/formdata" ] [ text "Github" ]
        , text " "
        , a [ href "https://package.elm-lang.org/packages/choonkeat/formdata/latest" ] [ text "Package doc" ]
        ]


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
            , Process.sleep 3000
                |> Task.perform (always Saved)
            )

        Saved ->
            ( { model | userForm = FormData.onSubmit False model.userForm }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- TYPES FOR FORMDATA


type FormField
    = Name
    | Age
    | Location
    | Hobbies Hobby


type Hobby
    = Soccer
    | Football
    | Basketball
    | Crochet


stringHobby : Hobby -> String
stringHobby hobby =
    case hobby of
        Soccer ->
            "Soccer"

        Football ->
            "Football"

        Basketball ->
            "Basketball"

        Crochet ->
            "Crochet"


stringFormField : FormField -> String
stringFormField f =
    case f of
        Name ->
            "name"

        Age ->
            "age"

        Location ->
            "location"

        Hobbies h ->
            "hobbies " ++ stringHobby h



-- MAIN LOGIC TO WRITE


parseDontValidate : List ( FormField, String ) -> ( Maybe User, List ( Maybe FormField, String ) )
parseDontValidate keyValueList =
    let
        initalUserWithErrors =
            ( { name = ""
              , age = 0
              , location = ""
              , hobbies = []
              }
            , [ ( Just Name, "cannot be blank" )
              , ( Nothing, "must choose one hobby" )
              ]
            )

        --
        buildUserErrs ( k, s ) ( partUser, partErrs ) =
            case k of
                Name ->
                    ( { partUser | name = s }
                    , if s /= "" then
                        List.filter (\( maybeK, _ ) -> maybeK /= Just k) partErrs

                      else
                        partErrs
                    )

                Age ->
                    case String.toInt s of
                        Just i ->
                            ( { partUser | age = i }
                            , if i > 0 then
                                List.filter (\( maybeK, _ ) -> maybeK /= Just k) partErrs

                              else
                                ( Just k, "must be a positive number" ) :: partErrs
                            )

                        Nothing ->
                            ( partUser, ( Just k, "is not a number" ) :: partErrs )

                Location ->
                    ( { partUser | location = s }
                    , if s /= "" then
                        List.filter (\( maybeK, _ ) -> maybeK /= Just k) partErrs

                      else
                        partErrs
                    )

                Hobbies h ->
                    ( { partUser | hobbies = h :: partUser.hobbies }
                    , List.filter (\( maybeK, _ ) -> maybeK /= Nothing) partErrs
                    )

        --
        ( value, errs ) =
            List.foldl buildUserErrs initalUserWithErrors keyValueList
    in
    if [] == errs then
        ( Just { value | hobbies = List.reverse value.hobbies }, [] )

    else
        ( Nothing, errs )
