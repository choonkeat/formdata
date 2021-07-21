module Main exposing (..)

import Browser
import FormData exposing (FormData)
import Html exposing (Html, a, button, div, input, label, option, p, pre, select, small, text)
import Html.Attributes exposing (checked, disabled, href, name, placeholder, rel, selected, type_, value)
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
    { userForm : FormData UserFields User
    }


type alias Flags =
    {}


type Msg
    = OnInput UserFields String
    | OnBlur (Maybe UserFields)
    | OnCheck UserFields Bool
    | Save User
    | Saved


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { userForm =
            FormData.init stringUserFields []
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

        ( submitAttr, submitLabel ) =
            case dataUser of
                FormData.Invalid ->
                    ( disabled True, "Submit" )

                FormData.Valid user ->
                    ( onClick (Save user), "Submit" )

                FormData.Submitting user ->
                    ( disabled True, "Submitting..." )
    in
    div []
        [ Html.node "link"
            [ rel "stylesheet"
            , href "https://cdn.jsdelivr.net/npm/@exampledev/new.css@1.1.2/new.min.css"
            ]
            []
        , Html.node "style" [] [ text "small { display: block; margin-top: -0.5em; font-size: 0.7em; color: red; }" ]
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
                    small [] [ text err ]

                Nothing ->
                    text ""
            ]
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
                    small [] [ text err ]

                Nothing ->
                    text ""
            ]
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
        , p []
            [ label []
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
            , label []
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
            ]
        , p []
            [ label []
                [ input
                    [ onCheck (OnCheck (Hobbies Soccer))
                    , onClick (OnBlur Nothing)
                    , type_ "checkbox"
                    , checked (FormData.isChecked (Hobbies Soccer) model.userForm)
                    ]
                    []
                , text " Soccer "
                ]
            , label []
                [ input
                    [ onCheck (OnCheck (Hobbies Basketball))
                    , onClick (OnBlur Nothing)
                    , type_ "checkbox"
                    , checked (FormData.isChecked (Hobbies Basketball) model.userForm)
                    ]
                    []
                , text " Basketball "
                ]
            , label []
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
                    small [] [ text err ]

                Nothing ->
                    text ""
            ]
        , p []
            [ button [ submitAttr ] [ text submitLabel ]
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



--


type UserFields
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


type alias User =
    { name : String
    , age : Int
    , location : String
    , hobbies : List Hobby
    }


stringUserFields : UserFields -> String
stringUserFields f =
    case f of
        Name ->
            "name"

        Age ->
            "age"

        Location ->
            "location"

        Hobbies h ->
            "hobbies " ++ stringHobby h


parseDontValidate : List ( UserFields, String ) -> ( Maybe User, List ( Maybe UserFields, String ) )
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
                            , if s /= "" then
                                List.filter (\( maybeK, _ ) -> maybeK /= Just k) partErrs

                              else
                                partErrs
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
