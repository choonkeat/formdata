module Main exposing (..)

import Browser
import FormData exposing (FormData)
import Html exposing (Html, button, div, input, label, option, p, pre, select, small, text)
import Html.Attributes exposing (checked, disabled, href, name, placeholder, rel, selected, type_, value)
import Html.Events exposing (onCheck, onInput, onSubmit)


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
    | OnCheck UserFields Bool
    | Save User


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
        ( maybeUser, errors ) =
            FormData.parse parseDontValidate model.userForm
    in
    div []
        [ Html.node "link"
            [ rel "stylesheet"
            , href "https://cdn.jsdelivr.net/npm/@exampledev/new.css@1.1.2/new.min.css"
            ]
            []
        , p []
            [ input [ onInput (OnInput Name), type_ "text", placeholder "Name" ] []
            , case List.head (List.filter (\( k, v ) -> k == Just Name) errors) of
                Just ( _, err ) ->
                    p [] [ small [] [ text err ] ]

                Nothing ->
                    text ""
            ]
        , p []
            [ input [ onInput (OnInput Age), type_ "number", placeholder "Age" ] []
            ]
        , p []
            [ select [ onInput (OnInput Location) ]
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
                    , type_ "checkbox"
                    , checked (FormData.isChecked (Hobbies Soccer) model.userForm)
                    ]
                    []
                , text " Soccer "
                ]
            , label []
                [ input
                    [ onCheck (OnCheck (Hobbies Basketball))
                    , type_ "checkbox"
                    , checked (FormData.isChecked (Hobbies Basketball) model.userForm)
                    ]
                    []
                , text " Basketball "
                ]
            , label []
                [ input
                    [ onCheck (OnCheck (Hobbies Crochet))
                    , type_ "checkbox"
                    , checked (FormData.isChecked (Hobbies Crochet) model.userForm)
                    ]
                    []
                , text " Crochet "
                ]
            , case List.head (List.filter (\( k, v ) -> k == Nothing) errors) of
                Just ( _, err ) ->
                    p [] [ small [] [ text err ] ]

                Nothing ->
                    text ""
            ]
        , p []
            [ button
                [ case maybeUser of
                    Just user ->
                        onSubmit (Save user)

                    Nothing ->
                        disabled True
                ]
                [ text "Submit" ]
            ]
        , pre []
            [ text ("FormData.parse parseDontValidate model.userForm\n--> " ++ Debug.toString ( maybeUser, errors ))
            ]
        , pre []
            [ text ("model.userForm\n--> " ++ Debug.toString model)
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInput k v ->
            ( { model | userForm = FormData.onInput k v model.userForm }
            , Cmd.none
            )

        OnCheck k bool ->
            ( { model | userForm = FormData.onCheck k bool model.userForm }
            , Cmd.none
            )

        Save user ->
            ( model, Cmd.none )


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
                            ( partUser, partErrs )

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
