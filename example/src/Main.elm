module Main exposing (..)

import Browser
import FormData exposing (FormData)
import Html exposing (Html, div, input, label, option, p, pre, select, text)
import Html.Attributes exposing (checked, href, name, placeholder, rel, selected, type_, value)
import Html.Events exposing (onCheck, onInput)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { userForm : FormData UserFields User String Int
    }


type alias Flags =
    {}


type Msg
    = OnInput UserFields String
    | OnCheck UserFields String Bool
    | OnRadio UserFields String Bool


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { userForm =
            FormData.init
                { fromKey = fromKey
                , toKey = toKey
                , parseData = parseData
                }
                []
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ Html.node "link"
            [ rel "stylesheet"
            , href "https://cdn.jsdelivr.net/npm/@exampledev/new.css@1.1.2/new.min.css"
            ]
            []
        , p []
            [ input [ onInput (OnInput Name), type_ "text", placeholder "Name" ] []
            ]
        , p []
            [ input [ onInput (OnInput Age), type_ "number", placeholder "Age" ] []
            ]
        , p []
            [ select [ onInput (OnInput Location) ]
                [ option [ value "" ] [ text " -- Location -- " ]
                , option [ selected (FormData.checked Location "Singapore" model.userForm) ] [ text "Singapore" ]
                , option [ selected (FormData.checked Location "US" model.userForm) ] [ text "US" ]
                ]
            ]
        , p []
            [ label []
                [ input
                    [ onInput (OnInput Location)
                    , value "Singapore"
                    , type_ "radio"
                    , checked (FormData.checked Location "Singapore" model.userForm)
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
                    , checked (FormData.checked Location "US" model.userForm)
                    , name "Location"
                    ]
                    []
                , text " US "
                ]
            ]
        , p []
            [ label []
                [ input
                    [ onCheck (OnCheck Hobbies "Soccer")
                    , type_ "checkbox"
                    , checked (FormData.checked Hobbies "Soccer" model.userForm)
                    ]
                    []
                , text " Soccer "
                ]
            , label []
                [ input
                    [ onCheck (OnCheck Hobbies "Basketball")
                    , type_ "checkbox"
                    , checked (FormData.checked Hobbies "Basketball" model.userForm)
                    ]
                    []
                , text " Basketball "
                ]
            , label []
                [ input
                    [ onCheck (OnCheck Hobbies "Crochet")
                    , type_ "checkbox"
                    , checked (FormData.checked Hobbies "Crochet" model.userForm)
                    ]
                    []
                , text " Crochet "
                ]
            ]
        , pre [] [ text (Debug.toString (FormData.parsed model.userForm)) ]
        , pre [] [ text (Debug.toString model) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInput k v ->
            ( { model | userForm = FormData.setValue k v model.userForm }
            , Cmd.none
            )

        OnCheck k v bool ->
            ( { model | userForm = FormData.toggleValue k v bool model.userForm }
            , Cmd.none
            )

        OnRadio k v _ ->
            ( { model | userForm = FormData.setValue k v model.userForm }
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
    | Hobbies


type alias User =
    { name : String
    , age : Int
    , location : String
    , hobbies : List String
    }


fromKey : UserFields -> Int
fromKey f =
    case f of
        Name ->
            1

        Age ->
            2

        Location ->
            3

        Hobbies ->
            4


toKey : Int -> UserFields
toKey int =
    case int of
        1 ->
            Name

        2 ->
            Age

        3 ->
            Location

        4 ->
            Hobbies

        _ ->
            Name


parseData : List ( UserFields, String ) -> ( Maybe User, List ( UserFields, String ) )
parseData list =
    -- let
    --     foldFields ( k, v ) maybeUser =
    --         case k of
    --             Name ->
    --                 maybeUser
    --
    --             Age ->
    --                 maybeUser
    --
    --             Location ->
    --                 maybeUser
    --
    --             Hobbies ->
    --                 maybeUser
    -- in
    -- List.foldl
    --     foldFields
    --     (Just { name = "", age = 0, location = "", hobbies = [] })
    --     list
    ( Nothing, [] )
