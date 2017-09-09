module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Response =
    { message : String
    , people : List Person
    , number : Int
    }



-- model


type alias Model =
    { message : String
    , people : List Person
    , number : Int
    }


type alias Person =
    { name : String
    , craft : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [] 0, people )


people : Cmd Msg
people =
    let
        url =
            "http://api.open-notify.org/astros.json"

        request =
            Http.get url responseDecoder

        cmd =
            Http.send People request
    in
    cmd


responseDecoder : Decoder Response
responseDecoder =
    decode Response
        |> Json.Decode.Pipeline.required "message" Json.Decode.string
        |> Json.Decode.Pipeline.required "people" (Json.Decode.list personDecoder)
        |> Json.Decode.Pipeline.required "number" Json.Decode.int


personDecoder : Decoder Person
personDecoder =
    decode Person
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "craft" Json.Decode.string



-- update


type Msg
    = People (Result Http.Error Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        People (Ok response) ->
            ( response, Cmd.none )

        People (Err err) ->
            ( { model | message = toString err }, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div []
        [ div [ id "stars" ] []
        , div [ id "stars2" ] []
        , div [ id "stars3" ] []
        , div [ id "title", class "container" ]
            [ div [ class "mx-5" ]
                [ h1 [ class "text-center" ] [ text "👨\x200D🚀 🚀\x200D" ]
                , h2 [ class "row justify-content-md-center mb-5 text-center" ] [ text "Astronauts Currently in Space" ]
                , astronautSection model
                , div [ class "text-center" ]
                    [ text "© Made with ☕️ and Elm by "
                    , a [ href "https://twitter.com/alejandroRINC0N", rel "external", target "_blank", class "text-center" ]
                        [ text "@alejandroRINC0N" ]
                    ]
                ]
            ]
        ]


astronautSection : Model -> Html Msg
astronautSection model =
    div [ class "row justify-content-md-center mx-2" ]
        [ table [ class "table table-sm table-hover mb-5" ]
            [ astronautHeader
            , astronautList model
            ]
        ]


astronautHeader : Html Msg
astronautHeader =
    thead [ class "thead" ]
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "Craft" ]
            ]
        ]


astronautList : Model -> Html Msg
astronautList model =
    model.people
        |> List.map person
        |> tbody []


person : Person -> Html Msg
person person =
    tr []
        [ td [] [ text person.name ]
        , td [] [ text person.craft ]
        ]



-- subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
