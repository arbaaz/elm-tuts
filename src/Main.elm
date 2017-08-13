module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, at, field, int, list, map3, string)


type alias Response =
    { id : Int
    , joke : String
    , categories : List String
    }


type alias ResponseList =
    List Response


type alias Model =
    ResponseList


initModel : Model
initModel =
    []


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


responseDecoder : Decoder Response
responseDecoder =
    JD.map3 Response
        (field "id" int)
        (field "joke" string)
        (field "categories" (JD.list string))


responseListDecorder : Decoder (List Response)
responseListDecorder =
    JD.list responseDecoder
        |> at [ "value" ]


randomJoke : Cmd Msg
randomJoke =
    let
        url =
            "//api.icndb.com/jokes/random/5"

        request =
            Http.get url responseListDecorder

        cmd =
            Http.send Joke request
    in
    cmd


type Msg
    = Joke (Result Http.Error ResponseList)
    | NewJoke


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Joke (Ok response) ->
            ( response, Cmd.none )

        Joke (Err err) ->
            ( model, Cmd.none )

        NewJoke ->
            ( model, randomJoke )


renderJoke : Response -> Html Msg
renderJoke joke =
    li [ class "list-group-item" ] [ text joke.joke ]


renderJokes : ResponseList -> Html Msg
renderJokes jokes =
    ul [ class "list-group" ] (List.map renderJoke jokes)


view : Model -> Html Msg
view model =
    let
        inner =
            div []
                [ button
                    [ onClick NewJoke
                    , class "btn btn-primary"
                    ]
                    [ text "Fetch Jokes" ]
                , br [] []
                , renderJokes model
                ]
    in
    div [ id "outer" ] [ inner ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
