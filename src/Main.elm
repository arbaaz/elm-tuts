module Main exposing (..)

-- import Html.Events exposing (onClick)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as JD exposing (Decoder, at, field, int, list, map3, string)


type alias Post =
    { url : String
    }


type alias PostList =
    List Post


type alias Model =
    PostList


initModel : Model
initModel =
    []


init : ( Model, Cmd Msg )
init =
    ( initModel, fetchPosts )


postDecoder : Decoder Post
postDecoder =
    JD.map Post
        (field "url" string)


postsDecoder : Decoder PostList
postsDecoder =
    at [ "data", "children" ] <|
        JD.list <|
            at [ "data" ] postDecoder



-- json : String
-- json =
--     """
-- {
--   "kind": "Listing",
--   "data": {
--     "children": [
--       {"data":{"url":"Hello"}},
--       {"data":{"url": "world"}}
--     ]
--   }
-- }
-- """


fetchPosts : Cmd Msg
fetchPosts =
    let
        url =
            "//www.reddit.com/r/elm/.json?limit=20&count=20"

        request =
            Http.get url postsDecoder

        cmd =
            Http.send Posts request
    in
    cmd


type Msg
    = Posts (Result Http.Error PostList)
    | FetchPosts


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Posts (Ok post) ->
            ( post, Cmd.none )

        Posts (Err err) ->
            ( model, Cmd.none )

        FetchPosts ->
            ( model, fetchPosts )


renderPost : Post -> Html Msg
renderPost post =
    li [ class "list-group-item" ] [ text (toString post) ]


renderPosts : Model -> Html Msg
renderPosts posts =
    ul [ class "list-group" ] (List.map renderPost posts)


view : Model -> Html Msg
view model =
    let
        inner =
            div []
                [ -- button
                  --     [ onClick FetchPosts
                  --     , class "btn btn-primary"
                  --     ]
                  --     [ text "Fetch Jokes" ]
                  -- ,
                  br [] []
                , renderPosts model
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
