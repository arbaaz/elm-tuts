module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder, at, field, int, list, map3, string)


-- json : String
-- json =
--     """
-- {
--   "kind": "Listing",
--   "data": {
--     "children": [
--       {"data":{"url":"http://www.example.com"}},
--       {"data":{"url": "http://www.example.com"}}
--     ]
--   }
-- }
-- """


type alias Post =
    { url : String
    , title : String
    }


type alias Url =
    { url : String
    , width : Int
    , height : Int
    }


urlDecoder : Decoder Url
urlDecoder =
    map3 Url
        (at [ "url" ] string)
        (at [ "width" ] int)
        (at [ "width" ] int)


type alias Source =
    { source : Url
    }


type alias SourceList =
    { images : List Source
    }


type alias PostList =
    List Post


postDecoder : Decoder Post
postDecoder =
    JD.map2 Post
        (field "url" string)
        (field "title" string)


postsDecoder : Decoder PostList
postsDecoder =
    let
        decoder =
            at [ "data" ] postDecoder
                |> JD.list
    in
    at [ "data", "children" ] decoder


fetchPosts : Model -> Cmd Msg
fetchPosts model =
    let
        url =
            "//www.reddit.com/r/" ++ model.query ++ "/hot.json?limit=100&count=100"

        request =
            Http.get url postsDecoder

        cmd =
            Http.send Posts request
    in
    cmd


type Msg
    = Posts (Result Http.Error PostList)
    | FetchPosts
    | RecordQuery String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Posts (Ok post) ->
            ( { model | data = post }, Cmd.none )

        Posts (Err err) ->
            ( { model | error = toString err }, Cmd.none )

        FetchPosts ->
            ( model, fetchPosts model )

        RecordQuery query ->
            ( { model | query = query }, Cmd.none )


renderPost : Post -> Html Msg
renderPost post =
    li [ class "list-group-item" ]
        [ a [ href post.url ] [ text post.title ]
        ]


renderPosts : Model -> Html Msg
renderPosts posts =
    ul [ class "list-group" ] (List.map renderPost posts.data)


view : Model -> Html Msg
view model =
    let
        inner =
            div []
                [ input [ onInput RecordQuery ] []
                , button
                    [ onClick FetchPosts
                    , class "btn btn-primary"
                    ]
                    [ text "Fetch Reddit Links" ]
                , br [] []
                , renderPosts model
                ]
    in
    div [ id "outer" ]
        [ inner
        , div [] [ text model.error ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { data : PostList
    , query : String
    , error : String
    }


initModel : Model
initModel =
    { data = []
    , query = "elm"
    , error = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, fetchPosts initModel )


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
