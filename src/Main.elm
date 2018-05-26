module Main exposing (..)

import Debug exposing (log)
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
    , ups : Int
    , source : Maybe String
    }


type alias Preview =
    { url : String
    }


type alias PostList =
    List Post


postDecoder : Decoder Post
postDecoder =
    JD.map4 Post
        (field "url" string)
        (field "title" string)
        (field "ups" int)
        (JD.maybe (at [ "preview", "images" ] <| JD.index 0 <| at [ "source", "url" ] string))


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
            ( { model | data = List.reverse (List.sortBy .ups post) }, Cmd.none )

        Posts (Err err) ->
            ( { model | error = toString err }, Cmd.none )

        FetchPosts ->
            ( model, fetchPosts model )

        RecordQuery query ->
            ( { model | query = query }, Cmd.none )


hasPreview : Post -> String
hasPreview post =
    Maybe.withDefault "http://place-hold.it/300x500" post.source


renderPost : Post -> Html Msg
renderPost post =
    div [ class "card" ]
        [ a [ href post.url ]
            [ img [ class "card-img-top", src (hasPreview post) ] []
            , div []
                [ span []
                    [ text post.title ]
                ]
            ]
        ]


renderPosts : Model -> Html Msg
renderPosts posts =
    div [] (List.map renderPost posts.data)


view : Model -> Html Msg
view model =
    let
        x =
            Debug.log "Fucks"

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
    , query = "marvel"
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
