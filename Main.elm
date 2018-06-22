module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Json


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( initModel, sendHttpRequest initModel.username )


type alias Model =
    { username : String
    , user : User
    , error : Bool
    , recent : List String
    }


initModel : Model
initModel =
    { username = "GiuliaTeggi"
    , user = User "" "" 0
    , error = False
    , recent = [ "sineang01", "eadeheamingway", "helenzhou6" ]
    }


type Msg
    = UserData (Result Http.Error User)
    | Username String
    | FetchUser
    | SearchRecentUser String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchUser ->
            ( model, sendHttpRequest model.username )

        UserData (Ok user) ->
            let
                log =
                    Debug.log "This is the user data:" user
            in
                ( { model | user = user, error = False, recent = user.login :: model.recent }, Cmd.none )

        UserData (Err err) ->
            ( { model | error = True }, Cmd.none )

        Username user ->
            ( { model | username = user }, Cmd.none )

        SearchRecentUser recent ->
            ( { model | username = recent }, Cmd.none )


view : Model -> Html Msg
view model =
    section [ class "bg-near-white h-100 flex flex-column items-center" ]
        [ h1 [ class "orange courier" ] [ text "Search Github Users" ]
        , section []
            [ input [ type_ "text", placeholder "Username", class "courier", onInput Username ] []
            , button [ class "ma1 courier", onClick FetchUser ] [ text "Search" ]
            ]
        , section []
            [ if model.error then
                div [] [ text "Sorry, the search failed. Please try again." ]
              else
                section [ class "bg-light-gray flex flex-column items-center ma4" ]
                    [ img [ src model.user.avatar_url, class "w5 br-100 ma4 ba b--orange bw2" ] []
                    , a [ href (String.append "http://github.com/" model.user.login), class "link underline-hover orange courier pa0" ] [ text model.user.login ]
                    , div [ class "flex flex-row" ]
                        [ h4 [ class "pa0 courier" ] [ text "Followers: " ]
                        , h4 [ class "pa0 courier" ] [ text (toString model.user.followers) ]
                        ]
                    ]
            ]
        , section [ class "flex flex-row items-center" ]
            [ h3 [ class "courier" ] [ text "Recent searches" ]
            , section [ class "flex flex-row flex-wrap items-center" ] (List.map renderRecent model.recent)
            ]
        ]


renderRecent : String -> Html Msg
renderRecent recent =
    p [ class "orange courier pa2 link underline-hover", onClick (Username recent) ] [ text recent ]


getRequest : String -> Request User
getRequest username =
    let
        url =
            "http://api.github.com/users/" ++ username ++ "?access_token=8c16592e3fe66652c170d086ad734e8847106f0b"
    in
        Http.get url userDecoder


type alias User =
    { login : String
    , avatar_url : String
    , followers : Int
    }



-- Json Decoder is a decoder of type User (if type wasn't known it would be Decoder a)


userDecoder : Json.Decoder User
userDecoder =
    Json.map3 User
        (Json.field "login" Json.string)
        (Json.field "avatar_url" Json.string)
        (Json.field "followers" Json.int)


sendHttpRequest : String -> Cmd Msg
sendHttpRequest username =
    Http.send UserData <| getRequest username
