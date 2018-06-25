module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Json


-- Initialising app


main : Program Never Model Msg
main =
    -- A program describes how the app works
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    -- use initModel as initial model and send an http request as initial command
    ( initModel, sendHttpRequest initModel.username )



-- MODEL


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



-- UPDATE


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
            -- add username to model.recent list only if it isn't already in the list
            if List.member user.login model.recent then
                ( { model | user = user, error = False }, Cmd.none )
            else
                ( { model | user = user, error = False, recent = user.login :: model.recent }, Cmd.none )

        UserData (Err err) ->
            ( { model | error = True }, Cmd.none )

        Username user ->
            ( { model | username = user }, Cmd.none )

        SearchRecentUser recent ->
            let
                log =
                    Debug.log "Recent" recent
            in
                ( { model | username = recent }, sendHttpRequest model.username )



-- VIEW


view : Model -> Html Msg
view model =
    section [ class "bg-near-white h-100 flex flex-column items-center" ]
        [ h1 [ class "orange courier" ] [ text "Search Github Users" ]
        , section []
            [ input [ type_ "text", placeholder "Username", class "courier", onInput Username ] []
            , button [ class "ma1 courier", onClick FetchUser ] [ text "Search" ]
            ]
        , section []
            [ section [ class "bg-light-gray flex flex-column items-center ma4" ]
                [ if model.error then
                    div [] [ text "Sorry, the search failed. Please try again." ]
                  else
                    img [ src model.user.avatar_url, class "w5 br-100 ma4 ba b--orange bw2" ] []
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



-- Render list of recently searched users (model.recent)


renderRecent : String -> Html Msg
renderRecent recent =
    p [ class "orange courier pa2 link underline-hover pointer", onClick (SearchRecentUser recent) ] [ text recent ]



-- Build http GET request


getRequest : String -> Request User
getRequest username =
    let
        url =
            "https://api.github.com/users/" ++ username
    in
        Http.get url userDecoder



-- Type alias of result of http request


type alias User =
    { login : String
    , avatar_url : String
    , followers : Int
    }



-- Decode JSON object returned from http request using User type alias


userDecoder : Json.Decoder User
userDecoder =
    Json.map3 User
        (Json.field "login" Json.string)
        (Json.field "avatar_url" Json.string)
        (Json.field "followers" Json.int)



-- Send http request


sendHttpRequest : String -> Cmd Msg
sendHttpRequest username =
    let
        log =
            Debug.log "send http request reached" username
    in
        Http.send UserData <| getRequest username
