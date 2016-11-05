module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import List exposing (..)
import Navigation
import Utils
import String


main : Program Never
main =
    Navigation.program urlParser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { pageName : String }


init : Result String String -> ( Model, Cmd Msg )
init result =
    urlUpdate result { pageName = "home" }



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



-- Navigation


toUrl : String -> String
toUrl pageName =
    "#/" ++ pageName


fromUrl : String -> Result String String
fromUrl url =
    let
        urlList =
            Debug.log "URL: " (String.split "/" url)

        pageName =
            urlList
                |> drop 1
                |> take 1
                |> List.head
                |> Maybe.withDefault "home"
    in
        Ok pageName


urlParser : Navigation.Parser (Result String String)
urlParser =
    Navigation.makeParser (fromUrl << .hash)


urlUpdate : Result String String -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Ok pageName ->
            { pageName = pageName }
                ! []

        Err _ ->
            model
                ! [ Navigation.modifyUrl (toUrl model.pageName) ]



-- View


pageBody : Model -> Html Msg
pageBody model =
    case model.pageName of
        "" ->
            text "home"

        "home" ->
            text "home"

        "blog" ->
            text "blog"

        "about" ->
            text "about"

        "users" ->
            text "users"

        _ ->
            text "404 error"


menuStyle : Html.Attribute Msg
menuStyle =
    style [ ( "list-style-type", "none" ) ]


menuElementStyle : Html.Attribute Msg
menuElementStyle =
    style [ ( "display", "inline" ), ( "margin-left", "10px" ) ]


link : String -> String -> Html Msg
link name url =
    a [ href url ] [ text name ]


view : Model -> Html Msg
view model =
    div []
        [ ul [ menuStyle ]
            [ li [ menuElementStyle ] [ link "home" "#/home" ]
            , li [ menuElementStyle ] [ link "about" "#/about" ]
            , li [ menuElementStyle ] [ link "blog" "#/blog" ]
            ]
        , pageBody model
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none