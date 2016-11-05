module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
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
    Ok (String.dropLeft 2 url)


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
        "home" ->
            text "home"

        "blog" ->
            text "blog"

        "about" ->
            text "about"

        _ ->
            text "404 error"


inlineListStyle : Html.Attribute Msg
inlineListStyle =
    style [ ( "display", "inline" ), ( "margin-left", "10px" ) ]


link : String -> String -> Html Msg
link name url =
    a [ href url ] [ text name ]


view : Model -> Html Msg
view model =
    div []
        [ ul [ style [ ( "list-style-type", "none" ) ] ]
            [ li [ inlineListStyle ] [ link "home" "#/home" ]
            , li [ inlineListStyle ] [ link "about" "#/about" ]
            , li [ inlineListStyle ] [ link "blog" "#/blog" ]
            ]
        , pageBody model
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
