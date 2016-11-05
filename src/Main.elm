module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import List exposing (..)
import Navigation
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
    { currentRoute : RoutePath
    , users : List User
    }


type alias User =
    { id : Int
    , name : String
    , hobbies : List Hobby
    }


type alias Hobby =
    String


type alias RoutePath =
    List String


type alias Route =
    Result String RoutePath


initialUsers : List User
initialUsers =
    [ User 1 "Fred" [ "running", "climbing" ]
    , User 2 "Joe" [ "kayaking", "poodle gromming", "goat soccer" ]
    , User 3 "Mark" [ "knitting", "kombucha making" ]
    ]


init : Route -> ( Model, Cmd Msg )
init route =
    urlUpdate route
        { currentRoute = [ "home" ]
        , users = initialUsers
        }



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



-- Navigation


toUrl : RoutePath -> String
toUrl currentRoute =
    "#/" ++ (String.join "/" currentRoute)


fromUrl : String -> Route
fromUrl url =
    let
        urlList =
            String.split "/" url

        routeElements =
            urlList
                |> drop 1
    in
        Ok routeElements


urlParser : Navigation.Parser Route
urlParser =
    Navigation.makeParser (fromUrl << .hash)


urlUpdate : Route -> Model -> ( Model, Cmd Msg )
urlUpdate route model =
    case route of
        Ok routeElements ->
            { model | currentRoute = routeElements }
                ! []

        Err _ ->
            model
                ! [ Navigation.modifyUrl (toUrl model.currentRoute) ]



-- View


homePage : Html Msg
homePage =
    text "home"


aboutPage : Html Msg
aboutPage =
    text "about"


notFoundPage : Html Msg
notFoundPage =
    text "404 error"


usersPage : Model -> Html Msg
usersPage model =
    ul []
        (List.map
            (\user ->
                li []
                    [ a
                        [ href ("/#/users/" ++ toString user.id) ]
                        [ text user.name ]
                    ]
            )
            model.users
        )


userFromId : List User -> String -> Maybe User
userFromId users idStr =
    let
        id =
            Result.withDefault 0 (String.toInt idStr)
    in
        List.filter (\user -> id == user.id) users
            |> head


userPage : Model -> String -> Html Msg
userPage model idStr =
    let
        user =
            userFromId model.users idStr
    in
        case user of
            Just u ->
                div []
                    [ text ("Details for user: ")
                    , a
                        [ href ("/#/users/" ++ idStr ++ "/hobbies") ]
                        [ text u.name ]
                    ]

            Nothing ->
                text "user not found"


hobbiesPage : Model -> String -> Html Msg
hobbiesPage model idStr =
    let
        user =
            userFromId model.users idStr
    in
        case user of
            Just u ->
                ul []
                    (List.map (\hobby -> li [] [ text hobby ]) u.hobbies)

            Nothing ->
                text "user not found"


pageBody : Model -> Html Msg
pageBody model =
    case model.currentRoute of
        [] ->
            homePage

        [ "home" ] ->
            homePage

        [ "about" ] ->
            aboutPage

        [ "users" ] ->
            usersPage model

        [ "users", userId ] ->
            userPage model userId

        [ "users", userId, "hobbies" ] ->
            hobbiesPage model userId

        _ ->
            notFoundPage


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
            , li [ menuElementStyle ] [ link "users" "#/users" ]
            ]
        , pageBody model
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
