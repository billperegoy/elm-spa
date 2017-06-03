module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)
import Navigation
import String


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { currentRoute : Navigation.Location
    , users : List User
    }


type alias User =
    { id : Int
    , name : String
    , hobbies : List Hobby
    }


type alias Hobby =
    String


type RoutePath
    = DefaultRoute
    | HomeRoute
    | AboutRoute
    | UsersRoute
    | UserRoute String
    | HobbiesRoute String
    | NotFoundRoute


initialUsers : List User
initialUsers =
    [ User 1 "Fred" [ "running", "climbing" ]
    , User 2 "Joe" [ "kayaking", "poodle grooming", "goat soccer" ]
    , User 3 "Mark" [ "knitting", "kombucha making" ]
    ]


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { currentRoute = location
    , users = initialUsers
    }
        ! []



-- Update


type Msg
    = UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            { model | currentRoute = location } ! []



-- Navigation


fromUrlHash : String -> RoutePath
fromUrlHash urlHash =
    let
        hashList =
            urlHash |> String.split "/" |> drop 1
    in
        case hashList of
            [] ->
                DefaultRoute

            [ "home" ] ->
                HomeRoute

            [ "about" ] ->
                AboutRoute

            [ "users" ] ->
                UsersRoute

            [ "users", userId ] ->
                UserRoute userId

            [ "users", userId, "hobbies" ] ->
                HobbiesRoute userId

            _ ->
                NotFoundRoute



-- View


userFromId : List User -> String -> Maybe User
userFromId users idStr =
    let
        id =
            Result.withDefault 0 (String.toInt idStr)
    in
        List.filter (\user -> id == user.id) users
            |> head


homePage : Html Msg
homePage =
    h1 [] [ text "Home" ]


aboutPage : Html Msg
aboutPage =
    h1 [] [ text "About" ]


notFoundPage : Html Msg
notFoundPage =
    h1 [] [ text "Page Not Found" ]


usersPage : Model -> Html Msg
usersPage model =
    div []
        [ h1 [] [ text "Users" ]
        , ul []
            (List.map
                (\user ->
                    li [] [ link user.name ("/#/users/" ++ toString user.id) ]
                )
                model.users
            )
        ]


userPage : Model -> String -> Html Msg
userPage model idStr =
    let
        user =
            userFromId model.users idStr
    in
        case user of
            Just u ->
                div []
                    [ h1 [] [ text ("User Profile") ]
                    , h2 []
                        [ link u.name ("/#/users/" ++ idStr ++ "/hobbies") ]
                    ]

            Nothing ->
                div []
                    [ h1 [] [ text "User not found" ]
                    ]


hobbiesPage : Model -> String -> Html Msg
hobbiesPage model idStr =
    let
        user =
            userFromId model.users idStr
    in
        case user of
            Just u ->
                div []
                    [ h1 [] [ text "User Hobbies" ]
                    , ul []
                        (List.map (\hobby -> li [] [ text hobby ]) u.hobbies)
                    ]

            Nothing ->
                text "user not found"


pageBody : Model -> Html Msg
pageBody model =
    let
        routePath =
            fromUrlHash model.currentRoute.hash
    in
        case routePath of
            DefaultRoute ->
                homePage

            HomeRoute ->
                homePage

            AboutRoute ->
                aboutPage

            UsersRoute ->
                usersPage model

            UserRoute userId ->
                userPage model userId

            HobbiesRoute userId ->
                hobbiesPage model userId

            NotFoundRoute ->
                notFoundPage


link : String -> String -> Html Msg
link name url =
    a [ href url ] [ text name ]


header : Html Msg
header =
    let
        menuStyle =
            style [ ( "list-style-type", "none" ) ]

        menuElementStyle =
            style [ ( "display", "inline" ), ( "margin-left", "10px" ) ]
    in
        ul [ menuStyle ]
            [ li [ menuElementStyle ] [ link "home" "#/home" ]
            , li [ menuElementStyle ] [ link "about" "#/about" ]
            , li [ menuElementStyle ] [ link "users" "#/users" ]
            ]


view : Model -> Html Msg
view model =
    div [ style [ ( "margin", "20px" ) ] ]
        [ header
        , pageBody model
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
