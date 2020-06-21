module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes


main =
    Browser.sandbox { init = init, view = view, update = update }



--Model


type alias Model =
    { player : Player
    , enemy : Enemy
    , enemyDeaths : Int
    , page : Page
    }


type alias Player =
    { attackPower : Int
    , hP : Int
    }


type alias Enemy =
    { attackPower : Int
    , hP : Int
    }



--init


init : Model
init =
    { player = initPlayer
    , enemy = initEnemy
    , enemyDeaths = initEnemyDeaths
    , page = Game
    }


initPlayer =
    { attackPower = 3, hP = 10 }


initEnemy =
    { attackPower = 1, hP = 4 }


initEnemyDeaths =
    0


type Msg
    = NoOp
    | ChangePage Page
    | AttackEnemy


type Page
    = Game
    | Win


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ChangePage page ->
            case model.page of
                Game ->
                    { model | page = page }

                Win ->
                    { model | page = page, player = initPlayer, enemy = initEnemy, enemyDeaths = initEnemyDeaths }

        AttackEnemy ->
            let
                --NewEnemy
                oldEnemy =
                    model.enemy

                newHpFromEnemy =
                    model.enemy.hP - model.player.attackPower

                respawnEnemy =
                    { attackPower = 1, hP = 4 }

                newEnemy =
                    --if the enemys hp is less than or equal too 0 then make newEnemy
                    --have the value of function respawnEnemy
                    --else just lower the enemy hp down from whatever value the players attack power is
                    if newHpFromEnemy <= 0 then
                        respawnEnemy

                    else
                        { oldEnemy
                            | hP = newHpFromEnemy
                        }

                newEnemyDeaths =
                    if newHpFromEnemy <= 0 then
                        model.enemyDeaths + 1

                    else
                        model.enemyDeaths

                --newPlayer =
                oldPlayer =
                    model.player

                newPlayer =
                    if newHpFromEnemy > 0 then
                        --Attack the player after player attacks enemy.
                        { oldPlayer | hP = model.player.hP - model.enemy.attackPower }

                    else
                        oldPlayer
            in
            if newEnemyDeaths >= 3 then
                { model | page = Win }

            else
                { model | enemy = newEnemy, player = newPlayer, enemyDeaths = newEnemyDeaths }



-- When player attacks, remove hp from enemy and then enemy can attack if enemy hp is above 0


view : Model -> Html.Html Msg
view model =
    layout []
        (viewGame model)


htmlBackgroundAudio : Element msg
htmlBackgroundAudio =
    Element.html
        (Html.audio [ Html.Attributes.autoplay True, Html.Attributes.controls True ]
            [ Html.source [ Html.Attributes.src "https://freepd.com/music/After%20the%20End.mp3", Html.Attributes.type_ "audio/mpeg" ] []
            , Html.text "Your browser does not support the audio element."
            ]
        )


viewGame : Model -> Element Msg
viewGame model =
    case model.page of
        Game ->
            let
                playerDeath =
                    model.player.hP <= 0

                enemyDeath =
                    model.enemy.hP <= 0
            in
            if playerDeath then
                row [ centerX, centerY ]
                    [ column
                        [ padding 100
                        , Background.color (rgba255 211 74 40 0.9)
                        ]
                        [ text "Hello Enemy!"
                        , text "Enemy wins!"
                        , text ("Enemy Health " ++ String.fromInt model.enemy.hP)
                        ]
                    ]

            else if enemyDeath then
                row [ centerX, centerY ]
                    [ column
                        [ padding 100
                        , Background.color (rgba255 211 74 40 0.9)
                        ]
                        [ text "Hello Player!"
                        , text "Player wins!"
                        , text ("Player Health " ++ String.fromInt model.player.hP)
                        , Input.button [ padding 10, Background.color (rgb255 102 168 135), Font.size 12 ]
                            { onPress = Just AttackEnemy, label = text "Attack Enemy!" }
                        ]
                    ]

            else
                column [ centerX, centerY ]
                    [ row []
                        [ column
                            [ padding 100
                            , Background.color (rgba255 211 74 40 0.9)
                            ]
                            [ text "Hello Player!"
                            , text ("Player Health " ++ String.fromInt model.player.hP)
                            , Input.button [ padding 10, Background.color (rgb255 102 168 135), Font.size 12 ]
                                { onPress = Just AttackEnemy, label = text "Attack Enemy!" }
                            ]
                        , column
                            [ padding 100
                            , Background.color (rgba255 211 74 40 0.9)
                            ]
                            [ text "Hello Enemy!"
                            , text ("Enemy Health " ++ String.fromInt model.enemy.hP)
                            ]
                        ]
                    , row
                        [ centerX, centerY, alignBottom, paddingEach { top = 100, right = 0, bottom = 0, left = 0 } ]
                        [ column []
                            [ htmlBackgroundAudio ]
                        ]
                    ]

        Win ->
            row [ centerX, centerY ]
                [ column
                    [ padding 300
                    , Background.color (rgba255 211 74 40 0.9)
                    , Font.size 50
                    ]
                    [ text "YOU WON!"
                    , image [] { src = "https://media.giphy.com/media/iJc8fraqaofWo/giphy.gif", description = "A firework gif" }
                    , Input.button [ padding 10, Background.color (rgb255 102 168 135), Font.size 12 ]
                        { onPress = Just (ChangePage Game), label = text "New game" }
                    ]
                ]
