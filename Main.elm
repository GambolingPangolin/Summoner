module Main exposing (main)

import Html exposing (
    program
    , Html
    , div
    , span
    , text
    )
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import ThreeSquares as TS

-- DATA

type Model =
    Welcome
    | TSModel TS.Model

type Msg =
    LoadComp Comp
    | TSMsg TS.Msg

type Comp = 
    ThreeSquares 
    | Triangulation

-- MAIN

main = Html.program {
    init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init = (Welcome, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- UPDATES

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model) of
        (LoadComp x, _) ->
            case x of 
                ThreeSquares ->
                    (TSModel TS.model, Cmd.none)
                Triangulation ->
                    (model, Cmd.none)
        (TSMsg a, TSModel b) -> (TS.update a b |> TSModel, Cmd.none)
        _ -> (model, Cmd.none)

-- VIEWS

view : Model -> Html Msg
view model =
    case model of
        Welcome -> welcome
        TSModel m -> Html.map TSMsg (TS.view m)

welcome : Html Msg
welcome = div [class "main"] [
    div [class "banner"] [text "Summoner."]
    , div [class "complist"] [
        span [onClick (LoadComp ThreeSquares)] [text "ThreeSquares"]
        ]
    ]
