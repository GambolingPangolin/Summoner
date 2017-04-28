module Main exposing (main)

import Html exposing (Html, beginnerProgram, div)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List as L
import String

-- DATA specifications

type alias Model = { pos1 : Float, pos2 : Float }

type Msg =
    Pos1 Float
    | Pos2 Float

-- MAIN declaration

main = Html.beginnerProgram {
    model = model
    , update = update
    , view = view
    }

-- INITIAL model

model : Model
model = { pos1 = 10, pos2 = 33 }

-- UPDATE

update : Msg -> Model -> Model
update msg model =
    case msg of
        Pos1 x ->
            {model | pos1 = x}
        Pos2 x ->
            {model | pos2 = x}

-- VIEW

view : Model -> Html Msg
view model =
    div [class "main"] [
        div [class "canvas"] [motif model.pos1 model.pos2 ]
        , div [class "controls"] [
            slider model.pos1 Pos1
            , slider model.pos2 Pos2
            ]
        ]

motif : Float -> Float -> Html Msg
motif x y =
    let
        getPoints p = [(0,100-p),(p,0),(100,p),(100-p,100)]
                |> L.map (\(x,y)-> (toString x) ++ "," ++ (toString y)) 
                |> L.intersperse " "
                |> String.concat
    in
    svg [ viewBox "0 0 100 100" ] [
        rect [width "100", height "100"] []
        , polygon [class "s1", points (getPoints x)] []
        , polygon [class "s2", points (getPoints y)] []
        ]

slider : Float -> (Float -> Msg) -> Html Msg
slider x f =
    svg [ viewBox "0 0 100 10", onSlide f] [
        line [ x1 "0", y1 "5", x2 "100", y2 "5" ] []
        , circle [ x |> toString |> cx, cy "5", r "2" ] []
        ]

onSlide : (Float -> Msg) -> Html.Attribute Msg
onSlide m = E.onClick (m 60)
