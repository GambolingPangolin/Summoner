module Main exposing (main)

import Html exposing (Html, beginnerProgram, div)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List as L
import String
import Debug exposing (log)

-- DATA specifications

type alias Model = { pos1 : Float, pos2 : Float }

type Msg = Position (Int,Int)

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
        Position (x,y) ->
              {model | pos1 = toFloat (x % 200) / 2, pos2 = toFloat (y % 200) / 2 }

-- VIEW

view : Model -> Html Msg
view model =
    div [class "main"] [
        div [class "canvas"] [motif model ]
        ]

motif : Model -> Html Msg
motif model =
    let
        getPoints p = [(0,100-p),(p,0),(100,p),(100-p,100)]
                |> L.map (\(x,y)-> (toString x) ++ "," ++ (toString y)) 
                |> L.intersperse " "
                |> String.concat
    in
    svg [ viewBox "0 0 100 100", E.on "mousemove" mousePos ] [
        rect [width "100", height "100"] []
        , polygon [class "s1", points (getPoints model.pos1)] []
        , polygon [class "s2", points (getPoints model.pos2)] []
        ]

mousePos : JD.Decoder Msg 
mousePos = 
  let
     xy = JD.map2 (,) (JD.field "x" JD.int) (JD.field "y" JD.int)
  in
     JD.map Position xy
     
