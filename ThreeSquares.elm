module ThreeSquares exposing (
    Model
    , Msg(..)
    , model
    , update
    , view)

import Html exposing (Html, div)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as JD
import Svg exposing (svg, rect, polygon)
import Svg.Attributes exposing (..)
import List as L
import String

-- DATA specifications

type alias Model = 
    { static : Bool
    , pos1 : Int
    , pos2 : Int 
    , outerColor : Int
    , innerColor : Int
    }

type Msg = 
    ToggleStatic
    | Canvas(Int,Int)
    | Outer (Int,Int)
    | Inner (Int,Int)

-- INITIAL model

model : Model
model = 
    { static = True
    , pos1 = 10
    , pos2 = 33
    , outerColor = 150 
    , innerColor = 250 
    }

-- UPDATE

update : Msg -> Model -> Model
update msg model =
    case (model.static, msg) of
        (_,ToggleStatic) ->
            {model | static = not model.static}
        (False, Canvas (x,y)) ->
            {model | pos1 = x % 400 // 4, pos2 = y % 400 // 4 }
        (False, Outer (x,y)) ->
            {model | outerColor = y % 400}
        (False, Inner (x,y)) ->
            {model | innerColor = y % 400}
        _ -> model

-- VIEW

toColor : Int -> String
toColor x =
    let 
        f x = 255 * (400 - 2 * abs (200 - x)) // 400
        r = f (x - 133) 
        g = f x 
        b = f (x + 133) 
    in
       [r,g,b]
       |> L.map toString
       |> L.intersperse ","
       |> String.concat
       |> \ s -> "rgba(" ++ s ++ ",0.4)"

view : Model -> Html Msg
view model =
    div [class "main", E.onClick ToggleStatic] [
        div [class "canvas"] [motif model]
        , div [class "controls"] [
            div [
                class "outer"
                , style ("background-color: " ++ toColor model.outerColor)
                , onMouseMove Outer
                ] []
            , div [
                class "inner"
                , style ("background-color: " ++ toColor model.innerColor)
                , onMouseMove Inner
                ] []
            ]
        ]

motif : Model -> Html Msg
motif model =
    let
        getPoints p = [(0,100-p),(p,0),(100,p),(100-p,100)]
                |> L.map (\(x,y)-> (toString x) ++ "," ++ (toString y)) 
                |> L.intersperse " "
                |> String.concat
    in
    svg [ viewBox "0 0 100 100", onMouseMove Canvas  ] [
        rect [
            width "100"
            , height "100"
            , style ("fill: " ++ toColor model.outerColor)
            ] []
        , polygon [
            class "s1"
            , style ("fill: " ++ toColor model.innerColor)
            , points (getPoints model.pos1)
            ] []
        , polygon [
            class "s2"
            , style ("fill: " ++ toColor model.innerColor)
            , points (getPoints model.pos2)
            ] []
        ]

onMouseMove : ((Int,Int) -> Msg) -> Html.Attribute Msg
onMouseMove f = E.on "mousemove" (JD.map f mousePos)

mousePos : JD.Decoder (Int,Int) 
mousePos = 
    JD.map2 (,) (JD.field "pageX" JD.int) (JD.field "pageY" JD.int)
     
