module CarpiWeb where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import Keyboard
import Window
import Time

import String

import CarpiBehavior

-- CONSTANTS
fps = 30
power = 3
brakePower = 3

-- MODEL

type alias Model =
  { torqueLevel: Float,
    torqueReversed: Bool
  }


initialCar : Model
initialCar =
  { torqueLevel = 0,
    torqueReversed = False
  }

-- UPDATE

type Action =
  NoOp |
  Left |
  Right |
  Accelerate |
  AccelerateLeft |
  AccelerateRight |
  Reverse |
  ReverseLeft |
  ReverseRight


update : Action -> Model -> Model
update action car =
  car
    |> applyTorque action
    --|> applyDirection action


applyTorque : Action -> Model -> Model
applyTorque action car =
  let
    data =
      { power = power,
        powerMutliplier = brakePower,
        level = car.torqueLevel,
        reversedLevel = car.torqueReversed }
    updatedData =
      if List.any (\v -> action == v) [Accelerate, AccelerateLeft, AccelerateRight] then
        CarpiBehavior.applyIncrease data
      else if List.any (\v -> action == v) [Reverse, ReverseLeft, ReverseRight] then
        CarpiBehavior.applyDecrease data
      else
        CarpiBehavior.applyIdle data
  in
    { car |
      torqueLevel = updatedData.level,
      torqueReversed = updatedData.reversedLevel }

-- VIEW

view : (Int, Int) -> Model -> Element
view (w, h) car =
  let
    (w', h') = (toFloat w, toFloat h)
  in
    collage w h
      [ drawCanvas w' h',
        toForm (show car)
      ]


drawCanvas : Float -> Float -> Form
drawCanvas w h =
  rect w h
    |> filled gray

-- SIGNALS

input : Signal Action
input =
  let
    delta = Time.fps fps
    toAction axis =
      case (axis.x, axis.y) of
        (1,0) -> Right
        (-1,0) -> Left
        (0,1) -> Accelerate
        (1,1) -> AccelerateRight
        (-1,1) -> AccelerateLeft
        (0,-1) -> Reverse
        (1,-1) -> ReverseRight
        (-1,-1) -> ReverseLeft
        _ -> NoOp

    actions = Signal.map toAction(Keyboard.arrows)
  in
    Signal.sampleOn delta actions

model : Signal Model
model =
  Signal.foldp update initialCar input


main : Signal Element
main =
  Signal.map2 view Window.dimensions model
