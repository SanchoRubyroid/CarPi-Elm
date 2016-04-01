module CarPi where

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

import CarPiBehavior

-- CONSTANTS
fps = 30
torquePower = 3
brakePower = 3
steeringPower = 7

-- MODEL

type alias Model =
  { torqueLevel: Float,
    torqueReversed: Bool,
    directionLevel: Float,
    direction: Direction
  }

type Direction = Straight | Left | Right

initialCar : Model
initialCar =
  { torqueLevel = 0,
    torqueReversed = False,
    directionLevel = 0,
    direction = Straight
  }

-- UPDATE

type Action =
  NoOp |
  TurnLeft |
  TurnRight |
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
    |> applyDirection action


applyTorque : Action -> Model -> Model
applyTorque action car =
  let
    data =
      { power = torquePower,
        powerMutliplier = brakePower,
        level = car.torqueLevel,
        reversedLevel = car.torqueReversed }

    updatedData =
      applyBehavior [Accelerate, AccelerateLeft, AccelerateRight] [Reverse, ReverseLeft, ReverseRight] action data
  in
    { car |
      torqueLevel = updatedData.level,
      torqueReversed = updatedData.reversedLevel }

applyDirection : Action -> Model -> Model
applyDirection action car =
  let
    data =
      { power = steeringPower,
        powerMutliplier = 1,
        level = car.directionLevel,
        reversedLevel = (car.direction == Left) }

    updatedData =
      applyBehavior [TurnRight, ReverseRight, AccelerateRight] [TurnLeft, ReverseLeft, AccelerateLeft] action data

    updatedDirection =
      if updatedData.reversedLevel then
        Left
      else if updatedData.level == 0 then
        Straight
      else
        Right
  in
    { car |
      directionLevel = updatedData.level,
      direction = updatedDirection }

applyBehavior : List Action -> List Action -> Action -> CarPiBehavior.Data -> CarPiBehavior.Data
applyBehavior increaseList decreaseList action data =
  if List.any (\v -> action == v) increaseList then
    CarPiBehavior.applyIncrease data
  else if List.any (\v -> action == v) decreaseList then
    CarPiBehavior.applyDecrease data
  else
    CarPiBehavior.applyIdle data

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
        (1,0) -> TurnRight
        (-1,0) -> TurnLeft
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
