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

-- MODEL

type alias Model =
  { power: Float,
    brakePower: Float,
    powerLevel: Float,
    reversedPower: Bool,
    turnDirection: Int
  }


initialCar : Model
initialCar =
  {
    power = 3,
    brakePower = 3,
    powerLevel = 0,
    reversedPower = False,
    turnDirection = 0
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
    decreasePowerLevel brakeApplied =
      let
        currenCarPower =
          if brakeApplied then car.brakePower * car.power else car.power
      in
        (car.powerLevel - currenCarPower) |> normalizedPowerLevel currenCarPower

    increasePowerLevel =
      (car.powerLevel + car.power) |> normalizedPowerLevel car.power

    normalizedPowerLevel currenCarPower newPowerLevel =
      let
        powerLevel =
          if abs (car.powerLevel + newPowerLevel) < currenCarPower then 0 else newPowerLevel
      in
        clamp 0 100 powerLevel

    newReversedPower newPowerLevel =
      if newPowerLevel == 0 then False else car.reversedPower
  in
    if action == NoOp then
      let
        newPowerLevel =
          if car.powerLevel == 0 then 0 else decreasePowerLevel False
      in
        { car |
          powerLevel = newPowerLevel,
          reversedPower = newReversedPower newPowerLevel }
    else if List.any (\v -> action == v) [Accelerate, AccelerateLeft, AccelerateRight] then
      let
        newPowerLevel =
          if car.reversedPower then
            decreasePowerLevel True
          else
            increasePowerLevel
      in
        { car |
          powerLevel = newPowerLevel,
          reversedPower = newReversedPower newPowerLevel }
    else if List.any (\v -> action == v) [Reverse, ReverseLeft, ReverseRight] then
      let
        newPowerLevel =
          if car.reversedPower then
            increasePowerLevel
          else
            decreasePowerLevel True
        revReversedPower =
          if car.powerLevel == 0 then True else car.reversedPower
      in
        { car |
          powerLevel = newPowerLevel,
          reversedPower = revReversedPower }
      else
        car
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
    delta = Time.fps 30
    toAction n =
      case (n.x, n.y) of
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
