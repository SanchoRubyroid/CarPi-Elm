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
    powerLevel: Float,
    turnDirection: Int
  }


initialCar : Model
initialCar =
  {
    power = 3,
    powerLevel = 0,
    turnDirection = 0
  }

-- UPDATE

update : { x : Int, y : Int } -> Model -> Model
update axis car =
  let
    directionalPower =
      if axis.y > 0 || ( axis.y == 0 && car.powerLevel < 0 ) then car.power else -1 * car.power
    appliedPowerLevel =
      if axis.y == 0 && car.powerLevel == 0 then car.powerLevel else car.powerLevel + directionalPower
    stabelizedZeroPowerLevel =
      if abs (car.powerLevel + appliedPowerLevel) < car.power then 0 else appliedPowerLevel
    getPowerLevel =
      clamp -100 100 stabelizedZeroPowerLevel
  in
  {car |
    turnDirection = axis.x,
    powerLevel = getPowerLevel}

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

getAxis : Signal { x : Int, y : Int }
getAxis =
  let
    delta = Time.fps 30
  in
    Signal.sampleOn delta Keyboard.arrows

model : Signal Model
model =
  Signal.foldp update initialCar getAxis


main : Signal Element
main =
  Signal.map2 view Window.dimensions model
