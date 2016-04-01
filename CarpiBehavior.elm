module CarPiBehavior where

type alias Data =
  { power: Float,
    powerMutliplier: Float,
    level: Float,
    reversedLevel: Bool
  }

applyIncrease : Data -> Data
applyIncrease data =
  (if data.reversedLevel then decreaseLevel data True else increaseLevel data)
    |> setReverse False data

applyDecrease : Data -> Data
applyDecrease data =
  (if data.reversedLevel then increaseLevel data else decreaseLevel data True)
    |> setReverse True data

applyIdle : Data -> Data
applyIdle data =
  if data.level == 0 then data
  else
    decreaseLevel data False |> setReverse False data

-- PRIVATE

decreaseLevel : Data -> Bool -> Data
decreaseLevel data brakeApplied =
  let
    currenDataPower =
      if brakeApplied then data.powerMutliplier * data.power else data.power
  in
    (data.level - currenDataPower) |> normalizeLevel data

increaseLevel : Data -> Data
increaseLevel data =
  (data.level + data.power) |> normalizeLevel data

normalizeLevel : Data -> Float -> Data
normalizeLevel data newLevel =
  { data | level = clamp 0 100 newLevel }

setReverse : Bool -> Data -> Data -> Data
setReverse reversing data updatedData =
  if reversing then
    if data.level == 0 then { updatedData | reversedLevel = True } else updatedData
  else
    if updatedData.level == 0 then { updatedData | reversedLevel = False } else updatedData
