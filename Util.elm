module Util where

-- arrow keys
type alias Keys = { x : Int, y : Int }

-- point in 2D space
type alias Vec2 = { x : Float, y : Float }

zeroVec2 : Vec2
zeroVec2 = { x = 0, y = 0 }

vecScalarMul : Vec2 -> Float -> Vec2
vecScalarMul v c =
  { x = v.x * c, y = v.y * c }
