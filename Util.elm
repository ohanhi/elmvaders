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

vecAdd : Vec2 -> Vec2 -> Vec2
vecAdd a b =
  { x = a.x + b.x, y = a.y + b.y }

vecSubtract : Vec2 -> Vec2 -> Vec2
vecSubtract a b =
  vecAdd a (vecScalarMul b -1)

dotProduct : Vec2 -> Vec2 -> Vec2
dotProduct a b =
  { x = a.x * b.x, y = a.y * b.y }

-- rectangle in 2D space
type alias Rectangle = { center : Vec2, size : Vec2 }

{--
Two rectangles overlap if the distance between the central points
along the x and y axes are less than their sizes halved.
--}
overlap : Rectangle -> Rectangle -> Bool
overlap a b =
  let sub       = vecSubtract a.center b.center
      aSizeHalf = vecScalarMul a.size 0.5
      bSizeHalf = vecScalarMul b.size 0.5
      maxDx     = aSizeHalf.x + bSizeHalf.x
      maxDy     = aSizeHalf.y + bSizeHalf.y
  in  abs sub.x < maxDx && abs sub.y < maxDy
