module Ship where

import Util exposing (Vec2, zeroVec2)

-- player controls a Ship
-- enemies are also Ships
type alias Ship =
  { pos : Vec2
  , vel : Vec2
  , shooting : Bool
  }

initShip : Ship
initShip =
  { pos = zeroVec2
  , vel = zeroVec2
  , shooting = False
  }

applyPhysics : Float -> Ship -> Ship
applyPhysics dt ship =
  let p = ship.pos
      v = ship.vel
  in  { ship | pos <- { x = p.x + v.x * dt
                      , y = p.y + v.y * dt } }

updateVel : Vec2 -> Ship -> Ship
updateVel newVel ship =
  { ship | vel <- newVel }

updateShooting : Bool -> Ship -> Ship
updateShooting isShooting ship =
  { ship | shooting <- isShooting }
