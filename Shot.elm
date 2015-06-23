module Shot where

import Util exposing (Vec2, zeroVec2)

-- "bullet"
type alias Shot =
  { pos : Vec2
  , vel : Vec2
  }

initShot : Shot
initShot =
  { pos = zeroVec2
  , vel = zeroVec2
  }

applyPhysics : Float -> Shot -> Shot
applyPhysics dt shot =
  let p = shot.pos
      v = shot.vel
  in  { shot | pos <- { x = p.x + v.x * dt
                      , y = p.y + v.y * dt } }
