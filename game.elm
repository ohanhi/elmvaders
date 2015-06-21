import Graphics.Element exposing (..)
import Time exposing (..)
import Keyboard

-- MODEL

-- arrow keys
type alias Keys = { x : Int, y : Int }

-- point in 2D space
type alias Vec2 = (Float, Float)

-- "bullet"
type alias Shot =
  { pos : Vec2
  , vel : Vec2
  }

-- player controls a Ship
-- enemies are also Ships
type alias Ship =
  { pos : Vec2
  , vel : Vec2
  , shooting : Bool
  }

type alias World =
  { player  : Ship
  , enemies : List Ship
  , shots   : List Shot
  }

-- initial values for records

zeroVec2 : Vec2
zeroVec2 = (0, 0)

initShip : Ship
initShip =
  { pos = zeroVec2
  , vel = zeroVec2
  , shooting = False
  }

initShot : Shot
initShot =
  { pos = zeroVec2
  , vel = zeroVec2
  }



-- UPDATE
applyPhysics : Float -> Ship -> Ship
applyPhysics dt ship =
  let (x,y)   = ship.pos
      (vx,vy) = ship.vel
  in  { ship | pos <- (x + vx * dt, y) }

updateVx : Float -> Ship -> Ship
updateVx newVx ship =
  let (vx, vy)  = ship.vel
      newVel    = (newVx, vy)
  in  { ship | vel <- newVel }

updateShooting : Bool -> Ship -> Ship
updateShooting isShooting ship =
  { ship | shooting <- isShooting }

update : (Float, Keys) -> Ship -> Ship
update (dt, keys) ship =
  let newVel      = toFloat keys.x
      isShooting  = keys.y > 0
  in  updateVx newVel (updateShooting isShooting (applyPhysics dt ship))


-- SIGNALS
inputSignal : Signal (Float, Keys)
inputSignal =
  let delta = fps 30
      tuples = Signal.map2 (,) delta Keyboard.arrows
  in  Signal.sampleOn delta tuples

main : Signal Element
main = Signal.map show (Signal.foldp update initShip inputSignal)
