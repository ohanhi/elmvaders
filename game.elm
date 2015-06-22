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

initWorld : World
initWorld =
  { player = initShip
  , enemies = []
  , shots = []
  }


-- UPDATE
applyPhysics : Float -> Ship -> Ship
applyPhysics dt ship =
  let (x,y)   = ship.pos
      (vx,vy) = ship.vel
  in  { ship | pos <- (x + vx * dt, y) }

updateVel : (Float, Float) -> Ship -> Ship
updateVel newVel ship =
  { ship | vel <- newVel }

updateShooting : Bool -> Ship -> Ship
updateShooting isShooting ship =
  { ship | shooting <- isShooting }

updatePlayer : (Float, Keys) -> Ship -> Ship
updatePlayer (dt, keys) ship =
  let newVel      = (toFloat keys.x, 0)
      isShooting  = keys.y > 0
  in  updateVel newVel (updateShooting isShooting (applyPhysics dt ship))

updateEnemies : Float -> List Ship -> List Ship
updateEnemies dt enemies =
  List.map (applyPhysics dt) enemies

updateShots : Float -> List Shot -> List Shot
updateShots dt shots =
  shots

-- take dt, `Keys` and `World`, return next state
update : (Float, Keys) -> World -> World
update (dt, keys) world =
  let player  = updatePlayer (dt, keys) world.player
      enemies = updateEnemies dt world.enemies
      shots   = updateShots dt world.shots
  in  { world | player  <- player
              , enemies <- enemies
              , shots   <- shots }

-- SIGNALS
inputSignal : Signal (Float, Keys)
inputSignal =
  let delta = fps 30
      tuples = Signal.map2 (,) delta Keyboard.arrows
  in  Signal.sampleOn delta tuples

main : Signal Element
main = Signal.map show (Signal.foldp update initWorld inputSignal)
