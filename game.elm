import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import Keyboard
import Window
import Debug

-- CONSTANTS

shotMinY  = -1
moveRatio = 0.5
shotMaxY  = 1 / moveRatio
translucentGray = rgba 0 0 0 0.2

-- MODEL

-- arrow keys
type alias Keys = { x : Int, y : Int }

-- point in 2D space
type alias Vec2 = { x : Float, y : Float }

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
zeroVec2 = { x = 0, y = 0 }

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

-- HELPERS

vecScalarMul : Vec2 -> Float -> Vec2
vecScalarMul v c =
  { x = v.x * c, y = v.y * c }

-- UPDATE
shipPhysics : Float -> Ship -> Ship
shipPhysics dt ship =
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

updatePlayer : (Float, Keys) -> Ship -> Ship
updatePlayer (dt, keys) ship =
  let newVel      = { x = toFloat keys.x, y = 0 }
      isShooting  = keys.y > 0
  in  ship
        |> shipPhysics dt
        |> updateShooting isShooting
        |> updateVel newVel

updateEnemies : Float -> List Ship -> List Ship
updateEnemies dt enemies =
  List.map (shipPhysics dt) enemies

shotPhysics : Float -> Shot -> Shot
shotPhysics dt shot =
  let p = shot.pos
      v = shot.vel
  in  { shot | pos <- { x = p.x + v.x * dt
                      , y = p.y + v.y * dt } }

addShot : Ship -> List Shot -> List Shot
addShot player shots =
  if player.shooting
    then { initShot | pos <- player.pos
                    , vel <- { x = 0, y = 1 } } :: shots
    else shots

updateShots : Float -> List Shot -> List Shot
updateShots dt shots =
  List.map (shotPhysics dt) shots

-- take dt, `Keys` and `World`, return next state
update : (Float, Keys) -> World -> World
update (dt, keys) world =
  let player  = updatePlayer (dt, keys) world.player
      enemies = updateEnemies dt world.enemies
      shots   = world.shots
                  |> updateShots dt
                  |> addShot player
                  |> List.filter (\shot ->
                                    shot.pos.y < shotMaxY &&
                                    shot.pos.y > shotMinY)
      debug   = Debug.watch "World" world
  in  { world | player  <- player
              , enemies <- enemies
              , shots   <- shots }


-- RENDER

toBottom : Float -> Form -> Form
toBottom r form =
  move (0, r * -0.5) form

renderShot : Float -> Shot -> Form
renderShot r shot =
  ngon 4 (r/200)
    |> filled translucentGray
    |> move (shot.pos.x * r * moveRatio, shot.pos.y * r * moveRatio)
    |> toBottom r


render : (Int, Int) -> World -> Element
render (w, h) world =
  let (w', h') = (toFloat w, toFloat h)
      r = h'
      bg =
        rect (w' * 2) (h' * 2)
          |> filled (rgb 230 240 255)
          |> move (w' * -0.5, h' * -0.5)
      player =
        ngon 4 (h' / 20)
          |> filled translucentGray
          |> move (world.player.pos.x * r * moveRatio,
                   world.player.pos.y * r * moveRatio)
          |> toBottom r
      shots =
        List.map (renderShot r) world.shots
      enemies =
        List.map (renderShot r) world.shots
  in  (collage w h
          (bg :: player :: (shots ++ enemies)))

-- SIGNALS
inputSignal : Signal (Float, Keys)
inputSignal =
  let delta = Signal.map (\n -> n / 1000) (fps 30)
      tuples = Signal.map2 (,) delta Keyboard.arrows
  in  Signal.sampleOn delta tuples

main : Signal Element
main = Signal.map2 render
           Window.dimensions
          (Signal.foldp update initWorld inputSignal)
