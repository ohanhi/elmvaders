module Main where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import Keyboard
import Window
import Debug
import Array

import Util exposing (..)
import Ship exposing (Ship, initShip)
import Shot exposing (Shot, initShot)

-- CONSTANTS

shotMinY  = -1
shotSize  = { x = 0.01, y = 0.01 }
moveRatio = 0.5
shotMaxY  = 1 / moveRatio
translucentGray = rgba 0 0 0 0.5

-- MODEL

type alias World =
  { player  : Ship
  , enemies : List Ship
  , shots   : List Shot
  }

-- initial values for records

initPlayer =
  { initShip | size <- { x = 0.05, y = 0.05 } }

initWorld : World
initWorld =
  { player = initPlayer
  , enemies = initEnemies
  , shots = []
  }

initEnemies : List Ship
initEnemies =
  let range = Array.toList (Array.initialize 10 identity)
      createEnemy = (\n ->
        { initShip | pos <- { x = -1 + toFloat n / 5, y = 1.8 }
                   , vel <- { x = 0, y = -0.02 }
                   , size <- { x = 0.02, y = 0.02 }
                   })
  in  List.map createEnemy range


-- UPDATE

updatePlayer : (Float, Keys) -> Ship -> Ship
updatePlayer (dt, keys) ship =
  let newVel      = { x = toFloat keys.x, y = 0 }
      isShooting  = keys.y > 0
  in  ship
        |> Ship.applyPhysics dt
        |> Ship.updateShooting isShooting
        |> Ship.updateVel newVel

notHitWith : List Shot -> Ship -> Bool
notHitWith shots ship =
  let shipHit shotRects ship =
        shotRects
          |> List.map (Util.overlap {center = ship.pos, size = ship.size})
          |> List.any identity
      shotRects = List.map (\s -> {center = s.pos, size = shotSize}) shots
  in  not (shipHit shotRects ship)

updateEnemies : Float -> List Ship -> List Ship
updateEnemies dt enemies =
  List.map (Ship.applyPhysics dt) enemies

addShot : Ship -> List Shot -> List Shot
addShot player shots =
  if player.shooting
    then { initShot | pos <- player.pos
                    , vel <- { x = 0, y = 1 } } :: shots
    else shots

updateShots : Float -> List Shot -> List Shot
updateShots dt shots =
  List.map (Shot.applyPhysics dt) shots

-- take dt, `Keys` and `World`, return next state
update : (Float, Keys) -> World -> World
update (dt, keys) world =
  let player  = updatePlayer (dt, keys) world.player
      enemies = world.enemies
                  |> updateEnemies dt
                  |> List.filter (notHitWith world.shots)
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

fromBottom : Float -> Form -> Form
fromBottom r form =
  move (0, r * -0.5) form

renderShot : Float -> Shot -> Form
renderShot r shot =
  let (x, y) = (shot.pos.x, shot.pos.y)
  in  ngon 4 (r * shotSize.x)
        |> filled translucentGray
        |> fromBottom r
        |> move (x * r * moveRatio, y * r * moveRatio)

renderEnemy : Float -> Ship -> Form
renderEnemy r enemy =
  let (x, y) = (enemy.pos.x, enemy.pos.y)
  in  ngon 3 (r * enemy.size.x)
        |> filled translucentGray
        |> rotate (degrees 30)
        |> fromBottom r
        |> move (x * r * moveRatio, y * r * moveRatio)

render : (Int, Int) -> World -> Element
render (w, h) world =
  let (w', h') = (toFloat w, toFloat h)
      r = h'
      bg =
        rect (w' * 2) (h' * 2)
          |> filled (rgb 230 240 255)
          |> move (w' * -0.5, h' * -0.5)
      player =
        ngon 4 (r * world.player.size.x)
          |> filled translucentGray
          |> fromBottom r
          |> move (world.player.pos.x * r * moveRatio,
                   world.player.pos.y * r * moveRatio)
      shots =
        List.map (renderShot r) world.shots
      enemies =
        List.map (renderEnemy r) world.enemies
      allForms =
        bg :: player :: shots ++ enemies
  in  collage w h allForms

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
