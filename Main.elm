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
import Ship exposing (..)
import Shot exposing (..)

-- CONSTANTS

shotMinY  = -1
moveRatio = 0.5
shotMaxY  = 1 / moveRatio
translucentGray = rgba 0 0 0 0.2

-- MODEL

type alias World =
  { player  : Ship
  , enemies : List Ship
  , shots   : List Shot
  }

-- initial values for records

initWorld : World
initWorld =
  { player = initShip
  , enemies = initEnemies
  , shots = []
  }

initEnemies : List Ship
initEnemies =
  let range = Array.toList (Array.initialize 10 identity)
      createEnemy = (\n ->
        { initShip | pos <- { x = -1 + toFloat n / 5, y = 0.8 }
                   , vel <- { x = 0, y = -0.1 } })
  in  List.map createEnemy range


-- UPDATE

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
  let (x, y) = (shot.pos.x, shot.pos.y)
  in  ngon 4 (r/200)
        |> filled translucentGray
        |> move (x * r * moveRatio, y * r * moveRatio)
        |> toBottom r

renderEnemy : Float -> Ship -> Form
renderEnemy r enemy =
  let (x, y) = (enemy.pos.x, enemy.pos.y)
  in  ngon 3 (r/50)
        |> filled translucentGray
        |> rotate (degrees 30)
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
        ngon 4 (h' / 20)
          |> filled translucentGray
          |> move (world.player.pos.x * r * moveRatio,
                   world.player.pos.y * r * moveRatio)
          |> toBottom r
      shots =
        List.map (renderShot r) world.shots
      enemies =
        List.map (renderEnemy r) world.enemies
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
