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
shotSize  = { x = 0.005, y = 0.02 }
shotMaxY  = 1
moveRatio = 0.2
playerMoveRatio = 0.3
shootDelay = 500 -- ms
translucentGray = rgba 0 0 0 0.5
groundLevel = 0.03
playerHeight = 0.03

-- MODEL

type GameState = Splash | Playing | GameOver

type alias World =
  { player  : Ship
  , enemies : List Ship
  , shots   : List Shot
  , untilNextShot : Float
  , score   : Int
  , state   : GameState
  }

-- initial values for records

initPlayer =
  { initShip | pos  <- { x = 0, y = groundLevel + playerHeight/2 }
             , size <- { x = 0.07, y = playerHeight } }

initWorld : World
initWorld =
  { player = initPlayer
  , enemies = initEnemies
  , shots = []
  , untilNextShot = 0
  , score = 0
  , state = Splash
  }

initEnemies : List Ship
initEnemies =
  let range = Array.toList (Array.initialize 10 identity)
      createEnemy = (\n ->
        { initShip | pos <- { x = -0.5 + toFloat n / 10, y = 1 }
                   , vel <- { x = 0, y = -0.1 * moveRatio }
                   , size <- { x = 0.05, y = 0.025 }
                   })
  in  List.map createEnemy range


-- UPDATE

updatePlayer : (Float, Keys) -> Ship -> Ship
updatePlayer (dt, keys) ship =
  let newVel      = { x = toFloat keys.x * playerMoveRatio, y = 0 }
      isShooting  = keys.y > 0
  in  ship
        |> Ship.applyPhysics dt
        |> Ship.updateShooting isShooting
        |> Ship.updateVel newVel

-- get tuple of given Ship, and the Shots that hit it (often an empty list)
hitTuple : List (Shot, Rectangle) -> (Ship, Rectangle) -> (Ship, List Shot)
hitTuple shotRectTuples shipRectTuple =
  let (ship, shipR) = shipRectTuple
      hits = shotRectTuples
               |> List.map ( \(shot, shotR) ->
                            { hit=overlap shotR shipR
                            , shot=shot } )
               |> List.filter .hit
      shots = List.map .shot hits
  in  (ship, shots)

-- get list of Ships that have collided, and the Shots they collided with
findCollisions : List Shot -> List Ship -> List (Ship, List Shot)
findCollisions shots ships =
  let shotRectTuples =
        List.map (\s -> (s, {center=s.pos, size=shotSize})) shots
      shipRectTuples =
        List.map (\s -> (s, {center=s.pos, size=s.size})) ships
  in  shipRectTuples
        |> List.map (hitTuple shotRectTuples)
        |> List.filter (\(ship, shots) -> List.length shots > 0)

updateEnemies : Float -> List Ship -> List Ship
updateEnemies dt enemies =
  List.map (Ship.applyPhysics dt) enemies

playerShoot : Ship -> Float -> List Shot -> List Shot
playerShoot s untilNextShot shots =
  let sp = s.pos
      position = { sp | y <- sp.y + s.size.y / 2 }
  in if s.shooting && untilNextShot <= 0
        then { initShot | pos <- position
                        , vel <- { x = 0, y = 2 * moveRatio } } :: shots
        else shots

updateShots : Float -> List Shot -> List Shot
updateShots dt shots =
  List.map (Shot.applyPhysics dt) shots

-- take dt, `Keys` and `World`, return next state
update : (Float, Keys) -> World -> World
update (dt, keys) world =
  let dt'     = dt / 1000
      player  = updatePlayer (dt', keys) world.player
      collisions = findCollisions world.shots world.enemies
      collidedShips = List.map (\(s,_) -> s) collisions
      collidedShots = List.concatMap (\(_, shots) -> shots) collisions
      enemies = world.enemies
                  |> List.filter (\s -> not (List.member s collidedShips))
                  |> List.filter (\s -> s.pos.y > groundLevel)
                  |> updateEnemies dt'
      shots   = world.shots
                  |> playerShoot player world.untilNextShot
                  |> List.filter (\s -> not (List.member s collidedShots))
      untilNextShot =
        if List.length shots == List.length world.shots
          then
            if world.untilNextShot > 0
              then world.untilNextShot - dt
              else 0
          else shootDelay
      updatedShots =
        shots
          |> updateShots dt'
          |> List.filter (\shot ->
                            shot.pos.y < shotMaxY &&
                            shot.pos.y > shotMinY)
      debug   = Debug.watch "World" world
  in  { world | player  <- player
              , enemies <- enemies
              , shots   <- updatedShots
              , untilNextShot <- untilNextShot }


-- RENDER

fromBottom : Float -> Form -> Form
fromBottom r form =
  move (0, r * -0.5) form

renderShot : Float -> Shot -> Form
renderShot r shot =
  let (x, y) = (shot.pos.x, shot.pos.y)
  in  rect (r * shotSize.x) (r * shotSize.y)
        |> filled translucentGray
        |> fromBottom r
        |> move (x * r, y * r)

renderShip : Float -> Ship -> List Form
renderShip r ship =
  let (x, y) = (ship.pos.x, ship.pos.y)
      (w, h) = (ship.size.x, ship.size.y)
      dir    = if ship.vel.y < 0 then -1 else 1
      fillAndMove = \(xo, yo) f -> f
          |> filled (rgb 128 128 128)
          |> fromBottom r
          |> move ((x + xo) * r, (y + yo) * r)
      bodyH = h / 3
      dotR  = bodyH / 2
      body =
        rect (r * w) (r * bodyH) |> fillAndMove (0, dir * -dotR)
      dots =
        [ circle (r * dotR) |> fillAndMove (0, 0)
        , circle (r * dotR) |> fillAndMove (-w/2, dir * -dotR)
        , circle (r * dotR) |> fillAndMove (w/2, dir * -dotR)
        ]
  in  body :: dots

renderGame : (Float, Float) -> World -> List Form
renderGame (w', h') world =
  let r = h'
      bg =
        rect w' h'
          |> filled lightGray
      ground =
        rect w' (r * groundLevel * 2)
          |> filled (rgb 70 40 80)
          |> fromBottom r
      player = renderShip r world.player
      shots =
        List.map (renderShot r) world.shots
      enemies =
        List.concatMap (renderShip r) world.enemies
  in  bg :: ground :: player ++ shots ++ enemies

render : (Int, Int) -> World -> Element
render (w, h) world =
  let (w', h') = (toFloat w, toFloat h)
      gameForms = renderGame (w', h') world
  in  case world.state of
      Splash    -> collage w h gameForms
      Playing   -> collage w h gameForms
      GameOver  -> collage w h gameForms

-- SIGNALS
inputSignal : Signal (Float, Keys)
inputSignal =
  let delta = fps 30
      tuples = Signal.map2 (,) delta Keyboard.arrows
  in  Signal.sampleOn delta tuples

main : Signal Element
main = Signal.map2 render
           Window.dimensions
          (Signal.foldp update initWorld inputSignal)
