module Main where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time exposing (..)
import Keyboard
import Mouse
import Window
import Debug
import Array
import Text exposing (Text)

import Util exposing (..)
import Ship exposing (Ship, initShip)
import Shot exposing (Shot, initShot)

-- CONSTANTS

shotMinY  = -1
shotSize  = { x = 0.005, y = 0.02 }
shotMaxY  = 1
shotMoveRatio = 0.2
enemyMoveRatio = 0.02
playerMoveRatio = 0.3
shootDelay = 500 -- ms
waveDelay = 10000 -- ms
shipsPerWave = 8
translucentGray = rgba 0 0 0 0.5
groundLevel = 0.03
playerHeight = 0.03
initDifficulty = 10
difficultyStep = 10

-- MODEL

type GameState = Splash | Playing | GameOver

type alias World =
  { player  : Ship
  , enemies : List Ship
  , shots   : List Shot
  , untilNextShot : Float
  , untilNextWave : Float
  , score   : Int
  , state   : GameState
  , difficulty : Int
  }

-- initial values for records

initPlayer =
  { initShip | pos  <- { x = 0, y = groundLevel + playerHeight/2 }
             , size <- { x = 0.07, y = playerHeight } }

initWorld : World
initWorld =
  { player = initPlayer
  , enemies = initEnemies initDifficulty
  , difficulty = initDifficulty
  , shots = []
  , untilNextShot = 0
  , untilNextWave = 0
  , score = 0
  , state = Splash
  }

initEnemies : Int -> List Ship
initEnemies value =
  let range = Array.toList (Array.initialize shipsPerWave identity)
      createEnemy = (\n ->
        { initShip | pos <- { x = -0.5 + toFloat n / shipsPerWave, y = 1.025 }
                   , vel <- { x = 0, y = -(enemyMoveRatio * (1 + toFloat value / 100)) }
                   , size <- { x = 0.05, y = 0.025 }
                   , value <- value
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
                        , vel <- { x = 0, y = 2 * shotMoveRatio } } :: shots
        else shots

updateShots : Float -> List Shot -> List Shot
updateShots dt shots =
  List.map (Shot.applyPhysics dt) shots

-- take dt, `Keys` and `World`, return next state
updatePlay : (Float, Keys) -> World -> World
updatePlay (dt, keys) world =
  let dt'     = dt / 1000
      dead    = List.any (\s -> s.pos.y <= groundLevel) world.enemies
      state   = if dead then GameOver else Playing
      player  = updatePlayer (dt', keys) world.player
      collisions = findCollisions world.shots world.enemies
      hitShips = List.map fst collisions
      hitShots = List.concatMap snd collisions
      enemies = world.enemies
                  |> List.filter (\s -> not (List.member s hitShips))
                  |> updateEnemies dt'
      shots   = world.shots
                  |> playerShoot player world.untilNextShot
                  |> List.filter (\s -> not (List.member s hitShots))
      untilNextShot =
        if List.length shots == List.length world.shots
          then if world.untilNextShot > 0
            then world.untilNextShot - dt
            else 0
          else shootDelay
      (updatedEnemies, untilNextWave, difficulty) =
        if world.untilNextWave > 0
          then ( enemies
               , world.untilNextWave - dt
               , world.difficulty )
          else ( enemies ++ (initEnemies world.difficulty)
               , waveDelay
               , world.difficulty + difficultyStep )
      updatedShots =
        shots
          |> updateShots dt'
          |> List.filter (\shot -> shot.pos.y < shotMaxY &&
                            shot.pos.y > shotMinY)
      score   = List.foldl (\s m -> m + s.value) world.score hitShips
      debug   = Debug.watch "World" world
  in  { world | player  <- player
              , enemies <- updatedEnemies
              , shots   <- updatedShots
              , untilNextShot <- untilNextShot
              , untilNextWave <- untilNextWave
              , difficulty <- difficulty
              , state   <- state
              , score   <- score }

-- if Mouse is pressed, go to Playing state
updateWait : Bool -> World -> World
updateWait mouseDown world =
  if not mouseDown
    then world
    else { initWorld | state <- Playing }

update : (Float, Keys, Bool) -> World -> World
update (dt, keys, mouseDown) world =
  case world.state of
    Splash   -> updateWait mouseDown world
    Playing  -> updatePlay (dt, keys) world
    GameOver -> updateWait mouseDown world

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

formatText : String -> Form
formatText s =
  Text.fromString s
    |> Text.color black
    |> Text.height 20
    |> centered
    |> toForm

splashText : Form
splashText =
  formatText "Click to start.\nArrow keys to play."

gameoverText : Int -> Form
gameoverText score =
  "Score: " ++ toString score ++ "\nClick to restart."
    |> formatText

scoreText : Int -> (Float, Float) -> Form
scoreText score (w, h) =
  "Score: " ++ toString score
    |> formatText
    |> move (80 - w/2, h/2 - 20)

render : (Int, Int) -> World -> Element
render (w, h) world =
  let (w', h') = (toFloat w, toFloat h)
      gameForms = renderGame (w', h') world
  in  case world.state of
      Splash    -> collage w h (gameForms ++ [splashText])
      Playing   -> collage w h (gameForms ++ [scoreText world.score (w', h')])
      GameOver  -> collage w h (gameForms ++ [gameoverText world.score])

-- SIGNALS
inputSignal : Signal (Float, Keys, Bool)
inputSignal =
  let delta = fps 30
      tuples = Signal.map3 (,,) delta Keyboard.arrows Mouse.isDown
  in  Signal.sampleOn delta tuples

world : Signal World
world = Signal.foldp update initWorld inputSignal

main : Signal Element
main = Signal.map2 render Window.dimensions world
