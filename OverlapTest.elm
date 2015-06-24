import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Util exposing (..)
import Mouse exposing (..)

tGray = rgba 0 0 0 0.2
tRed = rgba 255 0 0 0.2

staticRect : Rectangle
staticRect = { center = zeroVec2, size = { x = 100, y = 50 } }

userBaseRect : Rectangle
userBaseRect = { staticRect | size <- { x = 20, y = 20 } }

userRect : Int -> Int -> Rectangle
userRect x y =
  { userBaseRect | center <- {x = toFloat x, y = toFloat y} }

-- the beef
overlapColor : Rectangle -> Rectangle -> Color
overlapColor a b =
  if overlap a b then tRed else tGray


renderRect : Color -> Rectangle -> Form
renderRect c r = rect r.size.x r.size.y
                |> filled c
                |> move (r.center.x, r.center.y)

view : (Int, Int) -> Element
view (x, y) =
  let (w, h) = (600, 600)
      (w', h') = (toFloat w, toFloat h)
      bg = rect w h
             |> filled lightGray
      user = userRect (x - w//2) (h//2 - y)
      userForm = renderRect (overlapColor user staticRect) user
      staticForm = renderRect (overlapColor staticRect user) staticRect
  in  collage w h [ bg, staticForm, userForm ]

main =
  Signal.map view Mouse.position

