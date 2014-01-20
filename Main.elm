-- TYPES

type Point = { x : Int, y : Int }
type Size = { w : Int, h : Int }

type GameState = { sprites : [Sprite] }
type Sprite = { position : Point, stype : SpriteType }
type SpriteType = { imagePath : String, size : Size, velocity : Point }

-- CONSTANTS

canvasSize : Size
canvasSize = { w = 640, h = 480 }

desiredFps = 20

background : Form
background = filled blue (rect (toFloat canvasSize.w) (toFloat canvasSize.h))

initialGameState : GameState
initialGameState = let
                       playerSprite = { position = { x = div2 (canvasSize.w - playerSpriteType.size.w),
                                                     y = 0 },
                                        stype = playerSpriteType }
                   in
                       { sprites = [ playerSprite ] }

playerSpriteType = { imagePath = "assets/turret.png",
                     size = { w = 68, h = 56 },
                     velocity = { x = 0, y = 0 } }

-- METHODS

main = let
           deltaS = lift (\t -> t / 20) (fps desiredFps)
           gameStateS = foldp updateGame initialGameState (lift floor deltaS)
       in
           lift renderGame gameStateS

renderGame : GameState -> Element
renderGame gameState = collage canvasSize.w canvasSize.h (background :: (map render gameState.sprites))

render : Sprite -> Form
render s = let
               sz = s.stype.size
               pos = s.position
               offsetFromCenter = { x = pos.x - (div2 canvasSize.w) + (div2 sz.w),
                                    y = pos.y - (div2 canvasSize.h) + (div2 sz.h) }
               offsetFromCenter' = (toFloat offsetFromCenter.x, toFloat offsetFromCenter.y)
           in
               move offsetFromCenter' (toForm (image sz.w sz.h s.stype.imagePath))

updateGame : Int -> GameState -> GameState
updateGame delta gameState = { sprites = map (update delta) gameState.sprites }

update : Int -> Sprite -> Sprite
update delta s = { s | position <- { x = s.position.x + delta,
                                     y = s.position.y } }

-- UTILITY

div2 : Int -> Int
div2 x = floor ((toFloat x) / 2)