import Keyboard

-- TYPES

type Point = { x : Int, y : Int }
type Size = { w : Int, h : Int }

type GameState = { sprites : [Sprite] }
type Sprite = { position : Point, stype : SpriteType }
type SpriteType = { imagePath : String, size : Size, velocity : Point }

type Input = { timedelta : Int,
               arrows : Point }

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

gameSlowness = 20

playerSpriteType = { imagePath = "assets/turret.png",
                     size = { w = 68, h = 56 },
                     velocity = { x = 0, y = 0 } }

playerSpeed = 5

-- METHODS

main = let
           timedeltaS = lift (\t -> floor (t / gameSlowness)) (fps desiredFps)
           arrowsS = Keyboard.arrows
           inputS = lift2 (\t a -> { timedelta = t,
                                     arrows = a }
                          ) timedeltaS arrowsS
           gameStateS = foldp updateGame initialGameState inputS
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

updateGame : Input -> GameState -> GameState
updateGame input gameState = 
    let
        afterSpritesMoved = { sprites = map (updateSprite input) gameState.sprites }
    in
        afterSpritesMoved

updateSprite : Input -> Sprite -> Sprite
updateSprite input s = 
    if | s.stype == playerSpriteType ->
            let
                playerVelocity = { x = input.arrows.x * playerSpeed, y = 0 }
            in
                { s | position <- { x = s.position.x + (playerVelocity.x * input.timedelta),
                                    y = s.position.y + (playerVelocity.y * input.timedelta) } }
       | otherwise ->
            { s | position <- { x = s.position.x + (s.stype.velocity.x * input.timedelta),
                                y = s.position.y + (s.stype.velocity.y * input.timedelta) } }

-- UTILITY

div2 : Int -> Int
div2 x = floor ((toFloat x) / 2)