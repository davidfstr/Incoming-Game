import Keyboard

-- TYPES

type Point = { x : Float, y : Float }
type Size = { w : Int, h : Int }

type GameState = { sprites : [Sprite] }
type Sprite = { position : Point, stype : SpriteType }
type SpriteType = { imagePath : String, size : Size, velocity : Point }

type Input = { timedelta : Float,
               arrows : { x : Int, y : Int } }

-- CONSTANTS

canvasSize : Size
canvasSize = { w = 640, h = 480 }

-- Target FPS. Browsers seem to give 25fps max.
desiredFps = 20

-- Overall speed of the game. All other speed values are divided by this.
gameSlowness = 20

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

playerSpeed = 5

-- MAIN

main = let
           timedeltaS = lift (\t -> t / gameSlowness) (fps desiredFps)
           arrowsS = Keyboard.arrows
           inputS = lift2 (\t a -> { timedelta = t,
                                     arrows = a }
                          ) timedeltaS arrowsS
           gameStateS = foldp updateGame initialGameState inputS
       in
           lift renderGame gameStateS

-- RENDER

renderGame : GameState -> Element
renderGame gameState = collage canvasSize.w canvasSize.h (background :: (map render gameState.sprites))

background : Form
background = filled blue (rect (toFloat canvasSize.w) (toFloat canvasSize.h))

render : Sprite -> Form
render s = let
               sz = s.stype.size
               pos = s.position
               offsetFromCenter = { x = pos.x - (div2 canvasSize.w) + (div2 sz.w),
                                    y = pos.y - (div2 canvasSize.h) + (div2 sz.h) }
               offsetFromCenter' = (offsetFromCenter.x, offsetFromCenter.y)
           in
               move offsetFromCenter' (toForm (image sz.w sz.h s.stype.imagePath))

-- UPDATE

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
                playerVelocity = { x = (toFloat input.arrows.x) * playerSpeed, y = 0 }
            in
                { s | position <- { x = s.position.x + (playerVelocity.x * input.timedelta),
                                    y = s.position.y + (playerVelocity.y * input.timedelta) } }
       | otherwise ->
            { s | position <- { x = s.position.x + (s.stype.velocity.x * input.timedelta),
                                y = s.position.y + (s.stype.velocity.y * input.timedelta) } }

-- UTILITY

div2 : Int -> Float
div2 x = (toFloat x) / 2