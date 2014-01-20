import Keyboard
import Random

-- TYPES

type Point = { x : Float, y : Float }
type Size = { w : Int, h : Int }

type GameState = { sprites : [Sprite],
                   timeUntilNextBomb : Float }
type Sprite = { position : Point, stype : SpriteType }
type SpriteType = { imagePath : String, size : Size, velocity : Point }

type Input = { timedelta : Float,
               timeSinceLastFrame : Float,
               arrows : { x : Int, y : Int },
               randomBombX : Float }

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
                       { sprites = [ playerSprite ],
                         timeUntilNextBomb = 0 }

playerSpeed = 8
bombSpeed = 2

timeBetweenBombs = 1000 -- ms

playerSpriteType =  { imagePath = "assets/turret.png",
                      size = { w = 56, h = 68 },
                      velocity = { x = 0, y = 0 } }

bombSpriteType =    { imagePath = "assets/bomb.png",
                      size = { w = 22, h = 44 },
                      velocity = { x = 0, y = -bombSpeed } }

-- MAIN

main = let
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

-- INPUT

inputS : Signal Input
inputS =
    let
        timeSinceLastFrameS = fps desiredFps
        arrowsS = Keyboard.arrows
        randomBombXS = 
            let
                maxBombX = canvasSize.w - bombSpriteType.size.w
            in
                -- NOTE: Using Random.float instead of Random.range because
                --       Random.float gives a runtime error.
                lift toFloat (Random.range 0 maxBombX timeSinceLastFrameS)
    in
        lift3 (\dt a rbx -> { timedelta = dt / gameSlowness,
                              timeSinceLastFrame = dt,
                              arrows = a,
                              randomBombX = rbx }) timeSinceLastFrameS arrowsS randomBombXS

-- UPDATE

updateGame : Input -> GameState -> GameState
updateGame input gameState = 
    let
        afterSpritesMoved = { gameState | sprites <- map (updateSprite input) gameState.sprites }
        afterBombsDie =
            let
                prevState = afterSpritesMoved
                spriteShouldDie s = 
                    (s.stype == bombSpriteType) && 
                    (s.position.y < 0)
                spriteShouldLive s = not (spriteShouldDie s)
            in
                { gameState | sprites <- filter spriteShouldLive prevState.sprites }
        afterBombSpawn = 
            let
                prevState = afterBombsDie
                shouldSpawnBomb = (prevState.timeUntilNextBomb <= 0)
            in
                if | shouldSpawnBomb ->
                        let
                            newBombSprite = { stype = bombSpriteType,
                                              position = { x = input.randomBombX,
                                                           y = toFloat (canvasSize.h - bombSpriteType.size.h) } }
                        in
                            { prevState | sprites <- newBombSprite :: prevState.sprites,
                                          timeUntilNextBomb <- timeBetweenBombs }
                   | otherwise ->
                        { prevState | timeUntilNextBomb <- prevState.timeUntilNextBomb - input.timeSinceLastFrame }
    in
        afterBombSpawn
        

updateSprite : Input -> Sprite -> Sprite
updateSprite input s = 
    if | s.stype == playerSpriteType ->
            let
                playerVelocity = { x = (toFloat input.arrows.x) * playerSpeed, y = 0 }
                newPosition = { x = s.position.x + (playerVelocity.x * input.timedelta),
                                y = s.position.y + (playerVelocity.y * input.timedelta) }
                maxX = toFloat (canvasSize.w - playerSpriteType.size.w)
                newPositionClamped = { x = clamp 0 maxX newPosition.x,
                                       y = newPosition.y }
            in
                { s | position <- newPositionClamped }
       | otherwise ->
            { s | position <- { x = s.position.x + (s.stype.velocity.x * input.timedelta),
                                y = s.position.y + (s.stype.velocity.y * input.timedelta) } }

-- UTILITY

div2 : Int -> Float
div2 x = (toFloat x) / 2