import Keyboard
import Random

-- TYPES

type Point = { x : Float, y : Float }
type Size = { w : Int, h : Int }

type GameState = { sprites : [Sprite],
                   timeUntilNextBomb : Float }
type Sprite = { position : Point, stype : SpriteType }
type SpriteType = { imagePath : String, size : Size, velocity : Point }

type Input = { timeSinceLastFrame : Float,
               arrows : { x : Int, y : Int },
               randomBombX : Float }

-- CONSTANTS

canvasSize : Size
canvasSize = { w = 640, h = 480 }

-- Target FPS. Browsers seem to give 25fps max.
desiredFps = 20

initialGameState : GameState
initialGameState = let
                       playerSprite = { position = { x = div2 (canvasSize.w - playerSpriteType.size.w),
                                                     y = 0 },
                                        stype = playerSpriteType }
                   in
                       { sprites = [ playerSprite ],
                         timeUntilNextBomb = 0 }

playerSpeed = 400 / 1000 -- px/sec
bombSpeed = 100 / 1000   -- px/sec
shotSpeed = 300 / 1000   -- px/sec

timeBetweenBombs = 1000 -- ms

playerSpriteType =  { imagePath = "assets/turret.png",
                      size = { w = 56, h = 68 },
                      velocity = { x = 0, y = 0 } }

bombSpriteType =    { imagePath = "assets/bomb.png",
                      size = { w = 22, h = 44 },
                      velocity = { x = 0, y = -bombSpeed } }

shotSpriteType =    { imagePath = "assets/shot.png",
                      size = { w = 12, h = 20 },
                      velocity = { x = 0, y = shotSpeed } }

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
        arrowsS = Keyboard.wasd
        randomBombXS = 
            let
                maxBombX = canvasSize.w - bombSpriteType.size.w
            in
                -- NOTE: Using Random.float instead of Random.range because
                --       Random.float gives a runtime error.
                lift toFloat (Random.range 0 maxBombX timeSinceLastFrameS)
    in
        lift3 (\dt a rbx -> { timeSinceLastFrame = dt,
                              arrows = a,
                              randomBombX = rbx }) timeSinceLastFrameS arrowsS randomBombXS

-- UPDATE

updateGame : Input -> GameState -> GameState
updateGame input lastGameState = 
    let
        afterSpritesMoved = 
            let
                prevState = lastGameState
            in
                { prevState | sprites <- map (updateSprite input) prevState.sprites }
        afterCollisions =
            let
                prevState = afterSpritesMoved
                
                spriteShouldDie s1 = 
                    any (\s2 -> spritesCollide s1 s2 &&
                                spritesHaveTypes shotSpriteType bombSpriteType s1 s2
                        ) prevState.sprites
                spriteShouldLive s = not (spriteShouldDie s)
                
                spritesHaveTypes t1 t2 s1 s2 =
                    (s1.stype == t1 && s2.stype == t2) ||
                    (s1.stype == t2 && s2.stype == t1)
                
                spritesCollide s1 s2 = 
                    rectsCollide (spriteRect s1) (spriteRect s2)
                
                spriteRect s =
                    { x1 = s.position.x,
                      y1 = s.position.y,
                      x2 = s.position.x + (toFloat s.stype.size.w),
                      y2 = s.position.y + (toFloat s.stype.size.h) }
                
                rectsCollide r1 r2 =
                    rectIsValid (rectIntersection r1 r2)
                
                rectIntersection r1 r2 =
                    { x1 = max r1.x1 r2.x1,
                      x2 = min r1.x2 r2.x2,
                      y1 = max r1.y1 r2.y1,
                      y2 = min r1.y2 r2.y2 }
                
                rectIsValid r =
                    (r.x1 <= r.x2) && (r.y1 <= r.y2)
            in
                { prevState | sprites <- filter spriteShouldLive prevState.sprites }
        afterOffscreenSpritesDie =
            let
                prevState = afterCollisions
                spriteShouldDie s = 
                    (s.position.y < 0) || (s.position.y > (toFloat canvasSize.h))
                spriteShouldLive s = not (spriteShouldDie s)
            in
                { prevState | sprites <- filter spriteShouldLive prevState.sprites }
        afterBombSpawn = 
            let
                prevState = afterOffscreenSpritesDie
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
        afterShotSpawn =
            let
                prevState = afterBombSpawn
                shouldSpawnShot = (input.arrows.y == 1)
            in
                if | shouldSpawnShot ->
                        let newShotSprite =
                            let
                                findPlayer gameState =
                                    head (filter (\s -> s.stype == playerSpriteType) gameState.sprites)
                                player = findPlayer prevState
                                shotX = player.position.x + (div2 (playerSpriteType.size.w - shotSpriteType.size.w))
                                shotY = player.position.y
                            in
                                { stype = shotSpriteType, position = { x = shotX, y = shotY } }
                        in
                            { prevState | sprites <- newShotSprite :: prevState.sprites }
                   | otherwise ->
                        prevState
    in
        afterShotSpawn
        

updateSprite : Input -> Sprite -> Sprite
updateSprite input s = 
    if | s.stype == playerSpriteType ->
            let
                playerVelocity = { x = (toFloat input.arrows.x) * playerSpeed, y = 0 }
                newPosition = { x = s.position.x + (playerVelocity.x * input.timeSinceLastFrame),
                                y = s.position.y + (playerVelocity.y * input.timeSinceLastFrame) }
                maxX = toFloat (canvasSize.w - playerSpriteType.size.w)
                newPositionClamped = { x = clamp 0 maxX newPosition.x,
                                       y = newPosition.y }
            in
                { s | position <- newPositionClamped }
       | otherwise ->
            { s | position <- { x = s.position.x + (s.stype.velocity.x * input.timeSinceLastFrame),
                                y = s.position.y + (s.stype.velocity.y * input.timeSinceLastFrame) } }

-- UTILITY

div2 : Int -> Float
div2 x = (toFloat x) / 2