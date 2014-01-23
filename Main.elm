import Keyboard
import Random

-- TYPES

type Point = { x : Float, y : Float }
type Size = { w : Int, h : Int }

type GameState = { sprites : [Sprite],
                   timeUntilNextBomb : Float,
                   isGameOver : Bool,
                   score : Int }
-- NOTE: The 'timeToLive' field only applies to explosion-typed sprites.
--       Unfortunately the current code structure isn't well suited to
--       having extra fields for only certain sprites.
type Sprite = { position : Point, stype : SpriteType, timeToLive : Float }
type SpriteType = { imagePath : String, size : Size, velocity : Point }

type Input = { timeSinceLastFrame : Float,
               arrows : { x : Int, y : Int },
               randomBombX : Float,
               enter : Bool }

-- CONSTANTS

canvasSize : Size
canvasSize = { w = 640, h = 480 }

-- Target FPS. Browsers seem to give 25fps max.
desiredFps = 20

playerSpeed = 400 / 1000 -- px/sec
bombSpeed = 100 / 1000   -- px/sec
shotSpeed = 300 / 1000   -- px/sec

timeBetweenBombs = 1000 -- ms

immortalTimeToLive = -1
explosionInitialTimeToLive = 300 -- ms

playerSpriteType =  { imagePath = "assets/turret.png",
                      size = { w = 56, h = 68 },
                      velocity = { x = 0, y = 0 } }

bombSpriteType =    { imagePath = "assets/bomb.png",
                      size = { w = 22, h = 44 },
                      velocity = { x = 0, y = -bombSpeed } }

shotSpriteType =    { imagePath = "assets/shot.png",
                      size = { w = 12, h = 20 },
                      velocity = { x = 0, y = shotSpeed } }

explosionSpriteType =
                    { imagePath = "assets/explosion.png",
                      size = { w = 85, h = 85 },
                      velocity = { x = 0, y = 0 } }

logoSpriteType =    { imagePath = "assets/logo.png",
                      size = { w = 440, h = 84 },
                      velocity = { x = 0, y = 0 } }

initialGameState : GameState
initialGameState = 
    let
        playerSprite = { position = { x = div2 (canvasSize.w - playerSpriteType.size.w),
                                      y = 0 },
                         stype = playerSpriteType,
                         timeToLive = immortalTimeToLive }
    in
        { sprites = [ playerSprite ],
          timeUntilNextBomb = 0,
          isGameOver = True,
          score = 0 }

instructions =
    "Press Enter to begin.\n" ++
    "\n" ++
    "Arrow keys move the player.\n" ++
    "The up key fires shots.\n" ++
    "Objective: Destroy bombs.\n" ++
    ""

costToFireShot = 1
rewardToKillBomb = 10

-- MAIN

main : Signal Element
main = let
           gameStateS = foldp updateGame initialGameState inputS
       in
           lift renderGame gameStateS

-- RENDER

renderGame : GameState -> Element
renderGame gameState = 
    let
        scoreForm =
            move (0, div2 canvasSize.h - 15) (toForm
                (text
                    (Text.color white
                        (toText ("Score: " ++ (show gameState.score))))))
        
        forms =
            background :: (map render gameState.sprites) ++ [scoreForm]
        forms' = 
            if | gameState.isGameOver ->
                    let 
                        logoForm = 
                            moveY 60 (toForm
                                (image logoSpriteType.size.w
                                       logoSpriteType.size.h
                                       logoSpriteType.imagePath))
                        instructionsForm = 
                            moveY -50 (toForm
                                (centered
                                    (Text.bold
                                        (Text.color lightOrange
                                            (toText instructions)))))
                    in
                        forms ++ [logoForm, instructionsForm]
               | otherwise ->
                    forms
    in
        collage canvasSize.w canvasSize.h forms'

background : Form
background = filled blue (rect (toFloat canvasSize.w) (toFloat canvasSize.h))

render : Sprite -> Form
render s = let
               sz = s.stype.size
               pos = s.position
               offsetFromCenter = { x = pos.x - (div2 canvasSize.w) + (div2 sz.w),
                                    y = pos.y - (div2 canvasSize.h) + (div2 sz.h) }
               offsetFromCenter' = (offsetFromCenter.x, offsetFromCenter.y)
               
               opacity = if | s.timeToLive == immortalTimeToLive
                                -> 1.0
                            | otherwise
                                   -- HACK: The only type of non-immortal sprite
                                   --       is currently the explosion, so just
                                   --       hardcode its maximum TTL here.
                                -> (s.timeToLive / explosionInitialTimeToLive)
           in
               alpha opacity (move offsetFromCenter' (toForm (image sz.w sz.h s.stype.imagePath)))

-- INPUT

inputS : Signal Input
inputS =
    let
        timeSinceLastFrameS = fps desiredFps
        arrowsS = 
            let
                mergeArrows a1 a2 =
                    { x = if | a1.x /= 0 -> a1.x
                             | otherwise -> a2.x,
                      y = if | a1.y /= 0 -> a1.y
                             | otherwise -> a2.y }
            in
                lift2 mergeArrows Keyboard.arrows Keyboard.wasd
        randomBombXS = 
            let
                maxBombX = canvasSize.w - bombSpriteType.size.w
            in
                -- NOTE: Using Random.float instead of Random.range because
                --       Random.float gives a runtime error.
                lift toFloat (Random.range 0 maxBombX timeSinceLastFrameS)
        enterS = Keyboard.enter
        
        liveInputS = lift4 (\dt a rbx e -> 
            { timeSinceLastFrame = dt,
              arrows = a,
              randomBombX = rbx,
              enter = e })
            timeSinceLastFrameS arrowsS randomBombXS enterS
    in
        sampleOn timeSinceLastFrameS liveInputS

-- UPDATE

updateGame : Input -> GameState -> GameState
updateGame input lastGameState = 
    -- NOTE: Hanging paren needed to disambiguate the "otherwise" clauses
    if | lastGameState.isGameOver -> (
            if | input.enter ->
                    -- Reset game state
                    { initialGameState | isGameOver <- False }
               | otherwise ->
                    lastGameState )
       | otherwise ->
            updateRunningGame input lastGameState

updateRunningGame : Input -> GameState -> GameState
updateRunningGame input lastGameState = 
    let
        afterExplosionsAge =
            let
                prevState = lastGameState
                
                ageOrKillSprite : Sprite -> Maybe Sprite
                ageOrKillSprite s =
                    if | (s.timeToLive == immortalTimeToLive)       -> Just s
                       | (s.timeToLive >= input.timeSinceLastFrame) -> Just { s | timeToLive <- s.timeToLive - input.timeSinceLastFrame }
                       | otherwise                                  -> Nothing
            in
                { prevState | sprites <- filterJust (map ageOrKillSprite prevState.sprites) }
        afterSpritesMoved = 
            let
                prevState = afterExplosionsAge
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
                
                newExplosions = 
                    let 
                        makeExplosionIfDead s =
                            if | spriteShouldDie s && s.stype == bombSpriteType
                                           -> Just (makeExplosionForBomb s)
                               | otherwise -> Nothing
                    in
                        filterJust (map makeExplosionIfDead prevState.sprites)
                
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
                { prevState | sprites <- newExplosions ++
                                         filter spriteShouldLive prevState.sprites,
                              score <- prevState.score +
                                       (rewardToKillBomb * (length newExplosions)) }
        afterOffscreenShotsDie =
            let
                prevState = afterCollisions
                spriteShouldDie s = 
                    (s.position.y > (toFloat canvasSize.h))
                spriteShouldLive s = not (spriteShouldDie s)
            in
                { prevState | sprites <- filter spriteShouldLive prevState.sprites }
        afterBombsHitGround =
            let
                prevState = afterOffscreenShotsDie
                
                isBombOnGround s =
                    (s.stype == bombSpriteType) &&
                    (s.position.y <= 0)
                bombsThatHitGround = filter isBombOnGround prevState.sprites
            in
                if | bombsThatHitGround /= [] ->
                        let
                            deathBomb = head bombsThatHitGround
                            deathExplosion = makeExplosionForBomb deathBomb
                        in
                            { prevState | isGameOver <- True,
                                          sprites <- deathExplosion :: 
                                                     disj prevState.sprites deathBomb }
                   | otherwise -> prevState
        afterBombSpawn = 
            let
                prevState = afterBombsHitGround
                shouldSpawnBomb = (prevState.timeUntilNextBomb <= 0)
            in
                if | shouldSpawnBomb ->
                        let
                            newBombSprite = { stype = bombSpriteType,
                                              position = { x = input.randomBombX,
                                                           y = toFloat (canvasSize.h - bombSpriteType.size.h) },
                                              timeToLive = immortalTimeToLive }
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
                                shotY = player.position.y + (toFloat playerSpriteType.size.h)
                            in
                                { stype = shotSpriteType,
                                  position = { x = shotX, y = shotY },
                                  timeToLive = immortalTimeToLive }
                        in
                            { prevState | sprites <- newShotSprite :: prevState.sprites,
                                          score <- prevState.score - costToFireShot }
                   | otherwise ->
                        prevState
    in
        afterShotSpawn

-- Creates an explosion centered on the tip of the specified bomb.
makeExplosionForBomb : Sprite -> Sprite
makeExplosionForBomb s =
    { stype = explosionSpriteType,
      position = { x = s.position.x + (div2 (s.stype.size.w - explosionSpriteType.size.w)),
                   y = s.position.y - (div2 explosionSpriteType.size.h)},
      timeToLive = explosionInitialTimeToLive }

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

filterJust : [Maybe a] -> [a]
filterJust maybes =
    case maybes of
        Just x :: rest  -> x :: filterJust rest
        Nothing :: rest -> filterJust rest
        []              -> []

disj : [a] -> a -> [a]
disj list victim = filter (\x -> x /= victim) list