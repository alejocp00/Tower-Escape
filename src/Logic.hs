{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

module Logic
  ( poblate,
    movePlatforms,
    detectCollisions,
    transformGame,
    performGame,
    initialGame,
    getButtonState,
  )
where

import Buttons
import ButtonsConstants
import Control.Concurrent
import Data.Yaml
import GHC.IO (unsafePerformIO)
import Game
  ( EnemyState (..),
    Game (..),
    Platform (..),
    PlatformType (..),
    Player (..),
    Projectile (..),
    State (GameOver, Idle, Playing),
  )
import GameConstants
import Generator
  ( generatePlatforms,
    generatePlayer,
    getInitialSeed,
  )
import Graphics.Gloss.Data.Point.Arithmetic qualified as V
import Graphics.Gloss.Data.Vector (normalizeV)
import Graphics.Gloss.Interface.Pure.Game
  ( Event (EventKey),
    Key (Char, MouseButton, SpecialKey),
    KeyState (Down, Up),
    MouseButton (LeftButton),
    SpecialKey (KeySpace),
  )
import LogicClasses
import LogicConstants
import LogicUtils (playSound)

{-    //=========================\\
      || Generate and Purge zone ||
      \/=========================//    -}

-- | Generate or purge platforms in the game
poblate :: Game -> Game
poblate game = case gameState game of
  Idle -> generatePlatforms (generatePlayer game)
  Playing -> generatePlatforms (purgePlatforms game)
  _ -> game

-- | Remove those platforms that are not longer in the game and update the platforms count
purgePlatforms :: Game -> Game
purgePlatforms game = game {gamePlatforms = platforms, gamePlatformsCount = currentPlatformCount}
  where
    platforms = filter inPlayPlatform (gamePlatforms game)
    currentPlatformCount = length platforms
    inPlayPlatform platform =
      (y >= endScreenBelow game && unbrokenUnstable platform && gameState game /= GameOver)
        || pType == EndPlatform
      where
        y = snd (platformEndMovement platform) + platformHeight / 2
        pType = platformType platform
        unbrokenUnstable p = case platformType p of
          Unstable x -> x > 0
          _ -> True

-- | Remove projectiles that are not between the leftEnd and rightEnd
purgeProjectiles :: Game -> Game
purgeProjectiles game = game {gameProjectiles = newProjectiles}
  where
    newProjectiles = filter projectileTraveling (gameProjectiles game)
    projectileTraveling projectile = x > leftEnd && x < rightEnd
      where
        x = fst $ projectilePosition projectile
        rightEnd = (windowWidth / 2) + projectileRadius
        leftEnd = rightEnd * (-1)

{-    //===============\\
      || Moved related ||
      \/===============//    -}

-- | Manage player movement
movePlayer :: Float -> Game -> Game
movePlayer dt game = game {gamePlayer = movedPlayer {playerIsInmune = newInmuneState}}
  where
    state = gameState game
    increment = 3
    (moveLeft, moveRight) = gamePlayerAxisX game
    currentPlayerSpeed@(_, vy) = playerSpeed (gamePlayer game)
    lastPlatform
      | state == GameOver = last (gamePlatforms game)
      | otherwise = head (gamePlatforms game)
    player = gamePlayer game
    speedLeft
      | moveLeft && state == Playing = increment V.* (dt V.* (-playerSpeedX, 0))
      | otherwise = (0, 0)
    speedRight
      | moveRight && state == Playing = increment V.* (dt V.* (playerSpeedX, 0))
      | otherwise = (0, 0)
    movedPlayer
      | isOnLastPlatform player game =
          let newPosition = (fst (playerPosition player), snd (platformPosition lastPlatform))
           in player {playerPosition = newPosition}
      | otherwise = move dt (gamePlayer game) {playerSpeed = finalSpeed $ currentPlayerSpeed V.+ speedLeft V.+ speedRight}
    finalSpeed :: (Float, Float) -> (Float, Float)
    finalSpeed (sx, sy) = (fsx, sy)
      where
        fsx
          | sx > 1000 = 1000
          | sx < -1000 = -1000
          | otherwise = sx
    newInmuneState = vy >= playerInmuneSpeed

-- | Checks if the player in on the final platform
isOnLastPlatform :: Player -> Game -> Bool
isOnLastPlatform player game = playerY <= endPlatformY
  where
    playerY = snd (playerPosition player) + (playerHeight / 2)
    platform = last (gamePlatforms game)
    endPlatformY = snd (platformPosition platform) + snd (platformSize platform) / 2

-- | Manage projectiles movements
moveProjectiles :: Float -> Game -> Game
moveProjectiles dt game = game {gameProjectiles = newProjectiles}
  where
    newProjectiles = map (move dt) (gameProjectiles game)

-- | Manage platforms movements
movePlatforms :: Float -> Game -> Game
movePlatforms dt game = game {gamePlatforms = newPlatforms}
  where
    newPlatforms = map (move dt) (gamePlatforms game)

detectFalling :: Game -> Game
detectFalling game = game {gameState = newState}
  where
    player = gamePlayer game
    newState = unsafePerformIO $ do
      if snd (playerPosition player) < endScreenBelow game
        then do
          _ <- forkIO (playSound soundLose)
          return GameOver
        else do return Playing

{-    //====================\\
      || Collisions zones   ||
      \/====================//    -}

-- | See if there was a collision
detectCollisions :: Game -> Game
detectCollisions game
  | state == Playing && maybePlatform /= [] =
      let (newPlayer, newPlatform) = performCollision player (head maybePlatform)
          newPlatforms = map (\p -> if p == newPlatform then newPlatform else p) platforms
          newState
            | playerAlive newPlayer = Playing
            | otherwise = GameOver
       in game {gamePlatforms = newPlatforms, gamePlayer = newPlayer, gameState = newState}
  | state == GameOver && maybePlatform /= [] = case platformType $ head maybePlatform of
      EndPlatform -> game {gamePlayer = fst $ performCollision (gamePlayer game) (head maybePlatform)}
      _ -> game
  | otherwise = game
  where
    state = gameState game
    maybePlatform = getCollision game
    player = gamePlayer game
    platforms = gamePlatforms game

-- Detect if a projectile makes collision with an enemy
projectilesCollision :: Game -> Game
projectilesCollision game = game {gameProjectiles = newProjectiles, gamePlatforms = newPlatforms}
  where
    projectiles = gameProjectiles game
    platforms = gamePlatforms game
    pairs = getProjectileCollision projectiles platforms
    projectilesColliding = map fst pairs
    freezedEnemys = map snd pairs
    newProjectiles = map (\p -> if p `elem` projectilesColliding then p {projectilePosition = ((windowWidth / 2) + projectileRadius, -windowHeight)} else p) projectiles
    newPlatforms = map (\p -> if p `elem` freezedEnemys then p {platformType = Enemy (enemyType p) Freezed} else p) platforms
    enemyType plat = case platformType plat of
      Enemy eType _ -> eType
      _ -> error "This platform is not an enemy"

getProjectileCollision :: [Projectile] -> [Platform] -> [(Projectile, Platform)]
getProjectileCollision projectiles platforms = [performCollision p pl | p <- projectiles, pl <- platforms, collideWith p pl, isEnemy pl]
  where
    isEnemy platform = case platformType platform of
      Enemy _ _ -> True
      _ -> False

-- | Gets the the platforms colliding with the player
getCollision :: Game -> [Platform]
getCollision game = getPlatformCollision (gamePlatforms game) (gamePlayer game)

getPlatformCollision :: [Platform] -> Player -> [Platform]
getPlatformCollision platforms player = [platform | platform <- platforms, collideWith player platform]

{-    //=================\\
      || Updates related ||
      \/=================//    -}

-- | Update camera position
updateCamera :: Game -> Game
updateCamera game = game {gameCameraPos = newCamera, gamePlatforms = map newEndPlatform (gamePlatforms game)}
  where
    (_, currentCameraY) = gameCameraPos game
    (_, py) = playerPosition (gamePlayer game)
    y
      | gameState game == GameOver = py + 150
      | otherwise = max py currentCameraY
    newCamera = (0, y)
    newEndPlatform p = case gameState game of
      Playing -> if platformId p == 0 then p {platformPosition = (0, y) V.- (0, windowHeight + platformHeight)} else p
      _ -> p

-- | Update score
updateScore :: Game -> Game
updateScore game = game {gameScore = max player_y score}
  where
    player = gamePlayer game
    player_y = round (snd (playerPosition player))
    score = gameScore game

-- Update final score if the new one is bigger than the registered
updateFinalScore :: Game -> Game
updateFinalScore game = game {gameBestScores = newScores}
  where
    actualScore = gameScore game
    scores = gameBestScores game
    newScores
      | actualScore `notElem` scores = insertaOrd actualScore scores
      | otherwise = scores

-- | Save the best scores in a yaml file to be read later
saveBestScores :: Game -> IO Bool
saveBestScores game = do
  let scores = gameBestScores game
  encodeFile saveFilePath scores
  return True

-- | This method load the scores. If it fails, return an empty list. Use decodeFileEither
loadScores :: IO [Int]
loadScores = do
  scores <- decodeFileEither saveFilePath :: IO (Either ParseException [Int])
  case scores of
    Left _ -> return []
    Right s -> return s

{-    //====================\\
      || Logic loop perform ||
      \/====================//    -}

performGame :: Float -> Game -> Game
performGame seconds game
  -- In Idle state, the method will only move the platforms
  | state == Idle = movePlatforms seconds $ poblate newGame
  -- In Playing state, the method will move platforms and the y axis of the player. Also, the collisions will be made here
  | state == Playing =
      updateCamera $
        purgeProjectiles $
          updateScore $
            detectCollisions $
              movePlatforms seconds $
                detectFalling $
                  movePlayer seconds $
                    projectilesCollision $
                      moveProjectiles seconds $
                        poblate game
  -- In GameOver state, the method will only move the platforms
  | state == GameOver =
      updateFinalScore $
        updateCamera $
          detectCollisions $
            movePlayer seconds game
  | otherwise = game
  where
    state = gameState game
    regenPlatforms = getButtonState (gameButtons game) RegeneratePlatforms
    newButtons = map (\b -> if buttonType b == RegeneratePlatforms then b {buttonActive = False} else b) (gameButtons game)
    newGame
      | gameNeedPlatforms game || regenPlatforms = (emptyGame newButtons (gameBestScores game) (gameSeed game)) {gameNeedPlatforms = False}
      | otherwise = game

{-    //======================\\
      || Input handle related ||
      \/======================//    -}

-- Function to transform the game according to the event
transformGame :: Event -> Game -> Game
transformGame (EventKey (SpecialKey KeySpace) Down _ _) game = case gameState game of
  Idle -> game {gameState = Playing}
  Playing -> game
  GameOver -> if saveState then game {gameState = Idle, gameNeedPlatforms = True} else game
    where
      saveState = unsafePerformIO $ saveBestScores game
transformGame (EventKey (Char 'a') Down _ _) game = case state of
  Playing -> game {gamePlayerAxisX = newAxisState}
  _ -> game
  where
    state = gameState game
    (_, axisXRight) = gamePlayerAxisX game
    newAxisState = (True, axisXRight)
transformGame (EventKey (Char 'a') Up _ _) game = case state of
  Playing -> game {gamePlayerAxisX = newAxisState}
  _ -> game
  where
    state = gameState game
    (_, axisXRight) = gamePlayerAxisX game
    newAxisState = (False, axisXRight)
transformGame (EventKey (Char 'd') Down _ _) game = case state of
  Playing -> game {gamePlayerAxisX = newAxisState}
  _ -> game
  where
    state = gameState game
    (axisXLeft, _) = gamePlayerAxisX game
    newAxisState = (axisXLeft, True)
transformGame (EventKey (Char 'd') Up _ _) game = case state of
  Playing -> game {gamePlayerAxisX = newAxisState}
  _ -> game
  where
    state = gameState game
    (axisXLeft, _) = gamePlayerAxisX game
    newAxisState = (axisXLeft, False)

-- When the player clicks, we create a new projectile
transformGame (EventKey (MouseButton LeftButton) Down _ mousePosition@(x, y_in)) game = newGame
  where
    state = gameState game
    y = y_in + snd (gameCameraPos game)
    pos@(xp, yp) = playerPosition $ gamePlayer game
    vector = (x - xp, y - yp)
    direction = normalizeV vector
    -- soundPlayed = unsafePerformIO $ playAnySound soundClick
    projectiles = gameProjectiles game
    newProjectiles = Projectile (getProjectileID projectiles) pos direction : projectiles
    getProjectileID [] = 0
    getProjectileID (p : _) = projectileId p + 1
    newButtons = map (\b -> if collideWith b mousePosition then fst $ performCollision b mousePosition else b) (gameButtons game)
    newGame = unsafePerformIO $ do
      if state == Playing
        then do
          _ <- forkIO (playSound soundShoot)
          return game {gameProjectiles = newProjectiles}
        else do
          _ <- if any (`collideWith` mousePosition) newButtons then do forkIO (playSound soundClick) else do forkIO (return ())
          return game {gameButtons = newButtons}

-- Sudo Options :)

-- When r is pressed, player will teleport to (0,0)
transformGame (EventKey (Char 'r') Down _ _) game
  | debugActions = game {gamePlayer = player {playerPosition = (0, 100)}}
  | otherwise = game
  where
    debugActions = getButtonState (gameButtons game) ActiveDebugActions
    player = gamePlayer game

-- When c is pressed, camera will move to the player position
transformGame (EventKey (Char 'c') Down _ _) game
  | debugActions = game {gameCameraPos = newCameraPos}
  | otherwise = game
  where
    debugActions = getButtonState (gameButtons game) ActiveDebugActions
    player = gamePlayer game
    pos = playerPosition player
    (x, _) = gameCameraPos game
    newCameraPos = (x, snd pos)
transformGame (EventKey (Char 'p') Down _ _) game
  | debugActions = game {gameProjectiles = []}
  | otherwise = game
  where
    debugActions = getButtonState (gameButtons game) ActiveDebugActions
transformGame (EventKey (Char 's') Down _ _) game
  | debugActions = game {gamePlayer = player {playerSpeed = newSpeed}}
  | otherwise = game
  where
    debugActions = getButtonState (gameButtons game) ActiveDebugActions
    player = gamePlayer game
    (vx, vy) = playerSpeed player
    newSpeed = if vy > 0 then (vx, 0) else (vx, speedNormal)

-- \| Base case
transformGame _ game = game

{-    //=================\\
      || Buttons related ||
      \/=================//    -}

getButtonState :: [Button] -> ButtonType -> Bool
getButtonState buttons buttonTypeObjective = case button of
  [] -> False
  (b : _) -> buttonActive b
  where
    button = filter (\b -> buttonTypeObjective == buttonType b) buttons

{-    //====================\\
      || Initial game state ||
      \/====================//    -}

initialGame :: Game
initialGame = poblate $ emptyGame initButtonsPositions (insertaOrd 0 $ unsafePerformIO loadScores) getInitialSeed

{-    //=======\\
      || Utils ||
      \/=======//    -}

insertaOrd :: Ord t => t -> [t] -> [t]
insertaOrd x [] = [x]
insertaOrd x (y : ys)
  | x >= y = x : y : ys
  | otherwise = y : insertaOrd x ys