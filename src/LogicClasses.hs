{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

module LogicClasses
  ( performCollision,
    move,
    collideWith,
  )
where

import Buttons
import Control.Concurrent (forkIO)
import GHC.IO (unsafePerformIO)
import Game
import GameConstants
import Graphics.Gloss.Data.Point.Arithmetic qualified as V
import Graphics.Gloss.Data.Vector (magV, normalizeV)
import LogicConstants
  ( gravity,
    platformSpeed,
    projectileSpeed,
    soundBrokenPlatform,
    soundFreeze,
    soundJump,
    soundJumpHight,
    soundLose,
    speedDeadPlayer,
  )
import LogicUtils (playSound)

{-    //======================\\
      || Class CollisionMaker ||
      \/======================//    -}

class CollisionMaker a b where
  performCollision :: a -> b -> (a, b)

-- | This instance of CollisionMaker is for the collision between the player and the platforms
instance CollisionMaker Player Platform where
  performCollision :: Player -> Platform -> (Player, Platform)
  -- \| This method perform collision between player and normal platforms
  performCollision player platform@Platform {platformType = Normal, platformImpulse = vy, ..} = (newPlayer, platform)
    where
      (vx, _) = playerSpeed player
      soundToPlay = if vy >= projectileSpeed then soundJumpHight else soundJump
      speed = (vx, vy)
      isFalling = playerIsFalling player
      newPlayer = unsafePerformIO $ do
        _ <- if isFalling then forkIO (playSound soundToPlay) else forkIO (return ())
        if isFalling then return player {playerSpeed = speed} else return player

  -- \| This method perform collision between player and Unstable Platforms
  performCollision player platform@Platform {platformType = Unstable x, ..} = (newPlayer, newPlatform)
    where
      newPlayer = if playerIsFalling player then fst (performCollision player (platform {platformType = Normal})) else player
      newPlatform =
        unsafePerformIO $
          if playerIsFalling player
            then do
              _ <- forkIO (playSound soundBrokenPlatform)
              return platform {platformType = Unstable (x - 1)}
            else do
              return platform

  -- \| This method perform collision between player and Alive enemies
  performCollision player platform@Platform {platformType = Enemy _ Alive, ..} = (newPlayer, platform)
    where
      isInmune = playerIsInmune player
      newPlayer =
        unsafePerformIO $
          if isInmune
            then do
              return player
            else do
              _ <- forkIO (playSound soundLose)
              return player {playerAlive = False, playerSpeed = (0, speedDeadPlayer)}

  -- \| This method perform collision between player and Freezed enemies
  performCollision player platform@Platform {platformType = Enemy _ Freezed, ..} = (newPlayer, platform)
    where
      newPlayer = fst (performCollision player (platform {platformType = Normal}))
  performCollision player platform@Platform {platformType = EndPlatform, ..} = (newPlayer, platform)
    where
      newPlayer = player {playerSpeed = (0, 0)}

-- | This instance of CollisionMaker is for the collision between projectiles and the platforms
instance CollisionMaker Projectile Platform where
  performCollision :: Projectile -> Platform -> (Projectile, Platform)
  -- \| This method perform collision between projectiles and alive enemies
  performCollision projectile platform@Platform {platformType = Enemy enemyType Alive, ..} = (newProjectile, newPlatform)
    where
      newPlatform = unsafePerformIO $ do
        _ <- forkIO (playSound soundFreeze)
        return platform {platformType = Enemy enemyType Freezed}
      newProjectile = projectile {projectilePosition = (windowWidth / 2 + projectileRadius, -windowHeight)}
  -- \| This method perform collision with all platforms, except Alive enemys
  performCollision projectile platform = (projectile, platform)

instance CollisionMaker Button Point where
  performCollision :: Button -> Point -> (Button, Point)
  performCollision button point = (newButton, point)
    where
      currentState = buttonActive button
      newButton = button {buttonActive = not currentState}

{-    //========================\\
      || Class CollisionChecker ||
      \/========================//    -}

class CollisionChecker a b where
  collideWith :: a -> b -> Bool

-- | This instance of CollisionChecker is for checking collisions between the player and the platforms
instance CollisionChecker Player Platform where
  collideWith :: Player -> Platform -> Bool
  collideWith player platform@Platform {platformType = Enemy _ Alive, ..} = any (isPointInto (lefUpperCorner, rightBottomCorner)) playerPoints
    where
      playerPoints = extractPlayerPoints player
      (lefUpperCorner, rightBottomCorner) = extractPlatformPoints platform
  collideWith player platform = any (isPointInto (lefUpperCorner, rightBottomCorner)) playerPoints
    where
      playerPoints = take 2 (extractPlayerPoints player)
      (lefUpperCorner, rightBottomCorner) = extractPlatformPoints platform

-- | This instance of CollisionChecker is for checking collisions between projectiles and platforms
instance CollisionChecker Projectile Platform where
  collideWith :: Projectile -> Platform -> Bool
  collideWith projectile platform = any (isPointInto (lefUpperCorner, rightBottomCorner)) projectilePoints
    where
      (lefUpperCorner, rightBottomCorner) = extractPlatformPoints platform
      centerPoint = projectilePosition projectile
      radiusList = [(projectileRadius, 0), (-projectileRadius, 0), (0, projectileRadius), (0, -projectileRadius)]
      projectilePoints = map (V.+ centerPoint) radiusList

instance CollisionChecker Button Point where
  collideWith :: Button -> Point -> Bool
  collideWith button = isPointInto (leftUpperCorner, rightBottomCorner)
    where
      (leftUpperCorner, rightBottomCorner) = extreactButtonCorners button

{-    //===================\\
      || Class HasMovement ||
      \/===================//    -}

class HasMovement object where
  move :: Float -> object -> object

-- | Instance of HasMovement to define the movement of the platforms
instance HasMovement Platform where
  move :: Float -> Platform -> Platform
  move _ platform@Platform {platformType = Enemy _ Freezed, ..} = platform
  move dt p = p {platformPosition = newPosition, platformDirection = newDirection}
    where
      pos = platformPosition p
      start = platformStartMovement p
      end = platformEndMovement p
      moveVector = end V.- start
      moveLength = magV moveVector
      -- Calculates the distance between the current position of the platform and the End or Start point
      distanceToEnd = magV (end V.- pos)
      distanceToStart = magV (start V.- pos)
      -- Determines which direction the platform should move
      newDirection
        | platformDirection p == 1 && distanceToEnd <= platformSpeed * dt = -1 -- the platform has reached the End point, it changes direction
        | platformDirection p == -1 && distanceToStart <= platformSpeed * dt = 1 -- the platform has reached the Start point, it changes direction
        | otherwise = platformDirection p
      -- calculates the new position of the platform
      newPosition
        | newDirection == 1 = pos V.+ (platformSpeed V.* (dt V.* moveVector'))
        | otherwise = pos V.- (platformSpeed V.* (dt V.* moveVector'))
      -- Normalizes the motion vector to keep the velocity constant
      moveVector'
        | moveLength > 0 = normalizeV moveVector
        | otherwise = (0, 0)

-- | Instance for HasMovement to define the movement of the Player
instance HasMovement Player where
  move :: Float -> Player -> Player
  move dt player@(Player pPos pSpeed _ _ _) = player {playerPosition = newPosition, playerSpeed = newSpeed, playerIsFalling = vy < 0}
    where
      newSpeed@(_, vy) = pSpeed V.+ (dt V.* (0, gravity))
      notCircularPosition@(newX, y) = pPos V.+ (dt V.* newSpeed)
      newPosition
        | newX >= windowWidth / 2 + playerWidth / 2 = (-windowWidth / 2 - playerWidth / 2, y)
        | newX <= -windowWidth / 2 - playerWidth / 2 = (windowWidth / 2 + playerWidth / 2, y)
        | otherwise = notCircularPosition

-- | Instance of HasMovement to define projectiles movement
instance HasMovement Projectile where
  move :: Float -> Projectile -> Projectile
  move dt projectile@(Projectile _ pPos pMovement) = projectile {projectilePosition = newPosition}
    where
      newPosition = pPos V.+ (projectileSpeed V.* (dt V.* pMovement))

{-    //=======\\
      || Utils ||
      \/=======//    -}
-- \| Check if a point is within a certain interval
isPointInto :: (Point, Point) -> Point -> Bool
isPointInto (leftCornerU, rightCornerB) point = px >= lcux && py <= lcuy && px <= rcbx && py >= rcby
  where
    (px, py) = point
    (lcux, lcuy) = leftCornerU
    (rcbx, rcby) = rightCornerB

extreactButtonCorners :: Button -> (Point, Point)
extreactButtonCorners button = (leftUpperCorner, rightBottomCorner)
  where
    (buttonWidth, buttonHeight) = buttonSize button
    rectanglePoints = map (V.+ buttonPosition button) [(-buttonWidth / 2, buttonHeight / 2), (buttonWidth / 2, -buttonHeight / 2)]
    (leftUpperCorner, rightBottomCorner) = (head rectanglePoints, last rectanglePoints)

extractPlayerPoints :: Player -> [Point]
extractPlayerPoints player = playerPoints
  where
    leftX = (-playerWidth) / 2
    rightX = leftX * (-1)
    topY = playerHeight / 2
    bottomY = topY * (-1)
    points = [(rightX, bottomY), (leftX, bottomY), (leftX, topY), (rightX, topY), (0, 0)]
    playerPoints = map (V.+ playerPosition player) points

extractPlatformPoints :: Platform -> (Point, Point)
extractPlatformPoints platform = (lefUpperCorner, rightBottomCorner)
  where
    (currentPlatformWidth, currentPlatformHight) = platformSize platform
    rectanglePoints = map (V.+ platformPosition platform) [(-currentPlatformWidth / 2, currentPlatformHight / 2), (currentPlatformWidth / 2, -currentPlatformHight / 2)]
    (lefUpperCorner, rightBottomCorner) = (head rectanglePoints, last rectanglePoints)
