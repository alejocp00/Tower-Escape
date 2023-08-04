module Generator (generatePlatforms, generatePlayer, getInitialSeed) where

import Control.Monad.ST (runST)
import Game
  ( EnemyState (..),
    EnemyType (..),
    Game
      ( gameCameraPos,
        gamePlatforms,
        gamePlatformsCount,
        gamePlayer,
        gameSeed
      ),
    Platform (..),
    PlatformType (Enemy, Normal, Unstable),
    Point,
  )
import GameConstants
import GeneratorConstants
import LogicConstants
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)
import System.Random.PCG as PCG (Variate (uniformR), initialize)

{-    //=======================\\
      || Probability functions ||
      \/=======================//    -}

-- | This is a probability function that starts with a high probability and decreases to a low probability
probHighToLow :: Float -> Int
probHighToLow x = truncate ((p / (p + 0.3)) * 100)
  where
    p = 0.7 * exp (-0.5 * x)

-- | This is a probability function that starts with a low probability and increases to a high probability
probLowToHigth :: Float -> Int
probLowToHigth x = truncate ((p / (p + 0.4)) * 100)
  where
    p = 0.1 + ((0.4 * x) / (x + 5))

-- | This function generates the player
generatePlayer :: Game -> Game
generatePlayer game = game {gamePlayer = initialPlayer}

{-    //=================\\
      || Generation zone ||
      \/=================//    -}

-- | This functions generates the platforms for the game.
generatePlatforms :: Game -> Game
generatePlatforms game
  | canGenerate game = generatePlatforms newGameState
  | otherwise = game
  where
    currentHeight = snd (gameCameraPos game)
    platforms = gamePlatforms game
    (newPlatforms, newSeed) = generateOnePlatform currentHeight (gameSeed game) platforms
    newGameState = game {gamePlatforms = newPlatforms, gamePlatformsCount = gamePlatformsCount game + 1, gameSeed = newSeed}

-- | Checks if it can generate platforms
canGenerate :: Game -> Bool
canGenerate game = endScreenUp game >= lastPlatformY platforms
  where
    platforms = gamePlatforms game
    lastPlatformY [] = 0
    lastPlatformY (p : _) = snd (platformPosition p)

-- | Generate a single platform
generateOnePlatform :: Float -> Int -> [Platform] -> ([Platform], Int)
generateOnePlatform _ seed [] = (initialPlatform : [endPlatform], seed)
generateOnePlatform currentHeight seed allPlatforms@(lastPlatform : _) = (newPlatform : allPlatforms, seed5)
  where
    (lpX, lpY) = platformPosition lastPlatform
    (newType, seed1) = getType currentHeight seed
    (w, h) =
      case newType of
        Enemy FlyingSlime _ -> (flayingSlimeWidth, flayingSlimeHeight)
        _ -> (platformWidth, platformHeight)
    (newX, seed2) = getX lpX seed1
    (newY, seed3) = getY h lpY seed2
    (newImpulse, seed4) = getEffect currentHeight seed3
    ((moveStart, moveEnd), seed5) = getMovement (newX, newY) (w, h) seed4

    newPlatform = Platform {platformId = platformId lastPlatform + 1, platformPosition = (newX, newY), platformType = newType, platformImpulse = newImpulse, platformEndMovement = moveEnd, platformStartMovement = moveStart, platformSize = (w, h), platformDirection = 1}

{-    //========================\\
      || Get random values zone ||
      \/========================//    -}

-- | Gets in a "random" way movements for the platforms
getMovement :: Point -> Point -> Int -> ((Point, Point), Int)
getMovement (x, y) (w, h) seed = getRandom4 y seed3 static horizontal vertical diagonal
  where
    (verticalY, seed1) = getY h y seed
    (diagonalX, seed2) = getX x seed1
    (diagonalY, seed3) = getY h y seed2
    static = ((x, y), (x, y))
    horizontal = (((-windowWidth / 2) + (w / 2), y), ((windowWidth / 2) - (w / 2), y))
    vertical = ((x, y), (x, verticalY))
    diagonal = ((x, y), (diagonalX, diagonalY))

-- | This function generate the y position of the new platform according to the last platform
getY :: Float -> Float -> Int -> (Float, Int)
getY h y seed
  | randomNumber <= i1 = result (scale (0, i1) (shortInterval h y) randomNumber)
  | i1 < randomNumber && randomNumber <= i2 = result (scale (i1 + 1, i2) (mediumInterval h y) randomNumber)
  | i2 < randomNumber && randomNumber <= i3 = result (scale (i2 + 1, i3) (longInterval h y) randomNumber)
  | otherwise = getY h y newSeed
  where
    randomNumber = generateRandomInt (0, 100) seed
    newSeed = seed + 1
    i1 = probLowToHigth y
    i2 = probHighToLow y + i1
    i3 = 100
    result value = (value, newSeed)

-- | This function generates the x position of the new platform according to the last platform
getX :: Float -> Int -> (Float, Int)
getX _ seed = (fromIntegral (generateRandomInt (truncate final, truncate initial) seed), seed + 1)
  where
    initial = -windowWidth / 2 + platformWidth / 2
    final = windowWidth / 2 - platformWidth / 2

-- | get the type of the platforms
getType :: Float -> Int -> (PlatformType, Int)
getType y seed = getRandom3 y seed firstType (Unstable unstableCount) Normal
  where
    unstableCount = generateRandomInt (1, 3) (seed + 1)
    enemyType = case generateRandomInt (1, enemyTypes) (seed + 1) of
      1 -> FlyingSlime
      _ -> Default
    firstType
      | y > enemySpawnHight = Enemy enemyType Alive
      | otherwise = case generateRandomInt (1, 2) (seed + 1) of
          1 -> Unstable unstableCount
          _ -> Normal

-- | Gets the effect that may have a platform
getEffect :: Float -> Int -> (Float, Int)
getEffect y seed = getRandom4 y seed speedNormal speedUltraFast speedFast speedFaster

-- | This function recibe a float and three objects of type a, and return one of them
-- - The first object is returned with a probability of probLowToHigthFunc
-- - The second object is returned with a probability of probHighMidFunc
-- - The third object is returned with a probability of 0% to 100%
getRandom3 :: Float -> Int -> a -> a -> a -> (a, Int)
getRandom3 y seed item1 item2 item3
  | randomNumber <= i1 = result item1
  | i1 < randomNumber && randomNumber <= i2 = result item2
  | i2 < randomNumber && randomNumber <= i3 = result item3
  | otherwise = getRandom3 y newSeed item1 item2 item3
  where
    randomNumber = generateRandomInt (0, 100) seed
    i1 = probLowToHigth y
    i2 = i1 + probHighToLow y
    i3 = 100
    newSeed = seed + 1
    result item = (item, newSeed)

-- | This function recibe a float and 4 objects of type a, and return one of them
-- - The first object is returned with a probability of probLowToHight
-- - The second object is returned with a probability of probHighToLow
-- - The third and fourth object is returned with a probability of (100 - (probLowToHight + probHighToLow))/2
getRandom4 :: Float -> Int -> a -> a -> a -> a -> (a, Int)
getRandom4 x seed item1 item2 item3 item4
  | randomNumber <= i1 = result item1
  | i1 < randomNumber && randomNumber <= i2 = result item2
  | i2 < randomNumber && randomNumber <= i3 = result item3
  | i3 < randomNumber && randomNumber <= i4 = result item4
  | otherwise = getRandom4 x newSeed item1 item2 item3 item4
  where
    randomNumber = generateRandomInt (0, 100) seed
    i1 = probLowToHigth x
    i2 = i1 + probHighToLow x
    i3 = ((100 - i2) `div` 2) + i2
    i4 = 100
    newSeed = seed + 1
    result item = (item, newSeed)

{-    //=============================\\
      || Seed and random Int related ||
      \/=============================//    -}

generateRandomInt :: (Int, Int) -> Int -> Int
generateRandomInt (s, e) seed_in = runST $ do
  g <- PCG.initialize (fromIntegral seed_in) 0
  PCG.uniformR (s, e) g

-- | Generate a random Int.
getInitialSeed :: Int
{-# NOINLINE getInitialSeed #-}
getInitialSeed = unsafePerformIO randomIO

{-    //=======\\
      || Utils ||
      \/=======//    -}

-- | The function scale, recibe a interval where the number r was generated, and return the corresponding number in the second interval
scale :: (Int, Int) -> (Int, Int) -> Int -> Float
scale (a, b) (c, d) n = (x - i1) * (e2 - i2) / (e1 - i1) + i2
  where
    i1 = fromIntegral a
    e1 = fromIntegral b
    i2 = fromIntegral c
    e2 = fromIntegral d
    x = fromIntegral n