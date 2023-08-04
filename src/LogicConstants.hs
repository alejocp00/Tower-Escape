module LogicConstants
  ( speedNormal,
    speedFast,
    speedFaster,
    speedUltraFast,
    speedDeadPlayer,
    playerSpeedX,
    gravity,
    playerInmuneSpeed,
    platformSpeed,
    initialPlatform,
    endPlatform,
    initialPlayer,
    emptyGame,
    shortInterval,
    mediumInterval,
    endScreenBelow,
    endScreenUp,
    longInterval,
    soundBrokenPlatform,
    soundClick,
    soundLose,
    soundJumpHight,
    soundFreeze,
    soundShoot,
    soundJump,
    saveFilePath,
    projectileSpeed,
  )
where

import Buttons
import Game
import GameConstants

{-    //================\\
      || Player related ||
      \/================//    -}

-- | Normal speed value
speedNormal :: Float
speedNormal = 400.0

-- | Fast speed value
speedFast :: Float
speedFast = 600.0

-- | Faster speed value
speedFaster :: Float
speedFaster = 800.0

-- | Ultra fast speed value
speedUltraFast :: Float
speedUltraFast = 1200.0

-- | Speed of the player when is dead
speedDeadPlayer :: Float
speedDeadPlayer = 10.0

-- | Player speed in x axis
playerSpeedX :: Float
playerSpeedX = 400.0

-- | Gravity applied to the player
gravity :: Float
gravity = -200

-- | Speed when the player is Inmune
playerInmuneSpeed :: Float
playerInmuneSpeed = projectileSpeed * (4 / 5)

{-    //=====================\\
      || Projectiles related ||
      \/=====================//    -}

projectileSpeed :: Float
projectileSpeed = 900.0

{-    //==================\\
      || Platform related ||
      \/==================//    -}

platformSpeed :: Float
platformSpeed = 200.0

{-    //================\\
      || Initial values ||
      \/================//    -}

initialPlatform :: Platform
initialPlatform =
  Platform
    { platformType = Normal,
      platformStartMovement = (0, 0),
      platformSize = (platformWidth, platformHeight),
      platformPosition = (0, 0),
      platformImpulse = speedNormal,
      platformId = 1,
      platformEndMovement = (0, 0),
      platformDirection = 1
    }

endPlatform :: Platform
endPlatform =
  Platform
    { platformType = EndPlatform,
      platformStartMovement = (x, y),
      platformSize = (windowWidth, 4 + playerHeight),
      platformPosition = (x, y),
      platformImpulse = 0,
      platformId = 0,
      platformEndMovement = (x, y),
      platformDirection = 1
    }
  where
    x = 0
    y = -windowHeight - platformHeight

initialPlayer :: Player
initialPlayer =
  Player
    { playerSpeed = (0, 0),
      playerPosition = (0, platformHeight / 2 + playerHeight / 2 + 1),
      playerIsFalling = True,
      playerAlive = True,
      playerIsInmune = False
    }

emptyGame :: [Button] -> [Int] -> Int -> Game
emptyGame buttons scores seed =
  Game
    { gameState = Idle,
      gameCameraPos = (0, 0),
      gameNeedPlatforms = True,
      gamePlatforms = [],
      gamePlatformsCount = 0,
      gamePlayer = initialPlayer,
      gamePlayerAxisX = (False, False),
      gameProjectiles = [],
      gameScore = 0,
      gameButtons = buttons,
      gameBestScores = scores,
      gameSeed = seed
    }

{-    //====================\\
      || Game state related ||
      \/====================//    -}

i0, i1, i2 :: Float -> Float
i0 h = 5 + (h / 2)
i1 h = i0 h + (normalJumpDistance - h / 2.0) / 3.0
i2 h = i1 h + (normalJumpDistance - h / 2.0) / 3.0

normalJumpDistance :: Float
normalJumpDistance = 300

-- | This represent the short interval between a platform an the next one
shortInterval :: Float -> Float -> (Int, Int)
shortInterval h y = (truncate initial, truncate final)
  where
    initial = y + i0 h
    final = y + i1 h

-- | This represent the medium interval between a platform an the next one
mediumInterval :: Float -> Float -> (Int, Int)
mediumInterval h y = (truncate initial, truncate final)
  where
    initial = y + i1 h
    final = y + i2 h

-- | This represent the long interval between a platform an the next one
longInterval :: Float -> Float -> (Int, Int)
longInterval h y = (truncate initial, truncate final)
  where
    initial = y + i2 h
    final = y + (normalJumpDistance - h / 2.0)

-- | The smaller y axis value at the screen
endScreenBelow :: Game -> Float
endScreenBelow game = snd (gameCameraPos game) - (windowHeight / 2.0)

-- | The bigger y axis value at the screen
endScreenUp :: Game -> Float
endScreenUp game = snd (gameCameraPos game) + (windowHeight / 2)

saveFilePath :: FilePath
saveFilePath = "./.bestScores.yaml"

{-    //===============\\
      || Sound related ||
      \/===============//    -}

soundJump :: FilePath
soundJump = "./src/sounds/jump.wav"

soundShoot :: FilePath
soundShoot = "./src/sounds/shoot.wav"

soundFreeze :: FilePath
soundFreeze = "./src/sounds/freeze.wav"

soundJumpHight :: FilePath
soundJumpHight = "./src/sounds/jumpHight.wav"

soundLose :: FilePath
soundLose = "./src/sounds/lose.wav"

soundClick :: FilePath
soundClick = "./src/sounds/click.wav"

soundBrokenPlatform :: FilePath
soundBrokenPlatform = "./src/sounds/brokenPlatform.wav"