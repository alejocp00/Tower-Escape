{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Game
  ( Point,
    Player (..),
    State (..),
    PlatformType (..),
    Platform (..),
    Projectile (..),
    Game (..),
    EnemyState (..),
    EnemyType (..),
  )
where

import Buttons (Button)
import Graphics.Gloss (Point)
import Graphics.Gloss.Data.Vector

-- | Player data type
data Player = Player
  { -- | Current position of the player
    playerPosition :: Point,
    -- | Current speed of the player
    playerSpeed :: Vector,
    -- | Indicator if the player is falling 
    playerIsFalling :: Bool,
    -- | Indicator if the player is alive
    playerAlive :: Bool,
    -- | Indicator if the player is inmune
    playerIsInmune :: Bool
  }

-- | Game state definition
data State
  = Idle
  | Playing
  | GameOver
  deriving (Eq, Show)

-- | Enemy state definition
data EnemyState
  = Alive
  | Freezed
  deriving (Eq, Show)

-- | Enemy type definition
data EnemyType
  = FlyingSlime
  | Default
  deriving (Eq, Show)

-- | Platform type definition
data PlatformType
  = Normal
  | Unstable Int
  | Enemy EnemyType EnemyState
  | EndPlatform
  deriving (Eq, Show)

-- | Platform Data Type
data Platform = Platform
  { -- | Platform identifier
    platformId :: Int,
    -- | Current platform position
    platformPosition :: Point,
    -- | Platform type
    platformType :: PlatformType,
    -- | Impulse that the platform gives to the player
    platformImpulse :: Float,
    -- | End point of platform movement
    platformEndMovement :: Point,
    -- | Start point of platform movement
    platformStartMovement :: Point,
    -- | Platform size
    platformSize :: Vector,
    -- | Platform direction: left or right
    platformDirection :: Int
  }

-- | Definition of Equal for platform
instance Eq Platform where
  a == b = platformId a == platformId b
  a /= b = not (a == b)

-- | Projectile data type
data Projectile = Projectile
  {
    -- | Projectile identifier
    projectileId :: Int,
    -- | Projectile position
    projectilePosition :: Point,
    -- | Projectile Movement
    projectileMovement :: Point
  }

-- | Definition of Equal for projectiles
instance Eq Projectile where
  a == b = projectileId a == projectileId b
  a /= b = not (a == b)

-- | Game data type
data Game = Game
  { -- | Current game state
    gameState :: State,
    -- | Current game camera position
    gameCameraPos :: Point,
    -- | This value indicate if the game need to generate new platforms
    gameNeedPlatforms :: Bool,
    -- | Current game platforms
    gamePlatforms :: [Platform],
    -- | Current game platforms count
    gamePlatformsCount :: Int,
    -- | Current player of the game
    gamePlayer :: Player,
    -- | This value indicate if the player is moving to the left or to the right
    gamePlayerAxisX :: (Bool, Bool),
    -- | Current projectiles on the game
    gameProjectiles :: [Projectile],
    -- | Current game score
    gameScore :: Int,
    -- | Current game buttons
    gameButtons :: [Button],
    -- | Current game best scores
    gameBestScores :: [Int],
    -- | Current game seed
    gameSeed :: Int
  }