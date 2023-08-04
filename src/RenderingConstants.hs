module RenderingConstants
  ( platformImage,
    platformUnstable1Image,
    platformUnstable2Image,
    platformUnstable3Image,
    enemyFlyingSlimeImage,
    enemyPlatformImage,
    freezedEnemyPlatformImage,
    freezedEnemyFlyingSlimeImage,
    playerJumpUpImage,
    playerJumpDownImage,
    playerLoseImage,
    projectileImage,
    speedNormalImage,
    speedFastImage,
    speedFasterImage,
    speedUltraFastImage,
    endPlatformImage,
    gameBackgroundImage,
    textColor,
  )
where

import GHC.IO (unsafePerformIO)
import Graphics.Gloss (Picture, loadBMP)
import Graphics.Gloss.Data.Color (Color, white)

{-    //========\\
      || Colors ||
      \/========//    -}

textColor :: Color
textColor = white

{-    //=============\\
      || Images path ||
      \/=============//    -}

platformImagePath :: String
platformImagePath = "./src/images/platform.bmp"

platformUnstable1ImagePath :: String
platformUnstable1ImagePath = "./src/images/platformUnstable1.bmp"

platformUnstable2ImagePath :: String
platformUnstable2ImagePath = "./src/images/platformUnstable2.bmp"

platformUnstable3ImagePath :: String
platformUnstable3ImagePath = "./src/images/platformUnstable3.bmp"

enemyFlyingSlimeImagePath :: String
enemyFlyingSlimeImagePath = "./src/images/enemyFlyingSlime.bmp"

enemyPlatformImagePath :: String
enemyPlatformImagePath = "./src/images/enemyPlatform.bmp"

freezedEnemyPlatformImagePath :: String
freezedEnemyPlatformImagePath = "./src/images/freezedEnemyPlatform.bmp"

freezedEnemyFlyingSlimeImagePath :: String
freezedEnemyFlyingSlimeImagePath = "./src/images/freezedEnemyFlyingSlime.bmp"

playerJumpUpImagePath :: String
playerJumpUpImagePath = "./src/images/playerJumpUp.bmp"

playerJumpDownImagePath :: String
playerJumpDownImagePath = "./src/images/playerJumpDown.bmp"

playerLoseImagePath :: String
playerLoseImagePath = "./src/images/playerLose.bmp"

projectileImagePath :: String
projectileImagePath = "./src/images/projectile.bmp"

speedNormalImagePath :: String
speedNormalImagePath = "./src/images/speedNormal.bmp"

speedFastImagePath :: String
speedFastImagePath = "./src/images/speedFast.bmp"

speedFasterImagePath :: String
speedFasterImagePath = "./src/images/speedFaster.bmp"

speedUltraFastImagePath :: String
speedUltraFastImagePath = "./src/images/speedUltraFast.bmp"

endPlatformImagePath :: String
endPlatformImagePath = "./src/images/endPlatform.bmp"

gameBackgroundImagePath :: String
gameBackgroundImagePath = "./src/images/gameBackground.bmp"

{-    //========\\
      || Images ||
      \/========//    -}

platformImage :: Picture
{-# NOINLINE platformImage #-}
platformImage = unsafePerformIO $ loadBMP platformImagePath

platformUnstable1Image :: Picture
{-# NOINLINE platformUnstable1Image #-}
platformUnstable1Image = unsafePerformIO $ loadBMP platformUnstable1ImagePath

platformUnstable2Image :: Picture
{-# NOINLINE platformUnstable2Image #-}
platformUnstable2Image = unsafePerformIO $ loadBMP platformUnstable2ImagePath

platformUnstable3Image :: Picture
{-# NOINLINE platformUnstable3Image #-}
platformUnstable3Image = unsafePerformIO $ loadBMP platformUnstable3ImagePath

enemyFlyingSlimeImage :: Picture
{-# NOINLINE enemyFlyingSlimeImage #-}
enemyFlyingSlimeImage = unsafePerformIO $ loadBMP enemyFlyingSlimeImagePath

enemyPlatformImage :: Picture
{-# NOINLINE enemyPlatformImage #-}
enemyPlatformImage = unsafePerformIO $ loadBMP enemyPlatformImagePath

freezedEnemyPlatformImage :: Picture
{-# NOINLINE freezedEnemyPlatformImage #-}
freezedEnemyPlatformImage = unsafePerformIO $ loadBMP freezedEnemyPlatformImagePath

freezedEnemyFlyingSlimeImage :: Picture
{-# NOINLINE freezedEnemyFlyingSlimeImage #-}
freezedEnemyFlyingSlimeImage = unsafePerformIO $ loadBMP freezedEnemyFlyingSlimeImagePath

playerJumpUpImage :: Picture
{-# NOINLINE playerJumpUpImage #-}
playerJumpUpImage = unsafePerformIO $ loadBMP playerJumpUpImagePath

playerJumpDownImage :: Picture
{-# NOINLINE playerJumpDownImage #-}
playerJumpDownImage = unsafePerformIO $ loadBMP playerJumpDownImagePath

playerLoseImage :: Picture
{-# NOINLINE playerLoseImage #-}
playerLoseImage = unsafePerformIO $ loadBMP playerLoseImagePath

projectileImage :: Picture
{-# NOINLINE projectileImage #-}
projectileImage = unsafePerformIO $ loadBMP projectileImagePath

speedNormalImage :: Picture
{-# NOINLINE speedNormalImage #-}
speedNormalImage = unsafePerformIO $ loadBMP speedNormalImagePath

speedFastImage :: Picture
{-# NOINLINE speedFastImage #-}
speedFastImage = unsafePerformIO $ loadBMP speedFastImagePath

speedFasterImage :: Picture
{-# NOINLINE speedFasterImage #-}
speedFasterImage = unsafePerformIO $ loadBMP speedFasterImagePath

speedUltraFastImage :: Picture
{-# NOINLINE speedUltraFastImage #-}
speedUltraFastImage = unsafePerformIO $ loadBMP speedUltraFastImagePath

endPlatformImage :: Picture
{-# NOINLINE endPlatformImage #-}
endPlatformImage = unsafePerformIO $ loadBMP endPlatformImagePath

gameBackgroundImage :: Picture
{-# NOINLINE gameBackgroundImage #-}
gameBackgroundImage = unsafePerformIO $ loadBMP gameBackgroundImagePath