{-# OPTIONS_GHC -Wno-unused-imports #-}

module Rendering (gameAsPicture) where

import Buttons
import Codec.Picture.Jpg.Internal.Types (JpgAdobeApp14 (_adobeFlag0))
import GHC.Int (Int32)
import Game
  ( EnemyState (..),
    EnemyType (..),
    Game (..),
    Platform (..),
    PlatformType (..),
    Player (..),
    Projectile (..),
    State (GameOver, Idle, Playing),
  )
import GameConstants
import Graphics.Gloss (Picture (Translate), black, blue, circle, circleSolid, color, dark, green, line, pictures, rectangleSolid, rectangleWire, red, scale, text, translate, white)
import Graphics.Gloss.Data.Color (Color)
import Graphics.Gloss.Data.Picture (blank)
import Graphics.Gloss.Data.ViewPort
  ( ViewPort
      ( ViewPort,
        viewPortRotate,
        viewPortScale,
        viewPortTranslate
      ),
    applyViewPortToPicture,
  )
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.UI.GLUT.Fonts
import Logic (getButtonState)
import LogicConstants
import RenderingConstants
import System.IO.Unsafe (unsafePerformIO)

{-    //==================\\
      || Draw player zone ||
      \/==================//    -}

drawPlayer :: Game -> Picture
drawPlayer game =
  pictures $
    map (translate x y) [backPlayerImage, playerImage]
  where
    player_in = gamePlayer game
    state = gameState game
    (x, y) = playerPosition player_in
    (w, h) = (playerWidth, playerHeight)
    backPlayerImage
      | playerIsInmune player_in = color green $ circle (max w h)
      | otherwise = blank
    playerImage = case state of
      GameOver -> playerLoseImage
      _ -> if playerIsFalling player_in then playerJumpDownImage else playerJumpUpImage

{-    //=====================\\
      || Draw platforms zone ||
      \/=====================//    -}

drawPlatform :: Platform -> Picture
drawPlatform platform =
  putSpeedInfo
    platform
    ( case currentType of
        (Unstable t) -> unstableImage t
        Enemy eType eState -> translate x y $ scale (w / 70) (h / 15) (enemyImage eType eState)
        _ -> translate x y $ color (dark green) platformImage
    )
  where
    (x, y) = platformPosition platform
    (w, h) = platformSize platform
    currentType = platformType platform
    unstableImage t = case t of
      1 -> translate x y platformUnstable1Image
      2 -> translate x y platformUnstable2Image
      3 -> translate x y platformUnstable3Image
      _ -> translate x y $ color (dark red) $ rectangleSolid platformWidth platformHeight
    enemyImage :: EnemyType -> EnemyState -> Picture
    enemyImage eType eState
      | eState == Freezed = case eType of
          FlyingSlime -> freezedEnemyFlyingSlimeImage
          _ -> freezedEnemyPlatformImage
      | otherwise = case eType of
          FlyingSlime -> enemyFlyingSlimeImage
          _ -> enemyPlatformImage

-- | Put the picture according of the speed of the platform
putSpeedInfo :: Platform -> Picture -> Picture
putSpeedInfo platform picture = pictures [picture, translate x y $ scale (w / platformWidth) (h / platformHeight) speedImage]
  where
    (x, y) = platformPosition platform
    (w, h) = platformSize platform
    speed = platformImpulse platform
    speedImage
      | speed == speedNormal = speedNormalImage
      | speed == speedFast = speedFastImage
      | speed == speedFaster = speedFasterImage
      | speed == speedUltraFast = speedUltraFastImage
      | otherwise = blank

-- | Draw the final platform
drawEndPlatform :: Game -> Picture
drawEndPlatform game = pictures [translate x newY $ color red $ rectangleSolid w h | newY <- yPositions]
  where
    platform = last $ gamePlatforms game
    (x, y) = platformPosition platform
    (w, h) = platformSize platform
    y1 = y
    y2 = y1 - h / 2
    yPositions = [y1, y2 .. (endScreenBelow game - 10)]

{-    //=======================\\
      || Draw projectiles zone ||
      \/=======================//    -}

drawProjectile :: Projectile -> Picture
drawProjectile projectile = translate x y projectileImage
  where
    (x, y) = projectilePosition projectile

{-    //================\\
      || Draw text zone ||
      \/================//    -}

-- | Draw "Press space to start" text in the center of the screen
drawInitialText :: Game -> Picture
drawInitialText game = case state of
  Idle -> drawCenteredText "Press space to start" (0, 0) (0.3, 0.3) textColor
  _ -> blank
  where
    state = gameState game

-- | Draw the final text "You Lose"
drawLoseText :: Game -> Picture
drawLoseText game = case state of
  GameOver ->
    pictures
      [ drawCenteredText "You Lose T_T" (0, y + 150) (0.3, 0.3) textColor,
        drawCenteredText ("Score:" ++ show score) (0, y + 100) (0.3, 0.3) textColor,
        drawCenteredText ("Max Score:" ++ show maxScore) (0, y + 50) (0.3, 0.3) textColor,
        drawCenteredText "Press space to restart" (0, y) (0.3, 0.3) textColor
      ]
  _ -> blank
  where
    score = gameScore game
    maxScore = head $ gameBestScores game
    (_, y) = gameCameraPos game
    state = gameState game

drawDebugValues :: Game -> Picture
drawDebugValues game
  | debugValues =
      pictures
        [ translate (x - 100) (y + 100) $
            scale 0.1 0.1 $
              color textColor $
                text ("Camera: " ++ show (gameCameraPos game)),
          translate (x - 100) (y + 80) $
            scale 0.1 0.1 $
              color textColor $
                text ("Player: " ++ show (playerPosition $ gamePlayer game)),
          translate (x - 100) (y + 60) $
            scale 0.1 0.1 $
              color textColor $
                text ("Enemys: " ++ show enemys),
          translate (x - 100) (y + 40) $
            scale 0.1 0.1 $
              color textColor $
                text ("Unstable: " ++ show unstable),
          translate (x - 100) (y + 20) $
            scale 0.1 0.1 $
              color textColor $
                text ("Normals: " ++ show normals),
          translate (x - 100) y $
            scale 0.1 0.1 $
              color textColor $
                text ("EndPlatforms: " ++ show endPlatforms),
          translate (x - 100) (y - 20) $
            scale 0.1 0.1 $
              color textColor $
                text ("Projectiles: " ++ show projectilesCount),
          translate (x - 100) (y + 120) $
            scale 0.1 0.1 $
              color textColor $
                text ("EndPlatform Pos:" ++ show (platformPosition (last (gamePlatforms game)))),
          translate (x - 100) (y + 140) $
            scale 0.1 0.1 $
              color textColor $
                text ("Player speed:" ++ show (playerSpeed (gamePlayer game)))
        ]
  | otherwise = blank
  where
    debugValues = getButtonState (gameButtons game) ShowDebug
    (x, y) = gameCameraPos game
    (enemys, unstable, normals, endPlatforms) = getPlatformsData (gamePlatforms game)
    projectilesCount = length $ gameProjectiles game
    getPlatformsData :: [Platform] -> (Int, Int, Int, Int)
    getPlatformsData [] = (0, 0, 0, 0)
    getPlatformsData (platform : xs) = case platformType platform of
      Enemy {} -> (1 + enemysT, unstablesT, normalsT, endPlatformsT)
      Unstable _ -> (enemysT, 1 + unstablesT, normalsT, endPlatformsT)
      Normal -> (enemysT, unstablesT, 1 + normalsT, endPlatformsT)
      _ -> (enemysT, unstablesT, normalsT, 1 + endPlatformsT)
      where
        (enemysT, unstablesT, normalsT, endPlatformsT) = getPlatformsData xs

{-    //=================\\
      || Draw score zone ||
      \/=================//    -}

drawBestScores :: Game -> Picture
drawBestScores game
  | gameState game == Playing = color white $ pictures [line [(xi, fromIntegral y), (xe, fromIntegral y)] | y <- gameBestScores game, inScreen $ fromIntegral y]
  | otherwise = blank
  where
    xi = (-windowWidth) / 2
    xe = xi + 30
    inScreen value = value <= endScreenUp game && value >= endScreenBelow game

-- | Draw the score in the top left corner of the screen
drawScore :: Game -> Picture
drawScore game = case state of
  Playing -> translate textX textY $ scale 0.3 0.3 $ color textColor $ text ("Score: " ++ show score)
  _ -> blank
  where
    score = gameScore game
    (_, y) = gameCameraPos game
    state = gameState game
    textHeight = 35
    textX = (-windowWidth) / 2
    textY = ((windowHeight / 2) + y) - textHeight

{-    //===================\\
      || Draw buttons zone ||
      \/===================//    -}

drawButtons :: Game -> Picture
drawButtons game = case gameState game of
  Idle -> pictures [drawButton button | button <- gameButtons game]
  _ -> blank
  where
    drawButton :: Button -> Picture
    drawButton button =
      pictures
        [ translate x y $ color bColor $ rectangleSolid w h,
          drawCenteredText bText (x, y) (0.2, 0.2) btColor
        ]
      where
        (x, y) = buttonPosition button
        (w, h) = buttonSize button
        bText = buttonText button
        btColor = buttonTextColor button
        bColor
          | buttonActive button = buttonActivatedColor button
          | otherwise = buttonColor button

{-    //======================\\
      || Draw background zone ||
      \/======================//    -}

drawBackground :: Game -> Picture
drawBackground game = pictures (reverse [translate 0 y $ scale (windowWidth / 780) (windowHeight / 797) gameBackgroundImage | y <- positions])
  where
    positions = [-windowHeight, 0 .. endScreenUp game + windowHeight / 2 + 1]

drawOutsideBorders :: Game -> Picture
drawOutsideBorders game =
  pictures
    [ translate leftX cy $ color borderColor $ rectangleSolid lw (fromIntegral screenH),
      translate rightX cy $ color borderColor $ rectangleSolid rw (fromIntegral screenH),
      translate 0 topY $ color borderColor $ rectangleSolid windowWidth th,
      translate 0 bottomY $ color borderColor $ rectangleSolid windowWidth bh
    ]
  where
    (screenW, screenH) = unsafePerformIO getScreenSize
    (_, cy) = gameCameraPos game
    rw = (fromIntegral screenW / 2) - (windowWidth / 2)
    rightX = (windowWidth / 2) + (rw / 2)
    lw = rw
    leftX = -rightX
    th = (fromIntegral screenH + cy) - endScreenUp game
    topY = endScreenUp game + th / 2
    bh = th
    bottomY = endScreenBelow game - bh / 2
    borderColor = black

{-    //==================================\\
      || Principal Game renderer function ||
      \/==================================//    -}

-- | Manage rendering of the game
gameAsPicture :: Game -> Picture
gameAsPicture game =
  applyViewPortToPicture camera $
    pictures $
      [drawBackground game]
        ++ map drawPlatform (gamePlatforms game)
        ++ map drawProjectile (gameProjectiles game)
        ++ [drawBestScores game]
        ++ [drawPlayer game]
        ++ [drawButtons game]
        ++ [drawScore game]
        ++ [drawInitialText game]
        ++ [drawLoseText game]
        ++ [drawEndPlatform game]
        ++ [drawDebugValues game]
        ++ [drawOutsideBorders game]
  where
    cameraRotation = 0
    cameraScale = 1
    (cameraPosX, cameraPosY) = gameCameraPos game
    cameraPos = (cameraPosX, cameraPosY * (-1))
    camera = ViewPort {viewPortRotate = cameraRotation, viewPortScale = cameraScale, viewPortTranslate = cameraPos}

{-    //=======\\
      || Utils ||
      \/=======//    -}

drawCenteredText :: String -> (Float, Float) -> (Float, Float) -> Color -> Picture
drawCenteredText str (x, y) (scaleX, scaleY) tColor = translate (x - (fromIntegral width / 2) * scaleX) (y - (height / 2) * scaleY) $ scale scaleX scaleY $ color tColor $ text str
  where
    (width, height) = unsafePerformIO $ getFontSize str

getFontSize :: String -> IO (Int32, Float)
getFontSize str = do
  width <- stringWidth Roman str
  height <- fontHeight Roman
  return (width, height)
