module GameConstants
  ( windowWidth,
    windowHeight,
    platformWidth,
    platformHeight,
    playerWidth,
    playerHeight,
    projectileRadius,
    flayingSlimeWidth,
    flayingSlimeHeight,
    enemyTypes,
  )
where

{-	//================\\
	  || Window related ||
	  \/================//	-}

windowWidth, windowHeight :: Float
windowWidth = 490.0
windowHeight = 700.0

{-	//==================\\
	  || Platform related ||
	  \/==================//	-}

platformWidth, platformHeight, playerWidth, playerHeight :: Float
platformWidth = 70.0
platformHeight = 15.0
playerWidth = 40.0
playerHeight = 50.0

{-	//====================\\
	  || Projectile Related ||
	  \/====================//	-}

projectileRadius :: Float
projectileRadius = 5.0

{-	//=====================\\
	  || Enemy Types related ||
	  \/=====================//	-}

flayingSlimeWidth, flayingSlimeHeight :: Float
flayingSlimeWidth = 100.0
flayingSlimeHeight = 50.0

enemyTypes :: Int
enemyTypes = 2