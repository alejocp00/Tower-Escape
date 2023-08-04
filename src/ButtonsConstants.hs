module ButtonsConstants (initButtonsPositions) where

import Buttons (Button (..), ButtonType (..))
import GameConstants (windowWidth)
import Graphics.Gloss (light, red, white)
import Graphics.Gloss.Data.Color (blue)

{-	//=============\\
	  || Button Size ||
	  \/=============//	-}

buttonsWidth :: Float
buttonsWidth = windowWidth

buttonsHight :: Float
buttonsHight = 50.0

{-  //=============\\
	  || Button list ||
	  \/=============//	-}

buttonList :: [Button]
buttonList =
  [ buttonShowDebug,
    buttonActiveDebugActions,
    buttonRegeneratePlatforms
  ]

-- | Initial Buttons Positions
initButtonsPositions :: [Button]
initButtonsPositions = zipWith (\b p -> b {buttonPosition = p}) buttonList positions
  where
    initialY = (-buttonsHight / 2) - 20
    separationY = 50 * (-1)
    positions = zipWith (\i _ -> (0, i)) [initialY, -buttonsHight + separationY ..] buttonList

{-	//====================\\
	  || Button declaration ||
	  \/====================//	-}

buttonShowDebug :: Button
buttonShowDebug =
  Button
    { buttonType = ShowDebug,
      buttonText = "Show debug values",
      buttonTextColor = white,
      buttonColor = light blue,
      buttonActivatedColor = light red,
      buttonSize = (buttonsWidth, buttonsHight),
      buttonActive = False,
      buttonPosition = (0, 0)
    }

-- | Active debug actions
buttonActiveDebugActions :: Button
buttonActiveDebugActions =
  Button
    { buttonType = ActiveDebugActions,
      buttonText = "Active debug actions",
      buttonTextColor = white,
      buttonColor = light blue,
      buttonActivatedColor = light red,
      buttonSize = (buttonsWidth, buttonsHight),
      buttonActive = False,
      buttonPosition = (0, 0)
    }

-- | regenerate platforms button
buttonRegeneratePlatforms :: Button
buttonRegeneratePlatforms =
  Button
    { buttonType = RegeneratePlatforms,
      buttonText = "Regenerate platforms",
      buttonTextColor = white,
      buttonColor = light blue,
      buttonActivatedColor = light red,
      buttonSize = (buttonsWidth, buttonsHight),
      buttonActive = False,
      buttonPosition = (0, 0)
    }