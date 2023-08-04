module Buttons (Button (..), ButtonType (..)) where

import Graphics.Gloss (Point)
import Graphics.Gloss.Data.Color (Color)
import Graphics.Gloss.Data.Vector (Vector)

-- | Type of the button
data ButtonType
  = ShowDebug
  | ActiveDebugActions
  | RegeneratePlatforms
  deriving (Eq, Show)

-- | Button data type
data Button = Button
  { buttonType :: ButtonType,
    buttonPosition :: Point,
    buttonSize :: Vector,
    buttonColor :: Color,
    buttonActivatedColor :: Color,
    buttonTextColor :: Color,
    buttonActive :: Bool,
    buttonText :: String
  }

-- | Instance of Equal for button
instance Eq Button where
  a == b = buttonType a == buttonType b
  a /= b = not (a == b)
