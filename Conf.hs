import Graphics.Gloss
import Graphics.Gloss.Data.Color

-- Import your constants from a module
import Constants

-- Define yourconfiguration
config :: WindowConfig
config = InWindow
    { windowTitle = title  -- Change le titre de la fenêtre
    , windowIcon  = Just (Bitmap pathIcon)  -- Change l'icone de la fenêtre
    , windowDimensions = (winWidth, winHeight)  -- Change la taille de la fenêtre
    }

--main :: IO ()
--main = do
    -- Initialize your Love2D game with the configuration
  --  playIO config
  --      black  -- Background color
    --    60     -- Frames per second
    --    initialGameState  -- Initial game state
      --  render            -- Render function
      --  update            -- Update function
