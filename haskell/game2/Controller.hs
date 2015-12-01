module Controller where
import qualified Mod
import qualified Graphics.UI.GLUT as GLUT
import qualified Data.IORef as DIOR

data Inputs = Inputs { keysPressed :: DIOR.IORef [GLUT.Key]
                     }

updateInputs :: Inputs -> Mod.Model -> IO Mod.Model
updateInputs inputs model = do
  _keysPressed <- GLUT.get (keysPressed inputs)
  DIOR.writeIORef (keysPressed inputs) [] -- TODO Fix these two? Is there a guarantee that key presses aren't lost in between? According to 'tackling the awkward squad' this should be fine. But 'do notation considered harmful' says "Newcomers might think that the order of statements determines the order of execution."
  return Mod.Model { Mod.allKeysPressed = (Mod.frame model, _keysPressed) : Mod.allKeysPressed model
               , Mod.frame = Mod.frame model
               , Mod.player =Mod.player model
               , Mod.speed = Mod.speed model
               , Mod.enemy = Mod.enemy model
               }

keyboardMouse :: Inputs -> GLUT.KeyboardMouseCallback
keyboardMouse inputs key GLUT.Down _ _ = case key of
  (GLUT.SpecialKey _) -> addKey key (keysPressed inputs)
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()

addKey :: GLUT.Key -> DIOR.IORef [GLUT.Key] -> IO ()
addKey key list = do
  oldList <- GLUT.get list
  DIOR.writeIORef list (key : oldList)
