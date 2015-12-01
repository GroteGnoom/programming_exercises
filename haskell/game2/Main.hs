import qualified Graphics.UI.GLUT as GLUT
import qualified Data.IORef as DIOR
import qualified Mod
import qualified Init
import qualified View

data Inputs = Inputs { keysPressed :: DIOR.IORef [GLUT.Key]
                     }

main :: IO ()
main = do
  (_progName, _args) <- GLUT.getArgsAndInitialize
  _window <- GLUT.createWindow "Hello World"
  inputs <- initInputs
  outputs <- Init.initOutputs
  GLUT.keyboardMouseCallback GLUT.$= Just (keyboardMouse inputs)
  GLUT.displayCallback GLUT.$= View.view outputs
  GLUT.addTimerCallback 40 (update inputs Init.initModel outputs)
  GLUT.mainLoop

initInputs :: IO Inputs
initInputs = do
  _keysPressed <- DIOR.newIORef []
  return Inputs {keysPressed = _keysPressed}

keyboardMouse :: Inputs -> GLUT.KeyboardMouseCallback
keyboardMouse inputs key GLUT.Down _ _ = case key of
  (GLUT.SpecialKey _) -> addKey key (keysPressed inputs)
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()



update :: Inputs -> Mod.Model -> View.Outputs -> GLUT.TimerCallback
update inputs model outputs = do
  newModel1 <- updateInputs inputs model
  View.updateOutput (Mod.update newModel1) outputs
  GLUT.postRedisplay Nothing
  GLUT.addTimerCallback 40 (update inputs (Mod.update newModel1) outputs)

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

addKey :: GLUT.Key -> DIOR.IORef [GLUT.Key] -> IO ()
addKey key list = do
  oldList <- GLUT.get list
  DIOR.writeIORef list (key : oldList)
