import qualified Graphics.UI.GLUT as GLUT
import qualified Mod
import qualified Init
import qualified View
import qualified Controller as Ctrl

main :: IO ()
main = do
  (_progName, _args) <- GLUT.getArgsAndInitialize
  _window <- GLUT.createWindow "Hello World"
  inputs <- Init.initInputs
  outputs <- Init.initOutputs
  GLUT.keyboardMouseCallback GLUT.$= Just (Ctrl.keyboardMouse inputs)
  GLUT.displayCallback GLUT.$= View.view outputs
  GLUT.addTimerCallback 40 (update inputs Init.initModel outputs)
  GLUT.mainLoop

update :: Ctrl.Inputs -> Mod.Model -> View.Outputs -> GLUT.TimerCallback
update inputs model outputs = do
  newModel1 <- Ctrl.updateInputs inputs model
  Mod.updateOutput (Mod.update newModel1) outputs
  GLUT.postRedisplay Nothing
  GLUT.addTimerCallback 40 (update inputs (Mod.update newModel1) outputs)
