import qualified Graphics.UI.GLUT as GLUT
import qualified Data.IORef as DIOR
import qualified Safe

data Inputs = Inputs { keysPressed :: DIOR.IORef [GLUT.Key]
                     }

data Outputs = Outputs {outPos :: DIOR.IORef (GLUT.GLfloat, GLUT.GLfloat)}

data Model = Model { pos :: (Float, Float)
                   , allKeysPressed :: [(Int, [GLUT.Key])]
                   , frame :: Int
                   }

main :: IO ()
main = do
  (_progName, _args) <- GLUT.getArgsAndInitialize
  _window <- GLUT.createWindow "Hello World"
  inputs <- initInputs
  outputs <- initOutputs
  GLUT.keyboardMouseCallback GLUT.$= Just (keyboardMouse inputs)
  GLUT.displayCallback GLUT.$= view outputs
  GLUT.addTimerCallback 40 (update inputs initModel outputs)
  GLUT.mainLoop

initInputs :: IO Inputs
initInputs = do
  _keysPressed <- DIOR.newIORef []
  return Inputs {keysPressed = _keysPressed}

initOutputs :: IO Outputs
initOutputs = do
  _outPos <- DIOR.newIORef (0, 0)
  return Outputs {outPos = _outPos}

initModel :: Model
initModel =
  Model { pos = (0,0), allKeysPressed = [], frame = 0}

view :: Outputs -> GLUT.DisplayCallback
view outputs = do
  GLUT.clear [ GLUT.ColorBuffer ]
  (x',y') <- GLUT.get (outPos outputs)
  GLUT.renderPrimitive GLUT.Points $
     mapM_ (\(x, y, z) -> GLUT.vertex $ GLUT.Vertex3 x y z) [(x',y',0)]
  GLUT.flush

keyboardMouse :: Inputs -> GLUT.KeyboardMouseCallback
keyboardMouse inputs key GLUT.Down _ _ = case key of
  (GLUT.SpecialKey _) -> addKey key (keysPressed inputs)
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()

move :: (Float, Float) -> GLUT.Key -> (Float, Float)
move (x,y) key = case key of
  (GLUT.SpecialKey GLUT.KeyLeft ) -> (x-0.1,y)
  (GLUT.SpecialKey GLUT.KeyRight) -> (x+0.1,y)
  (GLUT.SpecialKey GLUT.KeyUp   ) -> (x,y+0.1)
  (GLUT.SpecialKey GLUT.KeyDown ) -> (x,y-0.1)
  _ -> (x,y)

update :: Inputs -> Model -> Outputs -> GLUT.TimerCallback
update inputs model outputs = do
  newModel1 <- updateModel inputs model
  updateOutput (updateState newModel1) outputs
  GLUT.postRedisplay Nothing
  GLUT.addTimerCallback 40 (update inputs (updateState newModel1) outputs)

updateModel :: Inputs -> Model -> IO Model
updateModel inputs model = do
  _keysPressed <- GLUT.get (keysPressed inputs)
  DIOR.writeIORef (keysPressed inputs) [] -- TODO Fix these two? Is there a guarantee that key presses aren't lost in between? According to 'tackling the awkward squad' this should be fine. But 'do notation considered harmful' says "Newcomers might think that the order of statements determines the order of execution."
  return Model {pos = pos model
               , allKeysPressed = (frame model, _keysPressed) : allKeysPressed model
               , frame = frame model
               }

updateState :: Model -> Model
updateState model =
  case Safe.headMay (allKeysPressed model) of
    Just (lastFrameWithPress, lastKeys) ->
      if lastFrameWithPress == frame model
        then Model {pos = foldl move (pos model) lastKeys
                   , allKeysPressed = allKeysPressed model
                   , frame = frame model}
        else model
    Nothing -> model

updateOutput :: Model -> Outputs -> IO()
updateOutput model output = do
  DIOR.writeIORef (outPos output) (posToOutPos (pos model))
  return ()

addKey :: GLUT.Key -> DIOR.IORef [GLUT.Key] -> IO ()
addKey key list = do
  oldList <- GLUT.get list
  DIOR.writeIORef list (key : oldList)

fToGL :: Float -> GLUT.GLfloat
fToGL = realToFrac :: Float -> GLUT.GLfloat

posToOutPos :: (Float, Float) -> (GLUT.GLfloat, GLUT.GLfloat)
posToOutPos (a,b) = (fToGL a, fToGL b)
