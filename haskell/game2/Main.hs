import qualified Graphics.UI.GLUT as GLUT
import qualified Data.IORef as DIOR
import qualified Safe
import qualified Geometry as Geo

data Inputs = Inputs { keysPressed :: DIOR.IORef [GLUT.Key]
                     }

data Outputs = Outputs { outPoly :: DIOR.IORef [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)]
                       , outEnPoly :: DIOR.IORef [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)]}

data Model = Model { allKeysPressed :: [(Int, [GLUT.Key])]
                   , frame :: Int
                   , player :: Geo.Poly
                   , speed :: Float
                   , enemy :: Geo.Poly
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
  _outPoly <- DIOR.newIORef (polyToOutPoly player1)
  _outEnPoly <- DIOR.newIORef (polyToOutPoly enemy1)
  return Outputs {outPoly = _outPoly, outEnPoly = _outEnPoly}

initModel :: Model
initModel =
  Model { allKeysPressed = []
        , frame = 0
        , player = player1
        , speed = 0.1
        , enemy = enemy1
        }

view :: Outputs -> GLUT.DisplayCallback
view outputs = do
  GLUT.clear [ GLUT.ColorBuffer ]
  poly <- GLUT.get (outPoly outputs)
  enPoly <- GLUT.get (outEnPoly outputs)
  GLUT.renderPrimitive GLUT.Polygon $
    mapM_ (\(x, y, z) -> GLUT.vertex $ GLUT.Vertex3 x y z) poly
  GLUT.renderPrimitive GLUT.Polygon $
    mapM_ (\(x, y, z) -> GLUT.vertex $ GLUT.Vertex3 x y z) enPoly
  GLUT.flush

keyboardMouse :: Inputs -> GLUT.KeyboardMouseCallback
keyboardMouse inputs key GLUT.Down _ _ = case key of
  (GLUT.SpecialKey _) -> addKey key (keysPressed inputs)
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()

keyToVec :: GLUT.Key -> Geo.Vec
keyToVec key = case key of
  (GLUT.SpecialKey GLUT.KeyLeft ) -> Geo.Vec (-1) 0
  (GLUT.SpecialKey GLUT.KeyRight) -> Geo.Vec 1 0
  (GLUT.SpecialKey GLUT.KeyUp   ) -> Geo.Vec 0 1
  (GLUT.SpecialKey GLUT.KeyDown ) -> Geo.Vec 0 (-1)
  _ -> Geo.Vec 0 0

update :: Inputs -> Model -> Outputs -> GLUT.TimerCallback
update inputs model outputs = do
  newModel1 <- updateInputs inputs model
  updateOutput (updateModel newModel1) outputs
  GLUT.postRedisplay Nothing
  GLUT.addTimerCallback 40 (update inputs (updateModel newModel1) outputs)

updateInputs :: Inputs -> Model -> IO Model
updateInputs inputs model = do
  _keysPressed <- GLUT.get (keysPressed inputs)
  DIOR.writeIORef (keysPressed inputs) [] -- TODO Fix these two? Is there a guarantee that key presses aren't lost in between? According to 'tackling the awkward squad' this should be fine. But 'do notation considered harmful' says "Newcomers might think that the order of statements determines the order of execution."
  return Model { allKeysPressed = (frame model, _keysPressed) : allKeysPressed model
               , frame = frame model
               , player = player model
               , speed = speed model
               , enemy = enemy model
               }

updateModel :: Model -> Model
updateModel model =
  case Safe.headMay (allKeysPressed model) of
    Just (lastFrameWithPress, lastKeys) ->
      if lastFrameWithPress == frame model
        then Model { allKeysPressed = allKeysPressed model
                   , frame = frame model
                   , player = Geo.move (player model)
                                       (Geo.vMul (foldl Geo.vAdd (Geo.Vec 0 0)
                                                            (map keyToVec lastKeys))
                                            (speed model))
                    , speed = speed model
                    , enemy = enemy model}
        else model
    Nothing -> model

updateOutput :: Model -> Outputs -> IO()
updateOutput model output = do
  DIOR.writeIORef (outPoly output) (polyToOutPoly (player model))
  return ()

addKey :: GLUT.Key -> DIOR.IORef [GLUT.Key] -> IO ()
addKey key list = do
  oldList <- GLUT.get list
  DIOR.writeIORef list (key : oldList)

fToGL :: Float -> GLUT.GLfloat
fToGL = realToFrac :: Float -> GLUT.GLfloat

posToOutPos :: Geo.Vec -> (GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)
posToOutPos (Geo.Vec a b) = (fToGL a, fToGL b, 0)

polyToOutPoly :: Geo.Poly -> [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)]
polyToOutPoly (Geo.Poly vecList) =
  map posToOutPos vecList


player1 :: Geo.Poly
player1 =
  Geo.Poly [ Geo.Vec 0 0
           , Geo.Vec 0 0.1
           , Geo.Vec 0.1 0.1
           , Geo.Vec 0.1 0
           ]

enemy1 :: Geo.Poly
enemy1 =
  Geo.Poly [ Geo.Vec 0.2 0.2
           , Geo.Vec 0.2 0.3
           , Geo.Vec 0.3 0.3
           , Geo.Vec 0.3 0.2
           ]
