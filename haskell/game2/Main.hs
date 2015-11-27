import qualified Graphics.UI.GLUT as GLUT
import qualified Data.IORef as DIOR

main :: IO ()
main = do
  (_progName, _args) <- GLUT.getArgsAndInitialize
  _window <- GLUT.createWindow "Hello World"
  model <- initModel
  GLUT.keyboardMouseCallback GLUT.$= Just (keyboardMouse model)
  GLUT.displayCallback GLUT.$= view model
  GLUT.addTimerCallback 40 (eachFrame model)
  GLUT.mainLoop

initModel :: IO Model
initModel = do
  pos <- DIOR.newIORef (0, 0)
  frame <- DIOR.newIORef 0
  return Model {myPos = pos, myFrame = frame}

view :: Model -> GLUT.DisplayCallback
view model = do
  GLUT.clear [ GLUT.ColorBuffer ]
  (x',y') <- GLUT.get (myPos model)
  GLUT.renderPrimitive GLUT.Points $
     mapM_ (\(x, y, z) -> GLUT.vertex $ GLUT.Vertex3 x y z) [(x',y',0)]
  GLUT.flush

data Model = Model { myPos :: DIOR.IORef (GLUT.GLfloat, GLUT.GLfloat)
                   , myFrame :: DIOR.IORef Int
                   } deriving (Eq)

keyboardMouse :: Model -> GLUT.KeyboardMouseCallback
keyboardMouse model key GLUT.Down _ _ = case key of
  (GLUT.SpecialKey GLUT.KeyLeft ) -> myPos model GLUT.$~! \(x,y) -> (x-0.1,y)
  (GLUT.SpecialKey GLUT.KeyRight) -> myPos model GLUT.$~! \(x,y) -> (x+0.1,y)
  (GLUT.SpecialKey GLUT.KeyUp   ) -> myPos model GLUT.$~! \(x,y) -> (x,y+0.1)
  (GLUT.SpecialKey GLUT.KeyDown ) -> myPos model GLUT.$~! \(x,y) -> (x,y-0.1)
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()

eachFrame :: Model -> GLUT.TimerCallback
eachFrame model = do
  GLUT.postRedisplay Nothing
  GLUT.addTimerCallback 40 (eachFrame (update model))

update :: Model -> Model
update model = model
