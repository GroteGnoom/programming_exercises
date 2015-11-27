import qualified Graphics.UI.GLUT as GLUT
import qualified Data.IORef as DIOR

main :: IO ()
main = do
  (_progName, _args) <- GLUT.getArgsAndInitialize
  _window <- GLUT.createWindow "Hello World"
  pos <- DIOR.newIORef (0, 0)
  GLUT.keyboardMouseCallback GLUT.$= Just (keyboardMouse pos)
  GLUT.displayCallback GLUT.$= view Model {myPos = pos}
  GLUT.idleCallback GLUT.$= Just idle
  GLUT.mainLoop

view :: Model -> GLUT.DisplayCallback
view model = do
  GLUT.clear [ GLUT.ColorBuffer ]
  (x',y') <- GLUT.get (myPos model)
  GLUT.renderPrimitive GLUT.Points $
     mapM_ (\(x, y, z) -> GLUT.vertex $ GLUT.Vertex3 x y z) [(x',y',0)]
  GLUT.flush

data Model = Model { myPos :: DIOR.IORef (GLUT.GLfloat, GLUT.GLfloat)
                   } deriving (Eq)

keyboardMouse :: DIOR.IORef (GLUT.GLfloat, GLUT.GLfloat) -> GLUT.KeyboardMouseCallback
keyboardMouse pos key GLUT.Down _ _ = case key of
  (GLUT.SpecialKey GLUT.KeyLeft ) -> pos GLUT.$~! \(x,y) -> (x-0.1,y)
  (GLUT.SpecialKey GLUT.KeyRight) -> pos GLUT.$~! \(x,y) -> (x+0.1,y)
  (GLUT.SpecialKey GLUT.KeyUp   ) -> pos GLUT.$~! \(x,y) -> (x,y+0.1)
  (GLUT.SpecialKey GLUT.KeyDown ) -> pos GLUT.$~! \(x,y) -> (x,y-0.1)
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()

idle :: GLUT.IdleCallback
idle = GLUT.postRedisplay Nothing
