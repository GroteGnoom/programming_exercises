module View where

import qualified Data.IORef as DIOR
import qualified Graphics.UI.GLUT as GLUT
import qualified Mod
import qualified Geometry as Geo

data Outputs = Outputs { outPoly :: DIOR.IORef [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)]
                       , outEnPoly :: DIOR.IORef [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)]}


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

updateOutput :: Mod.Model -> Outputs -> IO()
updateOutput model output = do
  DIOR.writeIORef (outPoly output) (polyToOutPoly (Mod.player model))
  return ()

fToGL :: Float -> GLUT.GLfloat
fToGL = realToFrac :: Float -> GLUT.GLfloat

posToOutPos :: Geo.Vec -> (GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)
posToOutPos (Geo.Vec a b) = (fToGL a, fToGL b, 0)

polyToOutPoly :: Geo.Poly -> [(GLUT.GLfloat, GLUT.GLfloat, GLUT.GLfloat)]
polyToOutPoly (Geo.Poly vecList) =
  map posToOutPos vecList
