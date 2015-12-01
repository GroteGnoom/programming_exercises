module Mod where
import qualified Geometry as Geo
import qualified Graphics.UI.GLUT as GLUT
import qualified Safe

data Model = Model { allKeysPressed :: [(Int, [GLUT.Key])]
                   , frame :: Int
                   , player :: Geo.Poly
                   , speed :: Float
                   , enemy :: Geo.Poly
                   }

update :: Model -> Model
update model =
  updateEnemy (updatePlayer model)

keyToVec :: GLUT.Key -> Geo.Vec
keyToVec key = case key of
  (GLUT.SpecialKey GLUT.KeyLeft ) -> Geo.Vec (-1) 0
  (GLUT.SpecialKey GLUT.KeyRight) -> Geo.Vec 1 0
  (GLUT.SpecialKey GLUT.KeyUp   ) -> Geo.Vec 0 1
  (GLUT.SpecialKey GLUT.KeyDown ) -> Geo.Vec 0 (-1)
  _ -> Geo.Vec 0 0

updatePlayer :: Model -> Model
updatePlayer model =
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

updateEnemy :: Model -> Model
updateEnemy model =
  Model { allKeysPressed = allKeysPressed model
             , frame = frame model
             , player = player model
             , speed = speed model
             , enemy = Geo.move (enemy model) (Geo.Vec 0.01 0.01)}
