module Init where
import qualified Mod
import qualified Geometry as Geo
import qualified View
import qualified Data.IORef as DIOR
import qualified Controller as Ctrl

initInputs :: IO Ctrl.Inputs
initInputs = do
  _keysPressed <- DIOR.newIORef []
  return Ctrl.Inputs {Ctrl.keysPressed = _keysPressed}

initOutputs :: IO View.Outputs
initOutputs = do
  _outPoly <- DIOR.newIORef (View.polyToOutPoly Init.player1)
  _outEnPoly <- DIOR.newIORef (View.polyToOutPoly Init.enemy1)
  return View.Outputs {View.outPoly = _outPoly, View.outEnPoly = _outEnPoly}

initModel :: Mod.Model
initModel =
  Mod.Model { Mod.allKeysPressed = []
        , Mod.frame = 0
        , Mod.player = player1
        , Mod.speed = 0.1
        , Mod.enemy = enemy1
        }

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
