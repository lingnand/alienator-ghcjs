module GamePlayScene
  (

  ) where

import qualified Alienator.Pool as P

import Alienator.PhysicsSprite
import Alienator.Actuator
import Alienator.Constants

-- the state definition of the GamePlay scene
data GamePlaySceneState = GamePlaySceneState
    { -- a set of bullets to be reused by the ships
      _bulletPool :: P.Pool (PhysicsSpriteState VelActuator CollisionType)
    -- , _shipPool ::
    }
