{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
-- | varieties of actuating components (to drive Transform)
module Alienator.Actuator
  (
    ConvertTransform(..)
  , VelActuator(VelActuator)
  , HasVel(..)
  , velActuatorTickReducer
  ) where

import Reflex.Cocos2d.Prelude

class ConvertTransform a where
    toTrans :: a -> Transform

-- positional change based on velocity
data VelActuator = VelActuator
                 { _velActuatorVel :: V2 Double
                 , _velActuatorPos :: P2 Double
                 }
makeFields ''VelActuator

velActuatorTickReducer :: NominalDiffTime -> VelActuator -> VelActuator
velActuatorTickReducer dt ac = ac & pos %~ (.+^ (realToFrac dt * ac^.vel))

instance ConvertTransform VelActuator where
    toTrans ac = Transform (ac^.pos) (ac^.vel @@ _Dir)
