{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
-- | varieties of actuating components (to drive Transform)
module Alienator.Actuator
  (
    IsActuator(..)
  , AnyActuator(..)
  , VelActuator(VelActuator)
  , HasVel(..)
  , HasPos(..)
  , HasFollowRotation(..)
  , AccelActuator(AccelActuator)
  , HasAccel(..)
  , Anchor(Anchor)
  ) where

import Data.Typeable
import Reflex.Cocos2d.Prelude

class IsActuator a where
    toTrans :: a -> Transform
    updateTick :: NominalDiffTime -> a -> a

newtype Anchor = Anchor { _anchorPos :: P2 Double } deriving (Typeable, Show, Eq)
makeFields ''Anchor

instance IsActuator Anchor where
    toTrans (Anchor pos) = Transform pos xDir
    updateTick _ = id

instance Default Anchor where
    def = Anchor 0

-- wrapper datatype to provide an interface for IsActuator
data AnyActuator = forall a. (IsActuator a, Typeable a, Show a, Eq a) => AnyActuator a

instance Show AnyActuator where
    show (AnyActuator a) = "Actuator " ++ show a

instance Eq AnyActuator where
    AnyActuator a == AnyActuator b =
      case cast b of
        Just b' -> a == b'
        Nothing -> False

instance Default AnyActuator where
    def = AnyActuator (def :: Anchor)

instance IsActuator AnyActuator where
  toTrans (AnyActuator a) = toTrans a
  updateTick dt (AnyActuator a) = AnyActuator $ updateTick dt a

-- positional change based on velocity
data VelActuator = VelActuator
                 { _velActuatorVel :: V2 Double
                 , _velActuatorPos :: P2 Double
                 , _velActuatorFollowRotation :: Bool
                 } deriving (Show, Read, Eq, Typeable)
makeFields ''VelActuator

instance IsActuator VelActuator where
    toTrans ac = Transform (ac^.pos) rot
      where rot | ac^.followRotation = ac^.vel @@ _Dir
                | otherwise = xDir
    updateTick dt ac = ac & pos %~ (.+^ (realToFrac dt * ac^.vel))

instance Default VelActuator where
    def = VelActuator 0 0 False

data AccelActuator = AccelActuator
                   { _accelActuatorAccel :: V2 Double
                   , _accelActuatorVelActuator :: VelActuator
                   } deriving (Show, Read, Eq, Typeable)
makeFields ''AccelActuator

instance HasVel AccelActuator (V2 Double) where
    vel = velActuator . vel

instance HasPos AccelActuator (P2 Double) where
    pos = velActuator . pos

instance HasFollowRotation AccelActuator Bool where
    followRotation = velActuator . followRotation

instance IsActuator AccelActuator where
    toTrans = toTrans . _accelActuatorVelActuator
    updateTick dt ac = ac & velActuator %~ updateVelAct
      where updateVelAct = updateTick dt . (vel %~ (.+^ (realToFrac dt * ac^.accel)))

instance Default AccelActuator where
    def = AccelActuator 0 def

