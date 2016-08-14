{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Alienator.PhysicsSprite
  (
    PhysicsSpriteState(PhysicsSpriteState)
  , HasActuator(..)
  , HasCType(..)
  , HasFixs(..)
  , HasSprName(..)
  , HasEnabled(..)
  , physicsSprite
  ) where

import Reflex.Cocos2d.Prelude

import Alienator.Actuator

-- generalized sprite that supports physical interaction within DynSpace
data PhysicsSpriteState ac ct = PhysicsSpriteState
                              { _physicsSpriteStateActuator :: ac
                              , _physicsSpriteStateCType    :: ct
                              , _physicsSpriteStateFixs     :: [Fixture]
                              , _physicsSpriteStateSprName  :: String
                              , _physicsSpriteStateEnabled  :: Bool
                              } deriving (Show, Eq)
makeFields ''PhysicsSpriteState

instance (Enum ct, Default ac) => Default (PhysicsSpriteState ac ct) where
    -- a default that doesn't quite make sense...
    def = PhysicsSpriteState
        { _physicsSpriteStateActuator = def
        , _physicsSpriteStateCType    = toEnum 0
        , _physicsSpriteStateFixs     = []
        , _physicsSpriteStateSprName  = ""
        , _physicsSpriteStateEnabled  = False
        }

-- def & shape .~ Segment 0 (-0.1) (0.1) -- just a point of contact
physicsSprite :: (NodeGraph t m, Eq ct, Enum ct, IsActuator ac)
              => DynSpace t -- space
              -> Event t NominalDiffTime
              -> DynStateT (PhysicsSpriteState ac ct) t m (Event t ct) -- collision events
physicsSprite sp ticks = do
    trDyn <- asksNubDyn (toTrans . (^.actuator))
    cTypeDyn <- asksNubDyn (^.cType)
    fixsDyn <- asksNubDyn (^.fixs)
    enabledDyn <- asksNubDyn (^.enabled)
    fixsDyn' <- combineDyn (\enabled fixs -> if enabled then fixs else []) enabledDyn fixsDyn
    -- periodically update the actuator
    modifyDyn $ (actuator %~) . updateTick <$> (gate (current enabledDyn) ticks)
    b <- staticBody sp [ dyn' fixtures      := fixsDyn'
                       , dyn' transform     := trDyn
                       , dyn' collisionType := cTypeDyn
                       ]
    sprNameDyn <- asksDyn (^.sprName)
    sprite_ [ dyn transform  := b^.transDyn
            , dyn spriteName := sprNameDyn
            , dyn visible    := enabledDyn
            ]
    return $ (^.otherCollisionType) <$> (b^.collisionBegan)
