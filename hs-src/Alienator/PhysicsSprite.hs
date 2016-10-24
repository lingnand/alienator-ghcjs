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

physicsSprite :: (NodeGraph t m, Eq ct, Enum ct, IsActuator ac)
              => DynSpace t ct -- space
              -> Event t NominalDiffTime
              -> DynStateT t (PhysicsSpriteState ac ct) m (Event t ct) -- collision events
physicsSprite sp ticks = do
    sDyn <- watch
    let enabledDyn = view enabled <$> sDyn
        enabledBeh = current enabledDyn
        fixsDyn = (\enabled -> if enabled then view fixs else const []) <$> enabledDyn <*> sDyn
    -- periodically update the actuator
    adjust $ over actuator . updateTick <$> gate enabledBeh ticks
    -- b <- staticBody sp [ uDyn' fixtures      := fixsDyn
    --                    , uDyn' transform     := views actuator toTrans <$> sDyn
    --                    , uDyn' collisionType := view cType <$> sDyn
    --                    ]
    sprite_ [ dyn' transform  := uniqDyn (views actuator toTrans <$> sDyn)
            , dyn' spriteName := uniqDyn (view sprName <$> sDyn)
            , dyn' visible    := uniqDyn enabledDyn
            ]
    -- return $ view otherCollisionType <$> b^.collisionBegan
    return never
