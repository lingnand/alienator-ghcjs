module Alienator.PhysicsSprite
  (
  ) where

import Reflex.Cocos2d.Prelude

import Alienator.Actuator

-- staticBodyDyn ::


-- generalized sprite that supports physical interaction within DynSpace
data PhysicsSpriteState ac ct = PhysicsSpriteState
                              { _physicsSpriteStateActuator :: ac
                              , _physicsSpriteStateCType    :: ct
                              , _physicsSpriteStateFixs     :: [Fixture]
                              , _physicsSpriteStateSprName  :: String
                              }
makeFields ''PhysicsSpriteState

-- def & shape .~ Segment 0 (-0.1) (0.1) -- just a point of contact
physicsSprite :: (NodeGraph t m, Enum a, IsActuator ac)
              => DynSpace t -- space
              -> Event t NominalDiffTime
              -> DynReader (PhysicsSpriteState Transform ct) t m (Event t ct) -- collision events
physicsSprite sp ticks fixs = do
    trDyn <- asksDyn (^.actuator)
    cTypeDyn <- asksDyn (^.cType)
    fixsDyn <- asksDyn (^.fixs)
    b <- staticBody sp [ dyn' fixs         := fixsDyn
                       , dyn transform     := trDyn
                       , dyn collisionType := cTypeDyn
                       ]
    sprNameDyn <- asksDyn (^.sprName)
    sprite_ [ dyn transform  := b^.transDyn
            , dyn spriteName := sprNameDyn
            ]
    return $ (^.otherCollisionType) <$> (b^.collisionBegan)

-- to actuate an bullet, do someting like this
-- flip runDynStateT (Mover {}) $ do
--   liftDynReader $ magnifyDyn (to $ \x -> BulletState (x^.moverPos) (dir $ x^.moverVel)) (bullet img collisionT sp)
--   modifyDyn $ moveTickReducer <$> tickE
