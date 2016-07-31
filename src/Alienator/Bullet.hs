{-# LANGUAGE TemplateHaskell #-}
-- | Generic bullet builders
module Alienator.Bullet
  (
    Mover(Mover)
  , moverVel
  , moverPos
  , moverTickReducer
  , BulletProps
  , moverToBulletProps
  , bullet
  ) where

import Reflex
import Reflex.Cocos2d.Prelude

bullet :: (NodeGraph t m, Enum a)
       => DynSpace t -- space
       -> DynReaderT BulletProps t m (Event t a) -- collision events
bullet img collisionT sp = do
    posDyn <- asksDyn (^.bulletPos)
    rotDyn <- asksDyn (^.bulletRot)
    b <- staticBody sp [ def & shape .~ Segment 0 (-0.1) (0.1) -- just a point of contact
                       ]
                       [ dyn pos       := posDyn
                       , dyn rot       := rotDyn
                       , collisionType := collisionT
                       ]
    sprite_ [ dyn (divided pos rot) := b^.transDyn
            , spriteName            := img
            ]
    return $ (^.otherCollisionType) <$> (b^.collisionBegan)

-- to actuate an bullet, do someting like this
-- flip runDynStateT (Mover {}) $ do
--   liftDynReader $ magnifyDyn (to $ \x -> BulletState (x^.moverPos) (dir $ x^.moverVel)) (bullet img collisionT sp)
--   modifyDyn $ moveTickReducer <$> tickE
