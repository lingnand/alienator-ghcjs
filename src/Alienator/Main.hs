module Alienator.Main
  (
    main
  ) where

import Reflex.Cocos2d.Prelude

import Alienator.Bullet
import Control.Monad

main :: IO ()
main = do
    setAdjustViewPort True
    setDesignResolutionSize 960 640 ShowAll
    setResizeWithBrowserSize True
    mainScene $ do
      tickE <- ticks
      sp <- space tickE []
      void $ flip runDynStateT (Mover (20^&20) (3^&3)) $ do
        liftDynReader $ magnifyDyn (to moverToBulletProps) (bullet "res/img/bullet.png" RoundBullet sp)
        modifyDyn $ moverTickReducer <$> tickE
