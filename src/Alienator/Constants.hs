module Alienator.Constants
  (
    CollisionType(..)
  ) where

data CollisionType = Wall
                   | PlayerBullet
                   | EnemyBullet
                   | PlayerShip
                   | EnemyShip
   deriving (Enum, Show, Read, Eq)

