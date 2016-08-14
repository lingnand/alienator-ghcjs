{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Alienator.GamePlayScene
  (
    GamePlaySceneState(GamePlaySceneState)

  , initGamePlaySceneState
  , gamePlayScene
  ) where

import qualified Data.Set as S
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Reflex.Cocos2d.Prelude

import qualified Alienator.Pool as P
import Alienator.PhysicsSprite
import Alienator.Actuator
import Alienator.Constants

type PSpriteState' ac = PhysicsSpriteState ac CollisionType

data Meter a = Meter { _meterRead :: a , _meterMax :: a } deriving (Show, Read, Eq)
makeLenses ''Meter

data PlayerShipState = PlayerShipState
    { _health  :: Meter Double
    , _attack  :: Double
    , _defense :: Double
    , _pSprite :: PSpriteState' VelActuator
    } deriving (Show, Eq)
makeLenses ''PlayerShipState

-- the state definition of the GamePlay scene
data GamePlaySceneState = GamePlaySceneState
    { -- a set of bullets to be reused by the ships
      _bulletPool :: P.Pool (PSpriteState' AnyActuator)
    , _playerShip :: PlayerShipState
    , _enemyShipPool :: P.Pool (PSpriteState' VelActuator)
    }
makeLenses ''GamePlaySceneState

initGamePlaySceneState :: V2 Double -- ^ Win size
                       -> GamePlaySceneState
initGamePlaySceneState winSize =
      GamePlaySceneState
    { _bulletPool = P.fromList $ replicate 25 def
    , _playerShip = PlayerShipState
                  { _health  = Meter 100 100
                  , _attack  = 20
                  , _defense = 30
                  , _pSprite = def & actuator.pos .~ playerStartPos
                                   & cType        .~ PlayerShip
                                   & fixs         .~ [ def & shape .~ Poly (uncurry rect $ unr2 playerShipContour)
                                                           & mass  .~ 5000
                                                     ]
                                   & sprName      .~ "res/img/player.png"
                  }
    , _enemyShipPool = P.fromList $ replicate 10 def
    }
  where playerStartPos = 0 .+^ (winSize & _x .~ 200
                                        & _y //~ 2)
        playerShipContour = 140^&40

-- | create a standard round bullet PSpriteState with the initial position, velocity and
-- acceleration
bullet :: CollisionType -> P2 Double -> V2 Double -> V2 Double -> PSpriteState' AnyActuator
bullet ct p v acc = def & actuator .~ AnyActuator accelAct
                        & cType    .~ ct
                        & fixs     .~ [ def & shape .~ Circle 5 0
                                            & mass .~ 5
                                            & sensor .~ True
                                      ]
                        & sprName  .~ "res/img/bullet.png"
    where accelAct :: AccelActuator
          accelAct = def & pos .~ p
                         & vel .~ v
                         & accel .~ acc

fireBulletsModifier :: (GamePlaySceneState -> [PSpriteState' AnyActuator]) -- ^ generate a set of bullets given the curr state
                    -> GamePlaySceneState
                    -> GamePlaySceneState
fireBulletsModifier bulletsGen gpState = gpState & bulletPool %~ (foldr P.putNextIdle ?? (bulletsGen gpState))

bulletShouldReset :: CollisionType -- ^ bullet collisionType
                  -> CollisionType -- ^ other collisionType
                  -> Bool
bulletShouldReset PlayerBullet PlayerShip = False
bulletShouldReset EnemyBullet EnemyShip = False
bulletShouldReset _ _ = True

keyToUnitV :: Key -> V2 Double
keyToUnitV ArrowUp = V2 0 1
keyToUnitV ArrowDown = V2 0 (-1)
keyToUnitV ArrowLeft = V2 (-1) 0
keyToUnitV ArrowRight = V2 1 0
keyToUnitV _ = 0

getRandomV2 :: (Random a, MonadRandom m) => (V2 a, V2 a) -> m (V2 a)
getRandomV2 (V2 fx fy, V2 tx ty) = do
    x <- getRandomR (fx, tx)
    y <- getRandomR (fy, ty)
    return $ x^&y

gamePlayScene :: (NodeGraph t m, RandomGen g)
              => g                       -- ^ Random Generator
              -> V2 Double               -- ^ window size
              -> DynSpace t              -- ^ physics space
              -> Dynamic t (S.Set Key)   -- ^ keys down
              -> Event t NominalDiffTime -- ^ ticks
              -> DynStateT GamePlaySceneState t m ()
gamePlayScene g winSize sp keysDyn ticks = do
    let bulletAnchorPos = 0.0 .+^ 2*winSize
        bulletBaseAccel = 10.0
        playerBaseVel = 50.0
        playerBulletPosOffset = 100.0
        enemyBaseVel = playerBaseVel*2
        enemyShipContour = 130^&80

    -- render bullets
    zoomDyn bulletPool $ P.buildPoolDiff_ $ \pid ps -> do
      hits <- zoomDyn (at pid . pnon ps) $ do
        hits <- physicsSprite sp ticks
        -- onEvent_ hits $ \ct -> liftIO . putStrLn $ show id ++ ": Got hit with " ++ show ct
        -- once hit the wall/ship/etc., anchor them and deactivate
        cTypeBeh <- asksBehavior (^.cType)
        let resetHits = attachWithMaybe (\x y -> guard $ bulletShouldReset x y) cTypeBeh hits
        modifyDyn $ (actuator .~ AnyActuator (Anchor bulletAnchorPos)) <$ resetHits
        return resetHits
      modifyDyn $ P.markIdle pid <$ hits

    -- render player ship
    zoomDyn playerShip $ do
      zoomDyn pSprite $ do
        hits <- physicsSprite sp ticks
        onEvent hits $ \case
          EnemyBullet -> liftIO $ putStrLn "Hit by bullet!"
          _ -> return ()
        modifyDyn $ ffor (updated keysDyn) $
          \ks -> actuator.vel .~ playerBaseVel *^ (foldr ((+) . keyToUnitV) 0 ks)

    -- render the enemy ships
    zoomDyn enemyShipPool $ do
      enemyReactuateFreqE <- dilate 2 ticks
      P.buildPoolDiff_ $ \pid ps -> do
        hits <- zoomDyn (at pid . pnon ps) $ do
          hits <- physicsSprite sp ticks
          let resetHits = ffilter (`elem` [PlayerBullet, Wall]) hits
          onEvent_ resetHits . const . liftIO $ putStrLn $ show pid ++ ": enemy reset"
          modifyDyn $ (actuator .~ (def & pos .~ bulletAnchorPos)) <$ resetHits
          -- randomly add movement into the enemy
          maybeVelsE <- runRandEvent . ffor enemyReactuateFreqE . const $ runMaybeT $ do
            -- first randomly select whether we should reactuate in the first place
            MaybeT $ pick [ (0.4, Just ())
                          , (0.6, Nothing)
                          ]
            -- choose the angle
            ang <- lift $ getRandomR (90, 270)
            -- choose the velocity
            vel <- lift $ getRandomR (0.0, enemyBaseVel)
            return $ vel *^ e (ang @@ deg)
          modifyDyn $ (actuator.vel .~) <$> fmapMaybe id maybeVelsE
          return resetHits
        modifyDyn $ P.markIdle pid <$ hits

      -- generate enemies
      enemySpawnFreqE <- dilate 5 ticks
      randSpawnEnemiesE :: Event t (PSpriteState' VelActuator -> PSpriteState' VelActuator)
        <- runRandEvent . ffor enemySpawnFreqE . const $ do
          rv2 <- getRandomV2 (winSize & _x *~ 0.75 & _y *~ 0.25, winSize & _y *~ 0.75)
          i :: Int <- getRandomR (0, 3) -- the enemy type
          return $ (actuator .~ (def & pos .~ 0 .+^ rv2))
                 . (cType    .~ EnemyShip)
                 . (fixs     .~ [ def & shape .~ Poly (uncurry rect $ unr2 enemyShipContour)
                                      & mass  .~ 6000
                                ])
                 . (sprName  .~ "res/img/enemy" ++ show i ++ ".png")
      modifyDyn $ P.modifyIdle <$> randSpawnEnemiesE

    -- generate bullets
    bulletFreqE <- dilate 0.8 ticks
    let onBulletFreq ks _ | S.member Space ks = Just ()
                          | otherwise = Nothing
        bulletFireE = attachDynWithMaybe onBulletFreq keysDyn bulletFreqE
        playerBulletsGen gpState =
          [ bullet PlayerBullet (gpState^.playerShip.pSprite.actuator.pos .+^ pOffset) v ac
          | ang <- [30, 0, -30]
          , let uv = e (ang @@ deg)
                pOffset = playerBulletPosOffset *^ uv
                v = (playerBaseVel*2.5) *^ uv
                ac = bulletBaseAccel *^ uv
          ]
    modifyDyn $ fireBulletsModifier playerBulletsGen <$ bulletFireE
