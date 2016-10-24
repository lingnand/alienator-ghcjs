{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Alienator.GamePlayScene
  (
    GamePlaySceneState(GamePlaySceneState)

  , gamePlayScene
  ) where

import qualified Data.Set as S
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Free
import Reflex.Cocos2d.Prelude

import qualified Alienator.Pool as P
import Alienator.PhysicsSprite
import Alienator.Actuator
import Alienator.Constants

type PSpriteState' ac = PhysicsSpriteState ac CollisionType
type BulletState = PSpriteState' AnyActuator

data Meter a = Meter { _meterRead :: a , _meterMax :: a } deriving (Show, Read, Eq)
makeLenses ''Meter

data PlayerShipState = PlayerShipState
    { _health  :: Meter Int
    , _attack  :: Double
    , _defense :: Double
    , _pSprite :: PSpriteState' VelActuator
    } deriving (Show, Eq)
makeLenses ''PlayerShipState

-- the state definition of the GamePlay scene
data GamePlaySceneState = GamePlaySceneState
    { -- a set of bullets to be reused by the ships
      _bulletPool :: P.Pool BulletState
    , _playerShip :: PlayerShipState
    , _enemyShipPool :: P.Pool (PSpriteState' VelActuator)
    }
makeLenses ''GamePlaySceneState

initPlayerShipState :: V2 Double -> PlayerShipState
initPlayerShipState winSize = PlayerShipState
    { _health  = Meter 100 100
    , _attack  = 20
    , _defense = 30
    , _pSprite = def & actuator.pos .~ playerStartPos
                     & cType        .~ PlayerShip
                     & fixs         .~ [ def & shape .~ Poly (uncurry rect $ unr2 playerShipContour)
                                             & mass  .~ 5000
                                       ]
                     & sprName      .~ "res/img/player.png"
                     & enabled      .~ True
    }
  where playerStartPos = 0 .+^ (winSize & _x .~ 200
                                        & _y //~ 2)
        playerShipContour = 140^&40

initBulletPool :: P.Pool BulletState
initBulletPool = P.fromList $ replicate 25 def

-- initGamePlaySceneState :: V2 Double -- ^ Win size
--                        -> GamePlaySceneState
-- initGamePlaySceneState winSize =
--       GamePlaySceneState
--     { _bulletPool = P.fromList $ replicate 25 def
--     , _playerShip = PlayerShipState
--                   { _health  = Meter 100 100
--                   , _attack  = 20
--                   , _defense = 30
--                   , _pSprite = def & actuator.pos .~ playerStartPos
--                                    & cType        .~ PlayerShip
--                                    & fixs         .~ [ def & shape .~ Poly (uncurry rect $ unr2 playerShipContour)
--                                                            & mass  .~ 5000
--                                                      ]
--                                    & sprName      .~ "res/img/player.png"
--                                    & enabled      .~ True
--                   }
--     , _enemyShipPool = P.fromList $ replicate 10 def
--     }

-- | create a standard round bullet PSpriteState with the initial position, velocity and
-- acceleration
bullet :: CollisionType -> P2 Double -> V2 Double -> V2 Double -> BulletState
bullet ct p v acc = def & actuator .~ AnyActuator accelAct
                        & cType    .~ ct
                        & fixs     .~ [ def & shape .~ Circle 5 0
                                            & mass .~ 5
                                            & sensor .~ True
                                      ]
                        & sprName  .~ "res/img/bullet.png"
                        & enabled  .~ True
    where accelAct :: AccelActuator
          accelAct = def & pos .~ p
                         & vel .~ v
                         & accel .~ acc

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

-- | add movement into enemy
reactuateEnemyModifier :: MonadRandom m => Double -> m (VelActuator -> VelActuator)
reactuateEnemyModifier enemyBaseVel = do
    -- choose the angle
    ang <- getRandomR (90, 270)
    -- choose the velocity
    v <- getRandomR (enemyBaseVel, enemyBaseVel*2)
    return $ vel .~ (v *^ e (ang @@ deg))

gamePlayScene :: (NodeGraph t m)
              => V2 Double                -- ^ window size
              -> DynSpace t CollisionType -- ^ physics space
              -> Dynamic t (S.Set Key)    -- ^ keys down
              -> Event t NominalDiffTime  -- ^ ticks
              -> m (Event t ()) -- ^ return when game is over
gamePlayScene winSize sp keysDyn ticks = fmap snd $ node [] <-< do
    let bulletBaseAccel = 10.0
        playerBaseVel = 50.0
        bulletPosOffset = 100.0
        -- enemyBaseVel = playerBaseVel*1.5
        -- enemyShipContour = 130^&80

    (_, finished) <- lift . load $ [ "res/img/enemy" ++ show i ++ ".png"
                                   | i <- [0..3] :: [Int]
                                   ]
                                   ++
                                   [ "res/img/bullet.png"
                                   , "res/img/player.png" ]
    liftF finished

    -- fps ticks
    [ fpsD8, fps1, fps2, fps5 ] <- lift $ mapM (flip dilate ticks) [0.8, 1, 2, 5]

    -- render player ship
    playerDyn <- lift $ flip execAccStateT (initPlayerShipState winSize) $ do
      playerHits <- focus pSprite $ do
        hits <- physicsSprite sp ticks
        adjust $ ffor (updated keysDyn) $
          \ks -> actuator.vel .~ playerBaseVel *^ (foldr ((+) . keyToUnitV) 0 ks)
        return hits

      let healthHits = ffor playerHits $ \case
            EnemyShip -> 30
            EnemyBullet -> 10
            _ -> 0
      adjust $ (health.meterRead -~) <$> healthHits

    -- player status
    lift $ label [ dyn text  := (\(Meter r m) -> show r ++ " / " ++ show m) . (^.health) <$> playerDyn
                 , fontSize  := 20
                 , fontColor := white
                 , position  := 200^&100
                 ]

    -- render the enemy ships
    -- bulletPoolModEE <- focus enemyShipPool $ do
    --   let -- randomly add movement into the enemy
    --       maybeReactuateModifier :: MonadRandom m => m (Maybe (VelActuator -> VelActuator))
    --       maybeReactuateModifier = runMaybeT $ do
    --           -- first randomly select whether we should reactuate in the first place
    --           MaybeT $ pick [ (0.4, Just ())
    --                         , (0.6, Nothing)
    --                         ]
    --           lift $ reactuateEnemyModifier enemyBaseVel
    --       maybeAttackModifier :: MonadRandom m => Int -> VelActuator -> m (Maybe [BulletState])
    --       maybeAttackModifier enemyType act = pick [ (0.8, Just bs)
    --                                                , (0.2, Nothing)
    --                                                ]
    --         where spawnP = act^.pos - bulletPosOffset^&0
    --               bs = [ bullet EnemyBullet (spawnP & _y +~ fromIntegral offset) ((norm $ act^.vel) *^ (-3^&0)) 0
    --                    | x <- [0..enemyType-1]
    --                    , let offset = ((x+1) `div` 2) * 30 * (if odd x then 1 else -1)
    --                    ]
    --
    --   -- rendering
    --   enemyModsE <- P.buildDiffs $ \pid -> do
    --     hits <- physicsSprite sp ticks
    --     let resetHits = ffilter (`elem` [PlayerBullet, Wall]) hits
    --     adjust $ (enabled .~ False) <$ resetHits
    --     -- behaviors
    --     enemyBeh <- refine current watch
    --     let enabledBeh = (^.enabled) <$> enemyBeh
    --         actBeh = (^.actuator) <$> enemyBeh
    --     -- random reactuation
    --     maybeModE <- runRandEvent $ maybeReactuateModifier <$ gate enabledBeh fps2
    --     adjust $ fmapMaybe ((actuator %~) <$>) maybeModE
    --     -- random firing of bullets
    --     -- TODO: use proper enemyType
    --     bulletT <- liftIO . evalRandIO $ getRandomR (1, 3)
    --     maybeBullets <- runRandEvent $ attachWith (const . maybeAttackModifier bulletT) actBeh $ gate enabledBeh fps1
    --     return (P.markIdle pid <$ resetHits, P.putNextIdles <$> fmapMaybe id maybeBullets)
    --
    --   let (resetModEE, bulletPoolModEE) = splitE $ ffor enemyModsE $ \mods ->
    --                                           let (as, bs) = unzip mods
    --                                               f = mergeWith (.)
    --                                           in (f as, f bs)
    --   buildEvent_ $ adjust <$> resetModEE
    --
    --   -- generate enemies
    --   randSpawnEnemiesE :: Event t (PSpriteState' VelActuator -> PSpriteState' VelActuator)
    --     <- runRandEvent . ffor fps5 . const $ do
    --       rv2 <- getRandomV2 (winSize & _x *~ 0.7 & _y *~ 0.25, winSize & _x *~ 0.9 & _y *~ 0.75)
    --       i :: Int <- getRandomR (0, 3) -- the enemy type
    --       modifier <- reactuateEnemyModifier enemyBaseVel
    --       return $ (actuator .~ (def & pos .~ 0 .+^ rv2 & modifier))
    --              . (cType    .~ EnemyShip)
    --              . (fixs     .~ [ def & shape .~ Poly (uncurry rect $ unr2 enemyShipContour)
    --                                   & mass  .~ 6000
    --                             ])
    --              . (sprName  .~ "res/img/enemy" ++ show i ++ ".png")
    --              . (enabled  .~ True)
    --   adjust $ P.modifyIdle <$> randSpawnEnemiesE
    --
    --   return bulletPoolModEE

    -- render bullets
    void . lift $ flip runAccStateT initBulletPool $ do
      resetModsE <- P.buildDiffs $ \pid -> do
        hits <- physicsSprite sp ticks
        -- onEvent_ hits $ \ct -> liftIO . putStrLn $ show id ++ ": Got hit with " ++ show ct
        -- once hit the wall/ship/etc., anchor them and deactivate
        cTypeBeh <- refine current $ watches (^.cType)
        let resetHits = attachWithMaybe (\x y -> guard $ bulletShouldReset x y) cTypeBeh hits
        adjust $ (enabled .~ False) <$ resetHits
        return $ P.markIdle pid <$ resetHits

      buildEvent_ $ adjust . mergeWith (.) <$> resetModsE

      -- generate bullets
      let onBulletFreq ks _ | S.member Space ks = Just ()
                            | otherwise = Nothing
          playerBulletFireE = attachPromptlyDynWithMaybe onBulletFreq keysDyn fpsD8
          playerBulletsGen p =
            [ bullet PlayerBullet (p .+^ pOffset) v ac
            | ang <- [30, 0, -30]
            , let uv = e (ang @@ deg)
                  pOffset = bulletPosOffset *^ uv
                  v = (playerBaseVel*5) *^ uv
                  ac = bulletBaseAccel *^ uv
            ]
          playerPosBeh = (^.pSprite.actuator.pos) <$> current playerDyn
      adjust $ P.putNextIdles . playerBulletsGen <$> tag playerPosBeh playerBulletFireE

      -- generate enemy bullets
      -- buildEvent_ $ adjust <$> bulletPoolModEE

    liftF $ void $ ffilter (<=0) $ (^.health.meterRead) <$> updated playerDyn
