{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Alienator.Main
  (
    main
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Reflex.Cocos2d.Prelude

import Alienator.GamePlayScene
import Alienator.Constants


data Scene = StartScene | GamePlayScene | GameOverScene deriving (Show, Read, Eq)

main :: IO ()
main = do
    setAdjustViewPort True
    setDesignResolutionSize 960 640 ShowAll
    setResizeWithBrowserSize True
    winSize <- getWinSize
    mainScene $ do
      tickE <- ticks
      uiE <- uiEvents
      keysDyn <- dynKeysDown (uiE^.keyPressed) (uiE^.keyReleased)
      sp <- space tickE [ iterations := 2 ]
      -- walls
      staticBody sp [ fixtures      := [
                        -- thick wall
                        def & shape .~ Segment 100 a b
                      | -- add some padding to the window size
                        let rectPts = uncurry rect $ unr2 (winSize + 100)
                      , (a, b) <- zip rectPts (tail $ cycle rectPts)
                      ]
                    , position      := 0 .+^ winSize/2
                    , collisionType := Wall
                    ]
      let halfsize = winSize/2
          center = 0 .+^ halfsize
      void $ runAccStateT ?? StartScene $ do
        vDyn <- watches $ \case
          StartScene -> do
            l <- label [ position := center
                       , text     := "Start"
                       , fontSize := 20
                       ]
            sz <- get l size
            liftIO $ putStrLn $ "got size of label " ++ show sz
            let bbox = fromPoints $ uncurry rect (unr2 sz) # translate halfsize
                touched = ffilter (contains bbox . (^.loc)) (uiE^.touchBegan)
            adjust $ const GamePlayScene <$ touched
          GamePlayScene -> do
            overE <- gamePlayScene winSize sp keysDyn tickE
            adjust $ const GameOverScene <$ overE
          GameOverScene -> do
            l <- label [ text     := "Game Over"
                       , fontSize := 50
                       , position := 0 .+^ winSize/2
                       ]
            sz <- get l size
            let bbox = fromPoints $ uncurry rect (unr2 sz) # translate halfsize
                touched = ffilter (contains bbox . (^.loc)) (uiE^.touchBegan)
            adjust $ const StartScene <$ touched
        node [] -<< vDyn
