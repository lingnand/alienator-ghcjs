{-# LANGUAGE ScopedTypeVariables #-}
module Alienator.Main
  (
    main
  ) where

import Control.Monad
import Reflex.Cocos2d.Prelude

import Alienator.GamePlayScene
import Alienator.Constants


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
                        def & shape .~ Segment 0 a b
                      | let rectPts = uncurry rect $ unr2 winSize
                      , (a, b) <- zip rectPts (tail $ cycle rectPts)
                      ]
                    , position      := 0 .+^ winSize/2
                    , collisionType := Wall
                    ]
      void $ flip runDynStateT (initGamePlaySceneState winSize) $ do
          gamePlayScene winSize sp keysDyn tickE
