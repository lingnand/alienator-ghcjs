import Reflex
import Reflex.Cocos2d.Prelude hiding (Box)

main = do
    os <- getOS
    putStrLn $ "OS: " ++ show os
    case os of
      Just IOS -> setEnableRetina True
      _ -> return ()
    setAdjustViewPort True
    setDesignResolutionSize 960 640 ShowAll
    setResizeWithBrowserSize True
    winSize <- getWinSize
    gen <- createSystemRandom
    mainScene $ do
      evts <- uiEvents
      dks <- dynKeysDown (evts^.keyPressed) (evts^.keyReleased)
      ts <- ticks
      -- fps10 <- dilate (1/10) ts
      drags <- dragged (evts^.singleTouchEvents)
      void $ layerColor (def & color .~ constDyn blueviolet) -<< do
        [lTouched, rTouched] <- forM [ (300, yellow)
                                     , (500, brown) ] $ \(x, c) -> lift $ do
            l <- layerColor $ def & pos .~ constDyn (x ^& (winSize^._y/2))
                                  & size .~ constDyn (pure 100)
                                  & color .~ constDyn c
            filterG ?? (evts^.touchBegan) $ nodeContains l . (^.loc)
        wrap $ leftmost [ P.platformer winSize dks ts <$ lTouched
                        , boxThrower gen winSize drags ts <$ rTouched
                        ]
