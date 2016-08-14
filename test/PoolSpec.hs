{-# LANGUAGE ScopedTypeVariables #-}
module PoolSpec (spec) where

import Alienator.Pool
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "empty" $ do
      prop "no elements" $ size empty == 0
    describe "changeNextIdle" $ do
      prop "auto inserting new element" $ \(x::String) n ->
        n < 0 || (elems $ iterate (snd . changeNextIdle x) empty !! n) == replicate n x
    describe "markIdle" $ do
      prop "element used and recycled goes back into pool" $ \(x::String) n ->
        n <= 0 || (elems $ iterate (uncurry markIdle . changeNextIdle x) empty !! n) == [x]

