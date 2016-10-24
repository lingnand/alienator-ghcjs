{-# LANGUAGE ScopedTypeVariables #-}
module PoolSpec (spec) where

import Alienator.Pool
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "empty" $ do
      prop "no elements" $ size empty == 0
    describe "putNextIdle" $ do
      prop "auto inserting new element" $ \(x::String) n ->
        n < 0 || (elems $ iterate (putNextIdle x) empty !! n) == replicate n x
    describe "markIdle" $ do
      prop "element used and recycled goes back into pool" $ \(x::String) n ->
        n <= 0 || (elems $ iterate (uncurry markIdle . putNextIdle' x) empty !! n) == [x]

