{-# LANGUAGE NamedFieldPuns #-}
-- A simple structure for a pool of resources
module Alienator.Pool
  (
    Pool
  , Id
  , fromList
  , empty
  , elems
  , size
  , changeNextIdle
  , markIdle
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

type Id = Int

data Pool a = Pool
           { _elems :: IM.IntMap a
           , _idleElems :: IS.IntSet
           } deriving (Show, Read)

-- initiate a pool with all initial elements idle
fromList :: [a] -> Pool a
fromList ls = Pool (IM.fromList (zip inds ls)) (IS.fromList inds)
  where inds = zipWith const [0..] ls

empty :: Pool a
empty = Pool IM.empty IS.empty

elems :: Pool a -> [a]
elems = IM.elems . _elems

size :: Pool a -> Int
size = IM.size . _elems

-- change the next idle element to the given state
-- if no idle element is available - insert a new element
changeNextIdle :: a -> Pool a -> (Id {- changed idle item id -}, Pool a)
changeNextIdle a p@Pool{ _elems, _idleElems }
  | IS.null _idleElems =
    let key = IM.size _elems
    in (key, p{ _elems = IM.insert key a _elems })
  | otherwise =
    let (key, idles') = IS.deleteFindMin _idleElems
    in (key, Pool{ _elems = IM.insert key a _elems, _idleElems = idles' })

markIdle :: Id -> Pool a -> Pool a
markIdle id p@Pool{ _elems, _idleElems }
  | IM.member id _elems = p{ _idleElems = IS.insert id _idleElems }
  | otherwise = p
