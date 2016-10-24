{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
-- A simple structure for a pool of resources
module Alienator.Pool
  (
    Pool
  , Id
  , fromList
  , empty
  , elems
  , idleIds
  , null
  , size
  , difference
  , foldrWithId
  , putNextIdle
  , putNextIdle'
  , putNextIdles
  , modifyIdle
  , markIdle
  , traverseWithId
  , traverseWithId_
  , buildDiffs
  ) where

import Prelude hiding (null)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Reflex.Cocos2d.Prelude as P

type Id = Int

data Pool a = Pool
           { _elems :: IM.IntMap a
           , _idleElems :: IS.IntSet
           } deriving (Show, Read, Eq)

P.makeLensesFor [("_elems", "elemsLens")] ''Pool

-- Lens for the mix
type instance P.Index (Pool a) = Id
type instance P.IxValue (Pool a) = a
instance P.Ixed (Pool a) where
    ix k = elemsLens . P.ix k

instance P.At (Pool a) where
    at k = elemsLens . P.at k

-- initiate a pool with all initial elements idle
fromList :: [a] -> Pool a
fromList ls = Pool (IM.fromList (zip inds ls)) (IS.fromList inds)
  where inds = zipWith const [0..] ls

empty :: Pool a
empty = Pool IM.empty IS.empty

elems :: Pool a -> [a]
elems = IM.elems . _elems

idleIds :: Pool a -> [Id]
idleIds = IS.elems . _idleElems

null :: Pool a -> Bool
null = IM.null . _elems

size :: Pool a -> Int
size = IM.size . _elems

difference :: Pool a -> Pool a -> Pool a
difference (Pool aElems aIdles) (Pool bElems _) = Pool (IM.difference aElems bElems)
                                                       (IS.difference aIdles (IM.keysSet bElems))

-- fold the ids and items in the pool
foldrWithId :: (Id -> a -> b -> b) -> b -> Pool a -> b
foldrWithId f b = IM.foldrWithKey f b . _elems

putNextIdle :: a -> Pool a -> Pool a
putNextIdle a = snd . putNextIdle' a

-- change the next idle element to the given state
-- if no idle element is available - insert a new element
putNextIdle' :: a -> Pool a -> (Id, Pool a)
putNextIdle' a p@Pool{ _elems, _idleElems }
  | IS.null _idleElems =
    let key = IM.size _elems
    in (key, p{ _elems = IM.insert key a _elems })
  | otherwise =
    let (key, idles') = IS.deleteFindMin _idleElems
    in (key, Pool{ _elems = IM.insert key a _elems, _idleElems = idles' })

putNextIdles :: [a] -> Pool a -> Pool a
putNextIdles = flip $ foldr putNextIdle

modifyIdle :: (a -> a) -> Pool a -> Pool a
modifyIdle f p@Pool{ _elems, _idleElems }
  | IS.null _idleElems = p
  | otherwise =
    let (key, idles') = IS.deleteFindMin _idleElems
    in Pool{ _elems = IM.adjust f key _elems, _idleElems = idles' }

markIdle :: Id -> Pool a -> Pool a
markIdle id p@Pool{ _elems, _idleElems }
  | IM.member id _elems = p{ _idleElems = IS.insert id _idleElems }
  | otherwise = p

traverseWithId :: Applicative f => (Id -> a -> f b) -> Pool a -> f [b]
traverseWithId f = foldrWithId (\pid a acc -> (:) <$> f pid a <*> acc) (pure [])

traverseWithId_ :: Applicative f => (Id -> a -> f ()) -> Pool a -> f ()
traverseWithId_ f = foldrWithId (\pid a acc -> f pid a *> acc) (pure ())

buildDiffs :: P.NodeGraph t m => (Id -> P.DynStateT t a m b) -> P.DynStateT t (Pool a) m (P.Event t [b])
buildDiffs onDiff = do
    let f prev curr | diff <- difference curr prev
                    , not (null diff) = (Just curr, Just diff)
                    | otherwise       = (Just curr, Nothing)
    diffE <- P.mapAccumMaybe_ f empty =<< P.pushPostBuild =<< P.watch
    P.buildEvent . P.ffor diffE . traverseWithId $ \pid ps -> do
      P.focus (P.at pid . P.pnon ps) $ onDiff pid
