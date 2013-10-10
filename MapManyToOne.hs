{-# OPTIONS -O2 -Wall #-}
module MapManyToOne
    (MapManyToOne,
     lookup, member, (!),
     keysFor, uniqueKeyFor,
     insert, delete,
     empty, singleton,
     fromList, toList,
     keys, backKeys)
where

import Prelude hiding (lookup)
import Control.Exception
import Data.Function(on)
import Data.Maybe(fromJust)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

data MapManyToOne k1 k2 = MapManyToOne { forward :: Map k1 k2,
                                         backward :: Map k2 (Set k1) }

instance (Ord k1, Ord k2, Show k1, Show k2) => Show (MapManyToOne k1 k2) where
    show = show . forward
instance (Ord k1, Ord k2) => Eq (MapManyToOne k1 k2) where
    (==) = (==) `on` forward
instance (Ord k1, Ord k2) => Ord (MapManyToOne k1 k2) where
    compare = compare `on` forward

lookup :: (Ord k1, Ord k2) => k1 -> MapManyToOne k1 k2 -> Maybe k2
k `lookup` m = k `Map.lookup` forward m

member :: (Ord k1, Ord k2) => k1 -> MapManyToOne k1 k2 -> Bool
k `member` m = k `Map.member` forward m

(!) :: (Ord k1, Ord k2) => MapManyToOne k1 k2 -> k1 -> k2
m ! k = forward m Map.! k

keysFor :: (Ord k1, Ord k2) => k2 -> MapManyToOne k1 k2 -> Set k1
keysFor k m = backward m Map.! k

uniqueKeyFor :: (Ord k1, Ord k2) => k2 -> MapManyToOne k1 k2 -> k1
uniqueKeyFor k = uniqueGet . keysFor k


modifyMap :: (Map k1 k2 -> Map k1' k2') ->
             (Map k2 (Set k1) -> Map k2' (Set k1')) ->
             MapManyToOne k1 k2 ->
             MapManyToOne k1' k2'
modifyMap modForward modBackward m =
  MapManyToOne ((modForward .  forward)  m)
               ((modBackward . backward) m)

insert :: (Ord k1, Ord k2) => k1 -> k2 -> MapManyToOne k1 k2 -> MapManyToOne k1 k2
insert k1 k2 = modifyMap (Map.insert k1 k2) (addToSetMap k1 k2)

delete :: (Ord k1, Ord k2) => k1 -> MapManyToOne k1 k2 -> MapManyToOne k1 k2
delete k m = modifyMap (Map.delete k) (delFromSetMap k (m ! k)) m

uniqueGet :: (Ord k) => Set k -> k
uniqueGet set = assert (1 == Set.size set) (head $ Set.toList set)

addToSetMap :: (Ord k1, Ord k2) => k1 -> k2 -> Map k2 (Set k1) -> Map k2 (Set k1)
addToSetMap k1 = Map.alter $ maybe (Just (Set.singleton k1)) (Just . Set.insert k1)

delFromSetMap :: (Ord k1, Ord k2) => k1 -> k2 -> Map k2 (Set k1) -> Map k2 (Set k1)
delFromSetMap k1 = Map.alter (delFromMaybeSet k1 . fromJust)
    where
      delFromMaybeSet k s = if Set.null newSet then Nothing
                              else Just newSet
        where
          newSet = Set.delete k s

empty :: (Ord k1, Ord k2) => MapManyToOne k1 k2
empty = MapManyToOne { forward = Map.empty, backward = Map.empty }

singleton :: (Ord k1, Ord k2) => k1 -> k2 -> MapManyToOne k1 k2
singleton k1 k2 = MapManyToOne { forward = Map.singleton k1 k2,
                                 backward = Map.singleton k2 . Set.singleton $ k1 }

makeBackwardMap :: (Ord k1, Ord k2) => [(k1, k2)] -> Map k2 (Set k1)
makeBackwardMap [] = Map.empty
makeBackwardMap ((k1, k2):xs) = addToSetMap k1 k2 (makeBackwardMap xs)

fromList :: (Ord k1, Ord k2) => [(k1, k2)] -> MapManyToOne k1 k2
fromList list = MapManyToOne { forward = (Map.fromList list),
                               backward = makeBackwardMap list }

toList :: (Ord k1, Ord k2) => MapManyToOne k1 k2 -> [(k1, k2)]
toList m = Map.toList (forward m)

keys :: (Ord k1, Ord k2) => MapManyToOne k1 k2 -> [k1]
keys m = Map.keys (forward m)

backKeys :: (Ord k1, Ord k2) => MapManyToOne k1 k2 -> [k2]
backKeys m = Map.keys (backward m)
