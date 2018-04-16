module OrderedMap where
import qualified Data.Map.Ordered as OMap
import qualified Data.Map as Map

type OrderedMap k v = (OMap.OMap k v)

empty :: (Ord k) => (OMap.OMap k v)
empty = OMap.empty

insert :: (Ord k) => k -> v -> (OMap.OMap k v) -> (OMap.OMap k v)
insert k v map =  (OMap.>|) map (k,v)

lookup :: (Ord k) => k -> (OMap.OMap k v) -> Maybe v
lookup k map = (OMap.lookup k map)

toList :: (Ord k) => (OMap.OMap k v) -> [(k,v)]
toList map = (OMap.assocs map)

fromList :: (Ord k) => [(k,v)] -> (OMap.OMap k v) 
fromList list = (OMap.fromList list)

member :: (Ord k) => k -> (OMap.OMap k v) -> Bool
member k map = (OMap.member k map)

size :: (Ord k) => (OMap.OMap k v) -> Int
size map = (OMap.size map)

keys :: (Ord k) => (OMap.OMap k v) -> [k]
keys m = (map (\e -> fst e) (toList m))

elems :: (Ord k) => (OMap.OMap k v) -> [v]
elems m = (map (\e -> snd e) (toList m))

union :: (Ord k) => (OMap.OMap k v) -> (OMap.OMap k v) -> (OMap.OMap k v)
union m1 m2 = let map1 = (toList m1)
                  map2 = (toList m2)
              in (OMap.fromList (map1 ++ map2))
