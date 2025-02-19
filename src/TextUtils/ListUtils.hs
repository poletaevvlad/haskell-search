module TextUtils.ListUtils(minTermDistance, minQueryDistance, pairs, sortByKey) where
import Data.List(sort)


posDistances :: [Int] -> [Int] -> [Int]
posDistances [] _ = []
posDistances _ [] = []
posDistances (a:as) (b:bs)
 | a < b     = (b - a):(posDistances as (b:bs))
 | otherwise = (a - b):(posDistances (a:as) bs)


minTermDistance :: [Int] -> [Int] -> Int
minTermDistance pos1 pos2 = minimum $ posDistances pos1 pos2


getPositions :: (Eq a) => [a] -> a -> [Int]
getPositions list val = reverse $ fst $ foldl (\(l, i) a -> (if a == val then i:l else l, i + 1)) ([], 0) list


minQueryDistance :: (Eq a) => [a] -> a -> a -> Int
minQueryDistance query t1 t2 = let l1 = getPositions query t1
                                   l2 = getPositions query t2
                                in minTermDistance l1 l2


pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = (zip (repeat x) xs) ++ pairs xs


data (Ord b) => Sortable b a = Sortable b a

instance (Ord b) => Eq (Sortable b a) where
  (Sortable x _) == (Sortable y _) = x == y

instance (Ord b) => Ord (Sortable b a) where
  compare (Sortable x _) (Sortable y _) = compare x y

sortByKey :: (Ord b) => (a -> b) -> [a] -> [a]
sortByKey p list = map (\(Sortable _ x) -> x) $ sort $ map (\x -> Sortable (p x) x) list
