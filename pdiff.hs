import Data.HashSet as HashSet
import Data.HashMap as HashMap
import Data.Hashable
import Data.Text as T
import Data.List as List

uniqs :: (Ord a, Hashable a) => [a] -> Set a
uniqs = uniqs' HashSet.empty
	where 
		uniqs' hash (x:xs) = if HashSet.member x hash
			then
				uniqs' (HashSet.delete x hash) xs
			else
				uniqs' (HashSet.insert x hash) xs
		uniqs' hash _ = hash


mapFilter :: ( a -> b -> (a,Maybe c) ) -> a -> [b] -> [c]
mapFilter l seed (x:xs) = case (l seed x) of
		(next, Just y) -> y : mapFilter l next xs
		(next, Nothing) -> mapFilter l next xs							
mapFilter _ _ _ = []

uniqIndices xs unq = findIndices (\x -> HashSet.member x unq) xs

uniqMap xs unique = HashMap.fromList $ List.filter (\(x,y) -> HashSet.member y unique) $ List.zip [1..] xs
uniqCommon xs ys = HashSet.intersection (uniqs xs) (uniqs ys)

blah xs ys =
	where
		common = uniqCommon xs ys
		uniqxmap = uniqMap xs common
		uniqymap = uniqMap ys common
		pairs = 

patience :: [a] -> [(Maybe a, Maybe a)]
patience xs = patience' xs [] Nothing
	where
		patience' (x:xs) [] past = [(x, past)]
		patience' (x:xs) (l:ls) past = if x < l 
			then 
				patience' xs 

pdiff (x:xs) (y:ys) = startmatch ++ patienced ++ endmatch
	where 
		matchStart (x:xs) (y:ys) = if x == y 
			then ((Just x, Just y) : remainder, remxs, remys)
			else (remainder, remxs, remys)
			where
				(remainder, remxs, remys) = matchStart xs ys
		(startmatch, nonstartxs, nonstartys) = matchStart xs ys
		(endmatch, midxs, midys) = List.reverse $ matchStart (List.reverse nonstartxs) (List.reverse nonstartys)
		patienced = XXX midxs midys
	
		

