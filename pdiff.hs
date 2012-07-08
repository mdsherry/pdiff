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

blah :: [String] -> [String] -> Set String
blah xs ys = common
	where
		common = uniqCommon xs ys
		--uniqxmap = uniqMap xs common
		--uniqymap = uniqMap ys common


patience :: Ord a => [(a,a)] -> [(a, a)]
patience xs = List.reverse $ List.last $ patience' xs []
	where
		patience' (x:xs) ls = patience' xs (findLocation x ls [])
		patience' _ ls = ls
		findLocation :: Ord a => (a,a) -> [[(a,a)]] -> [(a,a)] -> [[(a,a)]]
		findLocation v [] past = [v:past]
		findLocation v@(x, y) (l@((_,sy):history):ls) past = if y < sy 
			then 
				(v:past) : ls
			else
				l : findLocation v ls l


matchStart :: Eq a => [a] -> [a] -> ([(Maybe a, Maybe a)], [a], [a])
matchStart (x:xs) (y:ys) = if x == y 
	then ((Just x, Just y) : remainder, remxs, remys)
	else ([], x:xs, y:ys)
	where
		(remainder, remxs, remys) = matchStart xs ys
matchStart xs ys = ([], xs, ys)


pdiff xs ys = startmatch ++ patienced ++ endmatch
	where 
		(startmatch, nonstartxs, nonstartys) = matchStart xs ys
		(endmatch, midxs, midys) = (List.reverse ending, List.reverse xs, List.reverse ys) 
			where 
				(ending, xs, ys) = matchStart (List.reverse nonstartxs) (List.reverse nonstartys)
		patienced = [] -- XXX midxs midys
	
main = do
		putStrLn "Hello world"
