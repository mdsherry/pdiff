import Data.HashSet as HashSet
import Data.HashMap as HashMap
import Data.Hashable
import Data.Text as T
import Data.List as List
import Debug.Trace

uniqs :: (Ord a, Hashable a) => [a] -> Set a
uniqs = uniqs' HashSet.empty
	where 
		uniqs' hash (x:xs) = if HashSet.member x hash
			then
				uniqs' (HashSet.delete x hash) xs
			else
				uniqs' (HashSet.insert x hash) xs
		uniqs' hash _ = hash

uniqMap :: (Ord a, Hashable a) => [a] -> Set a -> Map a Int
uniqMap xs unique = HashMap.fromList $ List.filter (\(x,y) -> HashSet.member x unique) $ List.zip xs [1..]

uniqCommon xs ys = HashSet.intersection (uniqs xs) (uniqs ys)

getPairs ::(Ord a, Hashable a) => [a] -> [a] -> [(Int, Int)]
getPairs xs ys = deMaybe [(HashMap.lookup line uniqxmap, HashMap.lookup line uniqymap) | line <- HashSet.toList common ]
	where
		common = uniqCommon xs ys
		uniqxmap = uniqMap xs common
		uniqymap = uniqMap ys common
		deMaybe ((Just x, Just y):xs) = (x,y):deMaybe xs
		deMaybe (_:xs) = deMaybe xs
		deMaybe _ = []

patience :: Ord a => [(a,a)] -> [(a, a)]
patience xs = List.reverse $ List.last $ patience' xs []
	where
		patience' (x:xs) ls = patience' xs (findLocation x ls [])
		patience' _ ls = ls
		findLocation :: Ord a => (a,a) -> [[(a,a)]] -> [(a,a)] -> [[(a,a)]]
		findLocation v [] past = [v:past]
		findLocation v@(x, y) (l@((_,sy):_):ls) past = if y < sy 
			then 
				(v:past) : ls
			else
				l : findLocation v ls l

processPairs ps@(p:_) xs ys = processPairs' (0,0) ps xs ys
	where
		processPairs' (xoff, yoff) (p@(px,py):ps) xs ys = (pdiff leadx leady) ++ (processPairs' (xoff + px, yoff + py) ps tailx taily)
			where
				(leadx, tailx) = List.splitAt (px - xoff) xs
				(leady, taily) = List.splitAt (py - yoff) ys
		processPairs' _ [] xs ys = pdiff xs ys
processPairs [] xs ys = pdiff xs [] ++ pdiff [] ys


matchStart :: Eq a => [a] -> [a] -> ([(Maybe a, Maybe a)], [a], [a])
matchStart (x:xs) (y:ys) = if x == y 
	then ((Just x, Just y) : remainder, remxs, remys)
	else ([], x:xs, y:ys)
	where
		(remainder, remxs, remys) = matchStart xs ys
matchStart xs ys = ([], xs, ys)


pdiff [] ys = [(Nothing, Just y) | y <- ys]
pdiff xs [] = [(Just x, Nothing) | x <- xs]
pdiff xs ys = startmatch ++ patienced ++ endmatch
	where 
		(startmatch, nonstartxs, nonstartys) = matchStart xs ys
		(endmatch, midxs, midys) = (List.reverse ending, List.reverse xs, List.reverse ys) 
			where 
				(ending, xs, ys) = matchStart (List.reverse nonstartxs) (List.reverse nonstartys)
		midpairs = List.sort $ getPairs midxs midys
		patienced = processPairs midpairs midxs midys

printable (Just x, Just y) = x
printable (Just x, Nothing) = "-" ++ x
printable (Nothing, Just y) = "+" ++ y	

main = do
		f1 <- readFile "pdiff.hs"
		f2 <- readFile "pdiff2.hs"
		let lines1 = List.lines f1
		let lines2 = List.lines f2
		let diff = pdiff lines1 lines2
		mapM (putStrLn . printable) diff

