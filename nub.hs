import Data.List( (\\))
import Data.Set

nub = hub []
hub ws [] = []
hub ws (x:xs) = case (x `elem` xs, x `elem` ws) of
					(False,False) -> us ++ [x] ++ hub [] (xs (\\) us)
					(False,True)  -> us ++ [x] ++ hub (tail vs) (xs (\\) us)
					(True,False)  -> hub (us ++ [x]) xs
					(True,True)   -> hub ws xs
				where
					(us,vs) = span (<x) ws
					
					
					
					
					
					
nub2 = hub empty.preprocess
preprocess xs = zip xs (tail (scanr insert empty xs))
hub ws [] = []
hub ws ((x,xs):xss) = 
	case (member x xs, member x ws) of
		(False, False) -> eus ++ [x] ++ hub empty yss
		(False, True) -> eus ++ [x] ++ hub vs yss
		(True, False) -> hub (insert x us) xss
		(True,True) -> hub ws xss
	where 
		(us,vs) = split x ws
		eus = elems us
		yss = [(x,xs)| (x,xs) <- xss, not (member x us)]
