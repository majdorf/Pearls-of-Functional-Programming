import Data.List( (\\))

nub = hub []
hub ws [] = []
hub ws (x:xs) = case (x `elem` xs, x `elem` ws) of
					(False,False) -> us ++ [x] ++ hub [] (xs (\\) us)
					(False,True)  -> us ++ [x] ++ hub (tail vs) (xs (\\) us)
					(True,False)  -> hub (us ++ [x]) xs
					(True,True)   -> hub ws xs
				where
					(us,vs) = span (<x) ws
