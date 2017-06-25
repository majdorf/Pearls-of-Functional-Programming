
--maxtail::Ord a => [a] ->[a]
--maxtail = maximum.tails

maxtail = thd.cocktail
cocktail = foldl op (0,0,[],[])

op (p,q, ys, ws) x 
	| q == 0  = (0,1[x],[x])
	| w < x   =  cocktail (drop ( q - r) ws ++ [x])
	| w == x  = (p+1, q, ys ++ [x], tail ws ++ [x])
	| otherwise = (0, p + q + 1, ys ++ [x], ys ++ [x])
		where
			w = head ws
			r = p `mod` q
