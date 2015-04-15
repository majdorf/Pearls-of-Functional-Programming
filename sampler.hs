tails :: [a] -> [[a]]
tails [] = []
tails (x:xs) = (x:xs):tails xs

msc :: Ord a => [a] ->Int
msc xs = maximum[scount z zs | z:zs <- tails xs]
	where scount x xs = length ( filter ( x< ) xs)
	
msc2 :: Ord a => [a] -> [Int]
msc2 xs = [scount z zs | z:zs <- tails xs]
	where scount x xs = length ( filter ( x< ) xs)

joinS::Ord a => Int -> [(a,Int)]-> [(a,Int)]->[(a, Int)]
joinS 0 txs [] = txs
joinS n [] tys = tys
joinS n txs@((x,c):txs') tys@((y,d):tys')
	| x < y = (x, c + n):joinS n txs' tys
	| x >= y = (y,d) :joinS ( n-1 ) txs tys'

	
table :: Ord a => [a] -> [(a,Int)]
table [x] = [(x,0)]
table xs = joinS (m - n) (table ys) (table zs)
	where
		m = length xs;
		n = m `div` 2;
		(ys,zs) = splitAt n xs
		

invert::(Int->Int->Int)->Int->[(Int,Int)]
invert f z = findx (0,z) f z 

findx::(Int,Int)->(Int->Int->Int)->Int->[(Int, Int)]
findx (u,v) f z | ( u > z || v < 0 ) = []
		        | z' < z = findx (u + 1, v) f z 
		        | z' == z = (u,v):findx(u+1, v-1) f z 
		        | z' > z = findx (u, v - 1) f z 
					where z' = f u v

bsearch::(Int->Int)->(Int,Int)->Int->Int
bsearch g (a,b) z | a + 1 == b  = a
				   | g m <= z    = bsearch g (m,b) z 
				   | otherwise   = bsearch g (a,m) z
				   where m = (a + b) `div` 2 

find::(Int,Int)->(Int,Int)->(Int->Int->Int)->Int->[(Int, Int)]
find (u,v) (r,s) f z
	| u > r || v < s  = []
	| v - s  <= r - u = rfind ( bsearch (\x-> f x q) ( u - 1 , r + 1) z)
	| otherwise       = cfind ( bsearch (\y-> f p y) ( s - 1 , v + 1) z)
	where
		p = (u + r) `div` 2
		q = (v + s) `div` 2
		rfind p = (if f p q == z then (p,q):find (u,v) (p - 1, q + 1) f z 
				   else find (u,v) (p,q+1) f z  ++ find ( p + 1, q - 1) (r,s) f z)
		cfind q = find  (u,v) (p - 1, q + 1) f z ++
		          ( if f p q == z then (p,q):find (p + 1, q - 1) (r,s) f z
				   else find (p + 1, q) (r,s) f z )


f0::Int->Int->Int
f0 x y = (2^y) * ( 2 * x + 1) - 1
