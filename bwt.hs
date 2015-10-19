import Data.List(sort, takeWhile,sortBy)

transform::Ord a => [a] -> ([a], Int)
transform xs = (map last xss, position xs xss)
	where xss = sort (rots xs)
	
position xs xss = length (takeWhile (/= xs) xss)

rots :: [a] -> [[a]]
rots xs = take (length xs) (iterate lrot xs)
	where lrot (x : xs) = xs ++ [x]
	
	
takeCols :: Int -> [[a]] -> [[a]]
takeCols j = map (take j )

rrot::[a]->[a]
rrot xs = [last xs] ++ init xs

hdsort:: Ord a => [[a]]->[[a]]
hdsort = sortBy cmp where cmp (x:xs)(y:ys) = compare x y

consCol :: ([a],[[a]])->[[a]]
consCol (xs,xss) = zipWith (:) xs xss

untransform :: Ord a => ([a], Int) -> [a]
untransform (ys, k) = (recreate (length ys) ys) !! k

fork (f,g) x = (f x, g x)

recreate ::Ord a => Int ->[a] -> [[a]]
recreate 0  = map (const [])
recreate j  = hdsort.consCol.fork (id, recreate (j-1))



