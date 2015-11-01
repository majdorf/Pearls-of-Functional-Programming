import Data.List(sort, takeWhile,sortBy)
import Data.Array

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



untransform2 (ys,k) = take n (tail (map (ya!)(iterate (pa!) k)))
	where 
		n = length ys
		ya = listArray (0,n-1) ys
		pa = listArray (0,n-1) (map snd (sort (zip ys [0..])))

tag xs = xs ++ []

tails :: [a] -> [[a]]
tails [] = []
tails (x:xs) = (x:xs):tails xs

transform2 xs = ([xa! (pa!i)| i<-[0..n-1]],k)
	where
		n = length xs
		k = length (takeWhile (/= 0) ps)
		xa = listArray (0,n-1) (rrot xs)
		pa = listArray (0,n-1) ps
		ps = map snd (sort(zip (tails (tag xs)) [0..n-1]))

