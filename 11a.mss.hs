import Prelude

inits :: (Num a) => [a] -> [[a]]
inits = foldr g [[]] where g x xss = []:map (x:) xss

tails :: (Num a) => [a] -> [[a]]
tails =  foldr f [[]] where f x xss = (x: head xss):xss


segs :: (Num a) => [a] -> [[a]]
segs xs = concat.map inits $ tails xs

mss :: (Num a, Ord a ) => [a] -> a
mss xs = maximum.map sum $ segs xs


-- maximum.map sum $ inits
mss2 :: (Num a, Ord a ) => [a] -> a
mss2 xs = foldr mpl e xs where e = 0; u `mpl`  z   = e  `max` (u + z)

--mss3 :: (Num a, Ord a ) => [a] -> a
--	mss3 xs = map mss2 $ segs xs
