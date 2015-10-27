
mnss::[Int]->Int
mnss = maximum.map sum.nonsegs

markings::[a]->[[(a,Bool)]]
markings xs = [ zip xs bs | bs <- booleans (length xs)]

booleans 0 = [[]]
booleans n = [b:bs | b <- [True,False], bs <- booleans (n - 1)]

nonsegs::[a] ->[[a]]
nonsegs = extract.filter nonseg.markings

extract::[[(a,Bool)]] -> [[a]]
extract = map( map fst.filter snd)

data StateS = E | S | M | N
	deriving (Eq)
--F*T+F+T(T + F)*
step E  False = E
step E  True  = S
step S  False = M
step S  True = S
step M  False = M
step M True = N
step N False = N
step N True = N


nonseg::[(a,Bool)] -> Bool
nonseg = (N == ).(foldl step E).map snd


--second solution
--mnss2 xs = fourth (foldl h (start (take 3 xs)) (drop 3 xs))
--start [x,y,z] = (0, max [x+y+z,y+z,z], max [x, x+y, y], x + z)


