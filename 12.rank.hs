import Data.Array

psort::Ord b =>[(a,b)]->[[a]]
psort xys = pass xys []

pass [] xss = xss
pass (e@(x,y):xys) xss = step xys [] [x] [] xss
	where
		step [] ass bs cs xss = pass ass (bs:pass cs xss)
		step (e@(x,y'):xys) ass bs cs xss | y' < y = step xys (e:ass) bs cs xss
										 | y' == y = step xys ass (x:bs) cs xss
										 | y' > y =step xys ass bs (e:cs) xss


										 
ranktails::Ord a => [a]->[Int]
ranktails xs = (resort n.concat.label.applyUntil isperm (repartitions n).psort.zip [0..]) xs
	where
		n = length xs
		resort n = elems.array (0,n-1)
		label iss = zipWith tag iss (scanl (+) 0 (map length iss))
		tag is j = [(i,j) | i <- is]
		repartitions n  = map (repartition n) (iterate (*2) 1)
		repartitions n k iss = concatMap (psort.map install) iss
			where
				install i = (i, if j<n then k + a!j else n - i - 1)
				a = array (0,n-1) (concat (label iss))

applyUntil::(a->Bool)->[a->a]->a->a
applyUntil p (f:fs) x = if p x then x else applyUntil p fs ( f x)

isperm::[Int]->Bool
isperm is = and ( elems (accumArray (or) False (0, n - 1) (zip is (repeat True))))
	where
		n = length is
