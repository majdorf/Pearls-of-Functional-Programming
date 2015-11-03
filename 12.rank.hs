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


										 
--ranktails::Ord a => [a]->[Int]
--ranktails xs = (resort n.concat.label.applyUntil isperm (repartitions n).psort.zip [0..]) xs
--	where
--		n = length xs
--		resort n = elems.array (0,n-1)
--		label iss = zipWith tag iss (scanl (+) 0 (map length iss))
--		tag is j = [(i,j) | i <- is]
--		repartitions n  = map (repartition n) (iterate (*2) 1)
--		repartition n k iss = concatMap (psort.map install) iss
--			where
--				install i = (i, if j<n then k + a!j else n - i - 1)
--				a = array (0,n-1) (concat (label iss))

applyUntil::(a->Bool)->[a->a]->a->a
applyUntil p (f:fs) x = if p x then x else applyUntil p fs ( f x)

isperm::[Int]->Bool
isperm is = and ( elems (accumArray (||) False (0, n - 1) (zip is (repeat True))))
	where
		n = length is


--rank::Ord a =>[a]->[Int]
rank  = resort.concat.label.psort.zip [0..] 

resort::[(Int,Int)]->[Int]
resort ijs = elems (array (0, length ijs - 1) ijs)

label::[[a]]->[[(a,Int)]]
label xss = zipWith tag xss (scanl (+) 0 (map length xss))
tag xs k = [(x,k) | x <- xs]

partition::Ord a =>[a]->[[Int]]
partition = psort.zip [0..]
	
ranktails0 = applyUntil isperm rerankings.rank
rerankings = map rerank (iterate (*2) 1)
rerank k rs = rs `rzip` shiftBy k rs

shiftBy k rs = map (+k) (drop k rs) ++ [k-1,k-2..0]

rzip xs ys = rank ( zip xs ys)

repartitions = map repartition (iterate (*2) 1)
repartition k iss = partition (zip rs (shiftBy k rs))
	where
		rs = resort (concat (label iss))
