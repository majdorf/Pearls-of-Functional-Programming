import Data.List( sortBy )
import Data.List.Utils( mergeBy )
import Data.Array
 
type Label a = (a,(Int,Int))
ominus =(-)

sortsums::Num a => [a]->[a]->[a]
sortsums xs ys = map fst (sortsubs xs (map negate ys))

sortsubs xs ys = sortBy ( cmp (mkArray xs ys)) (subs xs ys)


subs::Num a => [a]->[a]->[Label a ]
subs xs ys = [(x - y, (i,j)) | (x,i) <- zip xs [1..], (y,j)<- zip ys [1..]]

cmp a (x,(i,j)) (y,(k,l)) = compare (a !(1,i,k)) ( a !(2,j,l))
mkArray xs ys = array b (zip (table xs ys) [1..])
				where 
					b = ((1,1,1),(2,p,p))
					p = max (length xs) (length ys)
					
merge = mergeBy compare			
	
table::Num a =>[a]->[a]->[(Int,Int,Int)]	
table xs ys = map snd (map (tag 1) xs `merge` map (tag 2) yys)
			where
				xxs = sortsubs' xs
				yys = sortsubs' ys

tag i (x,(j,k))  = (x, (i,j,k))


sortsubs' [] = []
sortsubs' [w] = [(w ominus w), (1,1)]
sortsubs' ws = foldr (merge) [ xxs , map (incr m) xys, map (incl m) yxs, map (incb m) yys]
		where
			xxs = sortsubs' xs
			xys = sortBy (cmp (mkArray xs ys)) (subs xs ys)
			yxs = map switch (reverse xys)
			yys = sortsubs' ys
			(xs, ys) = splitAt m ws
			m = length ws `div` 2

incl m (x,(i,j)) = (x,( m + i,j ))
incr m (x,(i,j)) = (x, (i,m +j))
incb m (x,(i,j)) = (x, (m +i, m+j))

switch (x,(i,j)) = (negate x, (j,i))

