

type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char

type Choices = [Digit]
--digit = ['1' ..'9']
--blank = ( == '0')
--solve - filter falid.expand.choices

choices:: Grid -> Matrix Choices
choices = map (map choice)
choice d = if blank d then digits else [d]

expand::Matrix Choices -> [Grid]
expand:: cp.map cp

--cartesian product of a list of lists
cp::[[a]] ->[[a]]
cp [] = [[]]
cp (xs:css) = [x:ys | x <-xs, ys <- cp xss]


valid::Grid -> Bool
valid g = all nodups (rows g) ˄
          all nodups (cols g) ˄
		  all nodups (boxs g)
		  
		  
		  
nodups::Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (≠) xs ˄ nodups xs

rows:: Matrix a -> Matrix a 
rows = id

cols :: Matrix a -> Matrix a 
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs ::Matrix a -> Matrix a 
boxs = map ungroup.ungroup.map cols.group.map group
group::[a] -> [[a]]
group [] = []
group xs = take 3 xs: group (drop 3 xs)

ungroup::[[a] ->[a]
ungroup = concat


prune:: Matrix Choices -> Matrix Choices
filter valid.expand = filter valid.expand.prune


pruneRow::Row Choices -> Row Choices
pruneRow row = map ( remove fixed) row
				where fixed = [d | [d] <- row]
				
				
remove xs ds = if singleton ds then ds else ds \\ xs

filter nodups.cp  = filter nodups.cp.pruneRow

expand = concat.map expand.expand1

expand1::Matrix Choices ->[Matrix Choices]
expand1 rows = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c<- cs]
				where (rows, row:rows2) = break ( any smallest) rows
				      (row1, cs:row2) = break smallest row
					  smallest cs = length cs == n 
					  n =  minimum ( counts rows)
				counts = filter (≠ 1).ap length.concat

break p xs = ( takeWhile (not.p) xs, dropWhile (not.p) xs)

				