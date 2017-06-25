
type Cell = Int
type Grid = [(Cell,Cell)]
type Vehicle = Int
type Move = (Vehicle, Cell)
type State = Grid


occupied::Grid->[Cell]
occupied = foldr (merge.fillcels) []

fillcells (r,f) = if r > f-7 then [r..f] else [r,r+7..f-7]


freecells::Grid ->[Cell]
freecells g = allcells \\ occupied g
	where allcells = [c | c<-[1..41], c mod 7 <>0]
	
moves:: Grid ->[Move]
moves g = [(v,c) | (v,i) <-zip [0..] g,c <- adjs i, c in fs]
	where fs = freecells g 
	
adj (r,f) = if r > f - 7 then [f + 1 , r - 1] else [f + 7, r - 7]

move q (v,c) = q1 ++ adjust i c:g2
	where (g1, i:g2 ) = splitAt v g
	
adjust (r,f) c =
	| r > f - 7 = if c >f then (r+1,c) else (c, f-1)
	| otherwise = if c < r then ( c, f-7) else (r+7,c)

solved::Grid->Bool
solved g = snd(head g) = 20

bfsolve :: Grid -> Maybe {Move]
bfsolve g = bfsearch' [] [] [([], g)]

goalmoves:: Grid -> Plan
goalmoves g = [(0,c) | c <- [snd (head g) + 1..20]]

blocker:: Grid -> Cell -> ( Vehicle, (Cell,Cell))
blocker g c = search (zip [0..] g) c

search ((v,i): vis) c = if covers c i then (v,i) else search vis c 

covers c (r,f) = r <= c and c <= f and (r > f - 7 or (c - r) mod 7 = 0 ) 


freeingmoves::Cell -> (Vehicle, (Cell , Cell )) -> [[Move]]
freeingmoves c (v, (r , f ))
	| r > f −7 = [[(v, j ) | j <- [f +1 .. c+n]] | c + n < k+7] ++
				 [[(v, j ) | j <- r−1, r−2 .. c−n]] | c − n > k]
	| otherwise = [[(v, j ) | j <- [r−7, r−14 .. c−m]] | c − m > 0] ++
				[[(v, j ) | j <- [f +7, f +14 .. c+m]] | c + m < 42]
	where (k,m, n) = (f −f mod 7, f −r + 7, f −r+1)
	
	
premoves :: Grid -> Move -> [[Move]]
premoves g (v, c) = freeingmoves c (blocker g c)


newplans :: Grid -> Plan -> [Plan]
newplans g [ ] = [ ]
newplans g (m : ms) = mkplans (expand g m ++ ms)
where mkplans ms = if m in gms then [ms] else
		concat [mkplans (pms ++ ms) |
		pms <- premoves g m,
		all (/∈ ms) pms]
		where m = head ms; gms = moves g
		
expand :: Grid -> Move ->  [Move]
expand g (v, c)
| r > f − 7 = if c > f then [(v, p) | p <- [f + 1 .. c]]
			  else [(v, p) | p <- [r−1, r−2 .. c]]
| otherwise = if c > f then [(v, p) | p <- [f +7, f +14 .. c]]
			  else [(v, p) | p <- [r−7, r−14 .. c]]
where (r , f) = g !! v

psearch' :: [State] -> AFrontier -> AFrontier -> Maybe [Move]
psearch' qs [ ] [ ] = Nothing
psearch' qs rs [ ] = psearch' qs [ ] rs
psearch' qs rs (p@(ms, q, plan) : ps)
| solved q = Just (reverse ms)
| q ∈ qs = psearch' qs rs ps
| otherwise = psearch' (q : qs) (bsuccs p ++ rs) (asuccs p ++ ps)


psolve :: Grid -> Maybe [Move]
psolve g = psearch' [ ] [ ] [([ ], g, goalmoves g)]