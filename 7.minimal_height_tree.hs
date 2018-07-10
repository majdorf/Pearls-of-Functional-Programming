
data Tree = Leaf Int | Fork Tree Tree

cost (Leaf x) = x
cost (Fork u v) = 1 + (cost u `max` cost v)

foldrn::(a->b->b)->(a->b)->[a]->b
foldrn f g [x] = g x
foldrn f g (x:xs) = f x (foldrn f g xs)

prefixes::Int->Tree->[Tree]
prefixes x t@(Leaf y) = [Fork (Leaf x) t]
prefixes x t@(Fork u v) = [Fork (Leaf x) t]++ [Fork u' v| u' <- prefixes x u]

trees = foldrn (concatMap.prefixes) (wrap.Leaf)
wrap x = [x]


instance  Show (Tree ) where
   show (Leaf x) = "( L "++ show x ++ ")"
   show (Fork l r) =  "( F " ++ show l ++ " " ++ show r ++ ")"
  
type Forest       = [Tree]  

rollup::Forest -> Tree
rollup = foldl1 Fork

forests::[Int]->[Forest]
forests = foldrn (concatMap.prefixes2) (wrap.wrap.Leaf )

prefixes2::Int->Forest->[Forest]
prefixes2 x ts = [Leaf x :rollup (take k ts) :drop k ts | k <- [1.. length ts]]

trees2 = map rollup.forests

insert x ts = Leaf x:split x ts

split x [u]= [u]
split x (u:v:ts) = if x `max` cost u < cost v then u:v:ts
                   else split x (Fork u v:ts)

				   
mincostTree :: [Int] -> Tree				   
mincostTree = foldl1 Fork . map  snd . foldrn insert2 (wrap.leaf) 
insert2 x ts = leaf x: split2 x ts 

split2 x [u] = [u]
split2 x (u:v:ts) = if x `max` fst u < fst v then u:v:ts
                    else split2 x (fork u v:ts)

leaf x = (x, Leaf x)
fork (a,u) (b,v) = (1+ a `max` b, Fork u v)
