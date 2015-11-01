
data Tree = Leaf Int | Fork Tree Tree

cost (Leaf x) = 
cost (Fork u v) = 1 + (cost u `max` cost v)

foldrn::(a->b->b)->(a->b)->[a]->b
foldrn f g [x] = g x
foldrn f g (x:xs) = f x (foldrn f g xs)

prefixes::Int->Tree->[Tree]
prefixes x t@(Leaf y) = [Fork (Leaf x) t]
prefixes x t@(Fork u v) = [Fork (Leaf x) t]++ [Fork u' v| u' <- prefixes x u]

trees = foldrn (concatMap.prefixes) (wrap.Leaf)
wrap x = [x]

trees = map rollup.forests

insert x ts = Leaf x:split x ts
split x [u]= [u]
split x (u:v:ts) = if x `max` cost u < cost v then u:v:ts 
				   else split x (Fork u v:ts)

mincostTree = foldl1 Fork.map snd.foldrn insert (wrap.leaf) 
insert x ts = leaf x: split x ts 
split x [u] = [u]
split x (u:v:ts) = if x `max` fst u < fst v then u:v:ts
				   else split x (fork u v:ts)

leaf x = (x, Leaf x)
fork (a,u) (b,v) = (1+ a `max` b, Fork u v)
