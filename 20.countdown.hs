
data Op = Add | Sub | Mul | Div
data Expr = Num Int | App Op Expr Expr
type Value = Int

countdown1 :: Int -> [Int] -> (Expr, Value)
countdown1 n = nearest n .concatMap mkExprs.subseqs

--nonempty subseqs
subseqs [x] = [[x]]
subseqs (x:xs) = xss ++ [x]:map (x:) xss
                 where xss = subseqs xs

value :: Expr -> Value
value (Num x) = x
value (App op e1 e2) = apply op (value e1) (value e2)
                       where apply Add = (+);
                             apply Mul = (*);
							 apply Div = div;
                             apply Sub = (-)

legal::Op -> Value -> Value -> Bool
legal Add v1 v2 = True
legal Sub v1 v2 = (v2 < v1)
legal Mul v1 v2 = True
legal Div v1 v2 = (v1 `mod` v2 == 0)

mkExprs::[Int] -> [(Expr, Value)]
mkExprs [x] = [(Num x , x )]
mkExprs xs = [ev | (ys, zs) <- unmerges xs,
                   ev1 <- mkExprs ys,
                   ev2 <- mkExprs zs,
                   ev  <- combine ev1 ev2]


unmerges::[a]->[([a], [a])]
unmerges [x , y] = [([x ], [y]), ([y], [x ])]
unmerges (x:xs) = [([x], xs), (xs, [x])] ++
                    concatMap (add x) (unmerges xs)
                    where add x (ys, zs) = [(x:ys, zs), (ys, x:zs)]
	
combine::(Expr, Value)->(Expr, Value)->[(Expr, Value)]
combine (e1, v1) (e2, v2) = [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2]
                            where ops = [Add, Sub, Mul, Div];
							      apply Add = (+);
							      apply Mul = (*);
							      apply Div = div;
                                  apply Sub = (-)

							
							
							
nearest n ((e,v):evs) = if d == 0 then (e, v)
                         else search n d (e, v) evs
                         where d = abs(n - v)
search n d ev [] = ev
search n d ev ((e, v) : evs) 
  | d' ==  0 = (e, v)
  | d' < d = search n d' (e, v) evs
  | d' >= d = search n d ev evs
          where d' = abs(n - v)
	
--display (countdown1 831 [1,3,7,10,25,50])	
--optimizations 
--1. use commutativity and associativity law
{-
legal Add v1 v2 = (v1 ≤ v2)
legal Sub v1 v2 = (v2 < v1)
legal Mul v1 v2 = (1< v1) ∧ (v1 ≤ v2)
legal Div v1 v2 = (1< v2) ∧ (v1 mod v2 0)

unmerges [x,y] = [([x], [y])]
unmerges (x:xs) = [([x], xs)] ++ concatMap (add x) (unmerges xss)
                  where add x (ys, zs) = [(x : ys, zs), (ys, x : zs)]
				  
				  
combine (e1, v1) (e2, v2)
  = [(App op e1 e2, apply op v1 v2) | op <- ops, legal op v1 v2] ++
    [(App op e2 e1, apply op v2 v1) | op <- ops, legal op v2 v1]
	
	
comb1 (e1, v1) (e2, v2)
  = [(App Add e1 e2, v1 + v2), (App Sub e2 e1, v2 − v1)] ++
    if 1 < v1 then
    [(App Mul e1 e2, v1 * v2)] ++ [(App Div e2 e1, q) | r == 0]
    else []
    where (q, r) = divMod v2 v1
	
comb2 (e1, v1) (e2, v2)
 = [(App Add e1 e2, v1 + v2)] ++
   if 1 < v1 then
   [(App Mul e1 e2, v1 * v2), (App Div e1 e2, 1)]
   else []
   
combine (e1, v1) (e2, v2)
  | v1 < v2 = comb1 (e1, v1) (e2, v2)
  | v1 v2 = comb2 (e1, v1) (e2, v2)
  | v1 > v2 = comb1 (e2, v2) (e1, v1)
-}

   
