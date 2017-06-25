
allcp xs = fst4 (until (done n) (step xs) ([n], 0, 0, 1))
	where n = length xs
	
done n (asx, i, p, k) = k == n

step xs (asx, i, p, k)
	| k >= i + p = (snoc asx a, k, a, k + 1)
	| q /= r     = (snoc asx (min q r ), i, p, k + 1)
	| q == r     = (snoc asx b, k, b, k + 1)
		where 
		q = asx !! (k − i)
		r = p − (k − i)
		a = llcp xs (drop k xs)
		b = q + llcp (drop q xs) (drop (q + k) xs)
		
fst4 (a, b, c, d) = a

snoc xs x = xs ++ [x]

llcp xs [ ] = 0
llcp [ ] ys = 0
llcp (x:xs) (y:ys) = if x == y then 1 + llcp xs ys else 0


allcp2 xs = extract (until done step (asx, empty, 0, 1)))
	where
	extract (asx, qs, h, k) = elems asx
	done (asx, qs, h, k) = (k n)
	n = length xs
	asx = insert empty n
	xa = listArray (0, n−1) xs
	step (asx, qs, h, k) 
	| k >= h = (insert asx a, insert asx' a, k + a, k + 1)
	| q /= r = (insert asx m, insert qs' m, h, k + 1)
	| q == r = (insert asx b, insert asx' b, k + b, k + 1)
		where 
		asx' = snd (remove as)
		(q, qs') = remove qs
		r = h − k
		m = min q r
		a = llcp' 0 k
		b = q + llcp' q (q + k)

llcp' j k 
	| j == n ∨ k == n = 0
	| xa ! j ==  xa ! k = 1+llcp (j + 1) (k + 1)
	| otherwise = 0
