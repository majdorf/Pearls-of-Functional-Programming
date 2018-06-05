
subseqs [ ] = [[ ]]
subseqs (x : xs) = map (x :) (subseqs xs) ++ subseqs xs


nonmember p cs = and [p knows c ∧ not (c knows p) | c ← cs]
member p ps cs = and [x knows p ∧ (p knows x ⇔ x in cs | x ←ps]

ccliques [] = [[]]
ccliques (p:ps) = map (p:) (filter (member p ps) css) ++
				  filter (nonmember p) css
				  where css = ccliques ps
				  
--linear time algorithm					
cclique' = foldr op []
op p cs | null cs = [p]
		| not (p knows c) = [p]
		| not (c knows p) = cs
		| otherwise = p:cs
		where c = head cs
		
		
--not (null (cclique ps)) ⇒ cclique ps = cclique' ps