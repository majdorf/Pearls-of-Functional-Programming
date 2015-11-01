
unravels::[a]->[[[a]]]
unravels = foldr (concatMap.prefixes) [[]]

prefixes x [] = [[[x]]]
prefixes x (xs:xss) = [(x:xs):xss]++ map (xs:) (prefixes x xss)

insert x [] = [[x]]
insert x (xs:xss) = if x <= head xs then (x:xs):xss 
					else xs:insert x xss

