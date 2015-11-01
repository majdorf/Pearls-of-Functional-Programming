type Expression = [Term]
type Term       = [Factor]
type Factor     = [Digit]
type Digit      = Int

extend::Digit->[Expression]->[Expression]
extend x []  = [[[[x]]]]
extend x es = concatMap (glue x) es

glue::Digit->Expression->[Expression]
glue x ((xs:xss):xsss) = [((x:xs):xss):xsss, ([x]:xs:xss):xsss, [[x]]:(xs:xss):xsss]

good::Int->Bool
good v = ( v == 100)

ok :: Int->Bool
ok v = (v <= 100)

valFact::Factor->Int
valFact = foldl1 (\n d -> 10*n + d)

valTerm::Term->Int
valTerm = product.map valFact

valExpr::Expression->Int
valExpr = sum.map valTerm

expressions = foldr extend []

--solutions = map fst.filter (good.snd).foldr expand []
--expand x = filter (ok.snd).zip.cross (extend x , modify x ).unzip


goodN c (k, f , t, e) = (f*t + e == c)
okN c (k, f , t, e) = (f*t + e <= c)

solutionsN c = map fst.filter (goodN c.snd).foldr (expand c) []

expand c x [] = [([[[x]]], (10, x , 1, 0))]
expand c x evs = concat (map (filter (okN c.snd).glueN x ) evs)

glueN x ((xs:xss):xsss, (k,f,t,e)) =
	[(((x:xs):xss):xsss, (10*k, k*x + f , t, e)),
	(([x]:xs:xss):xsss, (10, x , f*t, e)),
	([[x]]:(xs:xss):xsss, (10, x , 1, f *t + e))]
