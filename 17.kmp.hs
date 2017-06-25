import Data.List
import Hugs.Observe

data KMP a = KMP { 
  done :: Bool
, next :: (a -> KMP a)
}

makeTable :: Eq a => [a] -> KMP a
makeTable xs = table
   where table = makeTable' xs (const table)
   
makeTable' []     failure = KMP True failure
makeTable' (x:xs) failure = KMP False test
   where  test  c = if c == x then success else failure c
          success = makeTable' xs (next (failure x))
   
isSubstringOf2 :: Eq a => [a] -> [a] -> Bool
isSubstringOf2 as bs = match (makeTable as) bs
   where  match table []     = done table
          match table (b:bs) = done table || match (next table b) bs

		  

isSubstringOf3 as bs = any done $ scanl next (makeTable as) bs




--makeTable1 :: Eq a => [a] -> KMP a
--makeTable1 []     = KMP True  undefined?
--makeTable1 (x:xs) = KMP False (\c -> if c == x then makeTable1 xs else ????)