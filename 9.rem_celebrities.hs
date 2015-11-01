
subseqs [ ] = [[ ]]
subseqs (x : xs) = map (x :) (subseqs xs) ++ subseqs xs