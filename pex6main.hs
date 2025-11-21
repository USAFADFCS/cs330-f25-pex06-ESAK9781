-- pex6.hs 
-- unKnot Haskell

-- name: Elijah Sakamoto

{- DOCUMENTATION:

Used this thread to figure out how to do multiple arguments in haskell for "deletePair"
https://stackoverflow.com/questions/37694942/haskell-multiple-arguments-function

Used this thread to figure out how to do not equal to comparison in haskell for "applyII"
https://stackoverflow.com/questions/34415487/what-does-the-operator-in-haskell-mean

Used this thread to figure out how to use trace for debugging. Used this to debug. Debugging
was done almost everywhere. Haskell is a language with terrible debugging.
https://stackoverflow.com/questions/7253315/simple-debugging-in-haskell 


-}


-- does all possible I transformations this iteration
applyI :: [(Char, Char)] -> [(Char, Char)]
applyI [] = [] -- empty, do nothing
applyI (x : xs) = 
   if (null xs) -- tail is empty, clearly no pairs, do nothing
      then [x] ++ xs 
   else if ((fst x) == (fst (head xs))) -- cosecutive pair
      then applyI (drop 1 xs) -- splice out the pair, recurse over rest of list
   else [x] ++ applyI(xs) -- recurse over list


-- goes through the list, tries to find [...(fst pair, _), (snd pair, _), ...], 
-- deletes it if it exists
deletePair :: [(Char, Char)] -> (Char, Char) -> [(Char, Char)]
deletePair list pair 
   | null list = [] -- empty, return empty (do nothing)
   | null (drop 1 list) = list -- no 2 remaining elements, do nothing
   | (fst (head list) == fst pair) -- if first element matches pair
      && (fst (head (drop 1 list)) == snd pair) -- ...and second element matches pair
      && (snd (head list) == snd (head (drop 1 list)))  -- ...and same type
         = drop 2 list -- return the list with the pair spliced out, end recursive search
   | otherwise = [head list] ++ (deletePair (drop 1 list) pair) -- otherwise, recurse to search rest of list

-- does all possible II transformations this iteration
applyII :: [(Char, Char)] -> [(Char, Char)]
applyII list 
   | null list = [] -- empty, return empty (do nothing)
   | null (drop 1 list) = list -- empty tail, return tail (do nothing)
   | (snd (head list) == snd (head (drop 1 list))) -- possible pair
      = if (deletePair (drop 2 list) (fst (head list), fst (head (drop 1 list))) /= drop 2 list) -- same order
            then (deletePair (drop 2 list) (fst (head list), fst (head (drop 1 list)))) -- splice out the type II
        else if (deletePair (drop 2 list) (fst (head (drop 1 list)), fst (head list)) /= drop 2 list) -- different order
            then (deletePair (drop 2 list) (fst (head (drop 1 list)), fst (head list))) -- splice out the type II
            
        else [head list] ++ applyII (drop 1 list) -- reeeeeeeeecurse
   | otherwise = [head list] ++ (applyII (drop 1 list)) -- reeeeeeeeeeeeeeeeeeecurse


-- repeatedly tries to simplify the knot using recursion
simp :: [(Char, Char)] -> [(Char, Char)]
simp xs
   | null xs = xs
   | (xs == applyII(applyI(xs))) = xs
   | otherwise = simp(applyII(applyI(xs)))



unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null (simp tripCode) = "not a knot" -- it simplifies down to []
   | otherwise = "tangle - resulting trip code: " ++ (show (simp tripCode))

-- typeIknot :: [(Char, Char)] -> String
-- typeIknot tripCode

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

