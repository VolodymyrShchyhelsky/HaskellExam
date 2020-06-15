module Main where

-- Task 1
sameElement :: [Int] -> Int
sameElement (x:y:other)
    | x == y = x
    | otherwise = sameElement (y:other)
sameElement (x:_) = -1

-- Task 2
insertEl :: Int -> [Int] -> [Int]
insertEl x (y:ys) = if x < y 
                 then x:y:ys 
         else y : insertEl x ys
insertEl x [] = [x]

insSort :: [Int] -> [Int] -> [Int]
insSort sorted (first_el : other) = insSort (insertEl first_el sorted) (other)
insSort sorted [] = sorted

main = do
    -- Task 1
    let arr1 = [4, 1, 5, 5, 6, 7]
    let ans = (sameElement arr1)
    print "Same element"
    if ans == -1
        then print "No such element"
        else print ans

    -- Task 2
    let arr2 = [4, 1, 6, 2, 7]
    print "Sorted array"
    putStrLn $ show (insSort [] arr2)
