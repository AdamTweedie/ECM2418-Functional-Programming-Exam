-- ECM2418 Computer Languages and Representations
-- Continuous Assessment 1: Functional Programming
-- Author - 690024916

--------------------------------Question 1--------------------------------
-- 1.1
tupleToList :: (Int,Int,Int,Int,Int,Int) -> [Int]
tupleToList (t1,t2,t3,t4,t5,t6)
  = [t1,t2,t3,t4,t5,t6]

contains :: [Int] -> Int -> Bool
-- Does the list [] contain n
contains [] n
  = False
contains (x:xs) n
  = x == n || contains xs n

noDuplicates :: [Int] -> Bool
-- Ensuring there are no duplicates in the list
noDuplicates []
  = True
noDuplicates (x:xs)
  | contains xs x = False
  | otherwise = noDuplicates xs

rule1 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule1 (t1,t2,t3,t4,t5,t6)
  = noDuplicates (tupleToList (t1,t2,t3,t4,t5,t6))


-- 1.2
isEven :: Int -> Bool
isEven e
  = e `mod` 2 == 0

altOddEven :: [Int] -> [Bool]
-- Creates a list of the bool results to isEven for each element in list
altOddEven []
  = []
altOddEven (x:xs)
  = isEven x : altOddEven xs

rule2 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule2 (t1,t2,t3,t4,t5,t6)
  | altOddEven (tupleToList (t1,t2,t3,t4,t5,t6)) == [True,False,True,False,True,False] = True
  | altOddEven (tupleToList (t1,t2,t3,t4,t5,t6)) == [False,True,False,True,False,True] = True
  | otherwise = False


-- 1.3
diff :: Int -> Int -> Int
-- Finding the difference between two consecutive numbers 
-- Ensuring result is always positive
diff a b 
  |a < b = negate(a - b)
  |otherwise = a - b
  
rule3 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule3 (t1,t2,t3,t4,t5,t6)
    = diff t1 t2 > 2
    && diff t2 t3 > 2
    && diff t3 t4 > 2
    && diff t4 t5 > 2
    && diff t5 t6 > 2


-- 1.4
combineNumbers :: Int -> Int -> Int
-- This function will take two numbers and combine them to make one
-- e.g. a = 6, b = 7
-- 6*10 = 60 , 60 + 7 = 67
combineNumbers a b 
  = (a*10) + b

rule4 :: (Int,Int,Int,Int,Int,Int) -> Bool
rule4 (t1,t2,t3,t4,t5,t6)
  = (combineNumbers t1 t2) `mod` (combineNumbers t5 t6) == 0
  && combineNumbers t3 t4 `mod` (combineNumbers t5 t6) == 0


-- 1.5
possibilities :: Int -> (Int,Int,Int,Int,Int,Int)
-- Quote is integer division, it divides the first argument by the second one discarding remainder 
possibilities x = (x `quot` 100000, 
                  (mod x 100000) `quot` 10000, 
                  (mod x 10000) `quot` 1000, 
                  (mod x 1000) `quot`100, 
                  (mod x 100) `quot`10, 
                  (mod x 10) `quot` 1)

possibles :: [(Int,Int,Int,Int,Int,Int)]
possibles = map possibilities[0..999999]


-- 1.6
-- Combining all four rules to check solutions validity
isSolution :: (Int,Int,Int,Int,Int,Int) -> Bool
isSolution (t1,t2,t3,t4,t5,t6)
  = rule1 (t1,t2,t3,t4,t5,t6)
  && rule2 (t1,t2,t3,t4,t5,t6)
  && rule3 (t1,t2,t3,t4,t5,t6)
  && rule4 (t1,t2,t3,t4,t5,t6)


main :: IO()
main 
  = putStrLn (show (filter isSolution possibles))