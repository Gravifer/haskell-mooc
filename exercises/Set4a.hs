-- Exercise set 4a:
--
--  * using type classes
--  * working with lists
--
-- Type classes you'll need
--  * Eq
--  * Ord
--  * Num
--  * Fractional
--
-- Useful functions:
--  * maximum
--  * minimum
--  * sort

module Set4a where

import Mooc.Todo
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Array

------------------------------------------------------------------------------
-- Ex 1: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--   allEqual [] ==> True
--   allEqual [1,2,3] ==> False
--   allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!

allEqual :: Eq a => [a] -> Bool
allEqual xs = case xs of
    [] -> True
    (x:_) -> all (== x) xs

------------------------------------------------------------------------------
-- Ex 2: implement the function distinct which returns True if all
-- values in a list are different.
--
-- Hint: a certain function from the lecture material can make this
-- really easy for you.
--
-- Examples:
--   distinct [] ==> True
--   distinct [1,1,2] ==> False
--   distinct [1,2] ==> True

distinct :: Eq a => [a] -> Bool
distinct xs = case xs of
    [] -> True
    _  -> length xs == length (nub xs)

------------------------------------------------------------------------------
-- Ex 3: implement the function middle that returns the middle value
-- (not the smallest or the largest) out of its three arguments.
--
-- The function should work on all types in the Ord class. Give it a
-- suitable type signature.
--
-- Examples:
--   middle 'b' 'a' 'c'  ==> 'b'
--   middle 1 7 3        ==> 3

middle :: Ord a => a -> a -> a -> a
middle a b c  = sort [a, b, c] !! 1 -- 'cuz why not
-- middle a b c = let
--   ab = compare a b -- compares are actually more expensive than boolean `<=`s
--   bc = compare b c
--   ca = compare c a
--   in case (ab, ac, bc) of -- manually dispatched
--     (EQ, EQ, _ ) -> a -- allEq promised by Ord
--     -- (EQ, _ , EQ) -> c
--     -- (_ , EQ, EQ) -> b
--     (GT, GT, _ ) -> b
--     (GT, _ , GT) -> a
--     (_ , GT, GT) -> c
--     (LT, LT, _ ) -> b
--     (LT, _ , LT) -> a
--     (_ , LT, LT) -> c

------------------------------------------------------------------------------
-- Ex 4: return the range of an input list, that is, the difference
-- between the smallest and the largest element.
--
-- Your function should work on all suitable types, like Float and
-- Int. You'll need to add _class constraints_ to the type of range.
--
-- It's fine if your function doesn't work for empty inputs.
--
-- Examples:
--   rangeOf [4,2,1,3]          ==> 3
--   rangeOf [1.5,1.0,1.1,1.2]  ==> 0.5

rangeOf :: (Ord a, Num a) => [a] -> a
rangeOf [] = todo
rangeOf xs = maximum xs - minimum xs

------------------------------------------------------------------------------
-- Ex 5: given a (non-empty) list of (non-empty) lists, return the longest
-- list. If there are multiple lists of the same length, return the list that
-- has the smallest _first element_.
--
-- (If multiple lists have the same length and same first element,
-- you can return any one of them.)
--
-- Give the function "longest" a suitable type.
--
-- Challenge: Can you solve this exercise without sorting the list of lists?
--
-- Examples:
--   longest [[1,2,3],[4,5],[6]] ==> [1,2,3]
--   longest ["bcd","def","ab"] ==> "bcd"

longest :: Ord a => [[a]] -> [a]
longest [] = todo
longest ls = maximumBy (comparing (\l -> (length l, Down (head l)))) ls

------------------------------------------------------------------------------
-- Ex 6: Implement the function incrementKey, that takes a list of
-- (key,value) pairs, and adds 1 to all the values that have the given key.
--
-- You'll need to add _class constraints_ to the type of incrementKey
-- to make the function work!
--
-- The function needs to be generic and handle all compatible types,
-- see the examples.
--
-- Examples:
--   incrementKey True [(True,1),(False,3),(True,4)] ==> [(True,2),(False,3),(True,5)]
--   incrementKey 'a' [('a',3.4)] ==> [('a',4.4)]

incrementKey :: (Eq k, Num v) => k -> [(k,v)] -> [(k,v)]
incrementKey key = map increment
  where
    increment (k, v) = if k == key then (k, v + 1) else (k, v)

------------------------------------------------------------------------------
-- Ex 7: compute the average of a list of values of the Fractional
-- class.
--
-- There is no need to handle the empty list case.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

------------------------------------------------------------------------------
-- Ex 8: given a map from player name to score and two players, return
-- the name of the player with more points. If the players are tied,
-- return the name of the first player (that is, the name of the
-- player who comes first in the argument list, player1).
--
-- If a player doesn't exist in the map, you can assume they have 0 points.
--
-- Hint: Map.findWithDefault can make this simpler
--
-- Examples:
--   winner (Map.fromList [("Bob",3470),("Jane",2130),("Lisa",9448)]) "Jane" "Lisa"
--     ==> "Lisa"
--   winner (Map.fromList [("Mike",13607),("Bob",5899),("Lisa",5899)]) "Lisa" "Bob"
--     ==> "Lisa"

winner :: Map.Map String Int -> String -> String -> String
winner scores player1 player2 =
  let [s1, s2] = map (\p -> Map.findWithDefault 0 p scores) [player1, player2]
  in if s1 >= s2 then player1 else player2
-- winner scores player1 player2 = go . compare (Map.findWithDefault 0 player1 scores) (Map.findWithDefault 0 player2 scores)
--   where
--     go GT = player1
--     go EQ = player1
--     go LT = player2

------------------------------------------------------------------------------
-- Ex 9: compute how many times each value in the list occurs. Return
-- the frequencies as a Map from value to Int.
--
-- Challenge 1: try using Map.alter for this
--
-- Challenge 2: use foldr to process the list
--
-- Example:
--   freqs [False,False,False,True]
--     ==> Map.fromList [(False,3),(True,1)]

freqs :: (Eq a, Ord a) => [a] -> Map.Map a Int
freqs = foldr (Map.alter inc) Map.empty -- (Map.alter inc) :: a -> Map.Map a Int -> Map.Map a Int
  where
    inc Nothing  = Just 1
    inc (Just n) = Just (n + 1)
-- {-# LANGUAGE TupleSections #-}
-- (, 0)
-- freqs xs = let ans = (Map.fromList . map (\k -> (k, 0 :: Int)) $ nub xs)
--   in foldr (Map.alter go) ans xs
--   where go Nothing = Just 1
--         go (Just n) = Just (n + 1)
-- freqs xs = Map.fromListWith (+) [(x, 1) | x <- xs] -- O(n) -- * see https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Map-Internal.html#v:fromListWith
-- freqs xs = Map.fromList [(x, count) | x <- nub xs, let count = length (filter (== x) xs)] -- O(n^2)

------------------------------------------------------------------------------
-- Ex 10: recall the withdraw example from the course material. Write a
-- similar function, transfer, that transfers money from one account
-- to another.
--
-- However, the function should not perform the transfer if
-- * the from account doesn't exist,
-- * the to account doesn't exist,
-- * the sum is negative,
-- * or the from account doesn't have enough money.
--
-- Hint: there are many ways to implement this logic. Map.member or
-- Map.notMember might help.
--
-- Examples:
--   let bank = Map.fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Mike" 20 bank
--     ==> fromList [("Bob",80),("Mike",70)]
--   transfer "Bob" "Mike" 120 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Lisa" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Lisa" "Mike" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]

-- * for reference
-- withdraw :: String -> Int -> Map.Map String Int -> Map.Map String Int
-- withdraw account amount bank =
--   case Map.lookup account bank of
--     Nothing  -> bank                                   -- account not found, no change
--     Just sum -> Map.insert account (sum-amount) bank   -- set new balance

transfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer from to amount bank = 
  case (Map.lookup from bank, Map.lookup to bank) of
    (Nothing, _) -> bank  -- from account not found, no change
    (_, Nothing) -> bank  -- to account not found, no change
    (Just fromSum, Just toSum)
      | fromSum < amount || amount < 0 -> bank  -- invalid transfer conditions, no change
      | otherwise -> Map.insert from (fromSum - amount) $ Map.insert to (toSum + amount) bank

------------------------------------------------------------------------------
-- Ex 11: given an Array and two indices, swap the elements in the indices.
--
-- Example:
--   swap 2 3 (array (1,4) [(1,"one"),(2,"two"),(3,"three"),(4,"four")])
--         ==> array (1,4) [(1,"one"),(2,"three"),(3,"two"),(4,"four")]

swap :: Ix i => i -> i -> Array i a -> Array i a
swap i j arr = arr // [(i, arr ! j),(j, arr ! i)]

------------------------------------------------------------------------------
-- Ex 12: given an Array, find the index of the largest element. You
-- can assume the Array isn't empty.
--
-- You may assume that the largest element is unique.
--
-- Hint: check out Data.Array.indices or Data.Array.assocs

maxIndex :: (Ix i, Ord a) => Array i a -> i
maxIndex arr = let idxs = indices arr -- O(n) time, O(1) space
  in foldl' go (head idxs) (tail idxs)
  where
    go best i = if arr ! i > arr ! best then i else best
-- maxIndex arr = snd $ maximumBy (comparing fst) (assocs arr) -- O(n) time, O(n) space -- (assocs arr) == [(arr ! i, i) | i <- indices arr]
