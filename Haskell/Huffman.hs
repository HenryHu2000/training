module Huffman
    ( frequencies
    , encode
    , decode
    , Bit (..)
    ) where

import Data.List
import Data.Maybe

data Bit = Z | O deriving (Eq, Show)

data Tree a = Node Int (Tree a) (Tree a) | Leaf Int a
            deriving (Show)

frequency :: Ord a => a -> [a] -> Int
frequency _ [] = 0
frequency key (x : xs)
    | key == x  = frequency key (xs) + 1
    | otherwise = frequency key (xs)

-- | Calculate symbol frequencies of a text.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies list
    = map (\x -> (x, frequency x list)) (nub list)

getTreeFrequency :: Tree a -> Int
getTreeFrequency (Node freq _ _) = freq
getTreeFrequency (Leaf freq _)   = freq

initializeQueue :: [(a, Int)] -> [Tree a]
initializeQueue freqTable
    = sortOn getTreeFrequency (map toTree freqTable)
    where
        toTree (x, freq) = Leaf freq x

showTree' :: Show a => Int -> Tree a -> String
showTree' depth (Node freq left right)
    = (replicate depth ' ') ++ "Freq = " ++ show freq ++ "\n" ++ showTree' (depth + 1) left ++ showTree' (depth + 1) right
showTree' depth (Leaf freq value)
    = (replicate depth ' ') ++ "Freq = " ++ show freq ++ ", Value = " ++ show value ++ "\n"

showTree :: Show a => Tree a -> String
showTree = showTree' 0

-- Pre: tree to insert is a node
insertTreeToQueue :: Tree a -> [Tree a] -> [Tree a]
insertTreeToQueue x [] = x : []
insertTreeToQueue tree (tree' : trees)
    | getTreeFrequency tree <= getTreeFrequency tree' = tree : (tree' : trees)
    | otherwise                                       = tree' : (insertTreeToQueue tree trees)

-- Pre: queue is sorted and not empty
-- Post: queue contains only one element
reduceTreeQueue :: [Tree a] -> [Tree a]
reduceTreeQueue []       = []
reduceTreeQueue (x : []) = x : []
reduceTreeQueue (tree : tree' : trees)
    = reduceTreeQueue (insertTreeToQueue (Node (getTreeFrequency tree + getTreeFrequency tree') tree tree') trees)

-- Pre: frequency table is not empty
buildTree :: [(a, Int)] -> Tree a
buildTree = head . reduceTreeQueue . initializeQueue

treeToEncodingTable' :: Tree a -> [Bit] -> [(a, [Bit])] -> [(a, [Bit])]
treeToEncodingTable' (Leaf freq value) currSequence table
    = (value, currSequence) : table
treeToEncodingTable' (Node freq left right) currSequence table
    = ((treeToEncodingTable' left (currSequence ++ [Z])) . (treeToEncodingTable' right (currSequence ++ [O]))) table

treeToEncodingTable :: Tree a -> [(a, [Bit])]
treeToEncodingTable tree = treeToEncodingTable' tree [] []

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode [] _       = Nothing
encode (_ : []) _ = Nothing
encode freqTable string
    = Just (concat (mapMaybe ((flip lookup) encodingTable) string))
    where encodingTable = (treeToEncodingTable . buildTree) freqTable

decode' :: Tree a -> Tree a -> [Bit] -> [a]
decode' origTree (Node freq left right) (bit : bits)
    | bit == Z  = decode' origTree left bits
    | otherwise = decode' origTree right bits
decode' origTree (Leaf freq value) bits
    = value : decode' origTree origTree bits
decode' _ _ [] = []

-- | Decode a bit sequence using the given frequencies.
decode :: [(a, Int)] -> [Bit] -> Maybe [a]
decode [] _       = Nothing
decode (_ : []) _ = Nothing
decode freqTable string
    = Just (decode' tree tree string)
    where tree = buildTree freqTable
