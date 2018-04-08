-- Compile using: ghc greedy.hs
import Control.Monad
import Data.Function
import Data.List
import Data.Set (Set)

import qualified Data.Set as Set

type Input   = Int
type Node    = (Input, Input)
type Goal    = Set Node
type Circuit = Set Node

toNodes :: [Int] -> [Node]
toNodes = go . zip [0..]
  where
    go []          = []
    go ((_, 0):xs) = go xs
    go ((x, 1):xs) = (x, x + length ones) : go rest
      where
        (ones, rest) = span ((==1) . snd) xs

toGoal :: [[Int]] -> Goal
toGoal = Set.fromList . concatMap toNodes

inputs :: Int -> Circuit
inputs n = Set.fromList $ map (\x -> (x, x)) [0..(n - 1)]

-- Add a new node to the circuit by concatenating two existing nodes, hitting
-- as many nodes of the goal as possible. Returns Nothing if the goal has
-- already been reached.
greedyStep :: Goal -> Circuit -> Maybe (Int, Node)
greedyStep goal circuit
    | goal `Set.isSubsetOf` circuit = Nothing
    | otherwise = Just $ maximum nodes
  where
    nodes = [ (length $ filter (subNode c) (Set.toList goal), c)
            | x <- Set.elems circuit, y <- Set.elems circuit, snd x + 1 == fst y
            , let c = (fst x, snd y), c `Set.notMember` circuit ]
    subNode (x1, x2) (y1, y2) = x1 >= y1 && x2 <= y2

greedy :: Goal -> Circuit -> IO [Node]
greedy goal circuit = case greedyStep goal circuit of
    Nothing -> return []
    Just (gain, node) -> do
        putStrLn $ "Add node " ++ show node ++ " with gain = " ++ show gain
        (node :) <$> greedy goal (Set.insert node circuit)

diagonalZeroes6x6 :: [[Int]]
diagonalZeroes6x6 = [[0,1,1,1,1,1],
                     [1,0,1,1,1,1],
                     [1,1,0,1,1,1],
                     [1,1,1,0,1,1],
                     [1,1,1,1,0,1],
                     [1,1,1,1,1,0]]

custom6x6 :: [[Int]]
custom6x6 = [[1,0,1,1,1,0],
             [1,1,1,1,0,1],
             [0,1,0,1,0,1],
             [1,1,1,0,1,1],
             [0,1,0,1,1,1],
             [1,0,1,0,1,1]]

main :: IO ()
main = do
    let test = custom6x6
    res <- greedy (toGoal test) (inputs $ length test)
    putStrLn $ "Total nodes = " ++ show (length res)
