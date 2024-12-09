module Main (main) where

import SequentialAuction (auctionAlgorithm, optimalAssignment)
import JacobiAuction (jacobiAuctionAlgorithm)
import GSAuction (gsAuctionAlgorithm)
import qualified Data.Map as Map
import Control.Monad (unless)
import System.Random (mkStdGen, randomRs, StdGen, split)

type Bidder = Int
type Item = Int
type PayoffMatrix = [[Double]]
type Prices = Map.Map Item Double
type Assignment = Map.Map Bidder Item


printAuctionResults :: PayoffMatrix -> Assignment -> IO ()
printAuctionResults matrix assignment = do
    putStrLn "Total Payoff Breakdown:"
    let payoffBreakdown = [(bidder, item, matrix !! bidder !! item)
                           | (item, bidder) <- Map.toList assignment]
    mapM_ (\(b, i, p) -> putStrLn $ "Bidder " ++ show b ++ " -> Item " ++ show i ++ ": " ++ show p)
          payoffBreakdown

    let totalPayoff = sum [matrix !! bidder !! item | (item, bidder) <- Map.toList assignment]
    putStrLn $ "\nTotal Payoff: " ++ show totalPayoff


runTest :: (Double -> PayoffMatrix -> Double) -> PayoffMatrix -> Double -> IO ()
runTest algorithm matrix epsilon = do
    let totalPayoffAlgo = algorithm epsilon matrix

    putStrLn "--------------------------------- \nTest results:"
    putStrLn $ "\nTotal Payoff Algorithm: " ++ show totalPayoffAlgo

    putStrLn "--------------------------------- \nTest results:"
    -- putStrLn "\nPayoff Matrix:"
    -- mapM_ print matrix
    putStrLn $ "\nTotal Payoff Algorithm: " ++ show totalPayoffAlgo


main :: IO ()
main = do
    let seed = 42
        gen = mkStdGen seed
        (matrix, _) = generateMatrix gen 1000 1000

    -- putStrLn "\n------- jacobi test -------"
    runTest jacobiAuctionAlgorithm matrix 0.01


generateMatrix :: System.Random.StdGen -> Int -> Int -> (PayoffMatrix, System.Random.StdGen)
generateMatrix gen rows cols = (matrix, finalGen)
    where randomNumbers = take (rows * cols) $ System.Random.randomRs (0.0, 100.0) gen
          (finalGen, _) = System.Random.split gen
          matrix = take rows $ chunksOf cols randomNumbers

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (ys, zs) = splitAt n xs in ys : chunksOf n zs
