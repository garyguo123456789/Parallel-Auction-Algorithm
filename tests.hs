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


printAuctionResults :: PayoffMatrix -> Assignment -> Double -> IO ()
printAuctionResults matrix assignment totalPayoff = do
    putStrLn "Total Payoff Breakdown:"
    let payoffBreakdown = [(bidder, item, matrix !! bidder !! item)
                           | (item, bidder) <- Map.toList assignment]
    mapM_ (\(b, i, p) -> putStrLn $ "Bidder " ++ show b ++ " -> Item " ++ show i ++ ": " ++ show p)
          payoffBreakdown
    putStrLn $ "\nTotal Payoff: " ++ show totalPayoff

runTest :: (Double -> PayoffMatrix -> (Assignment, Double)) -> PayoffMatrix -> Double -> IO ()
runTest algorithm matrix epsilon = do
    let (assignment, totalPayoff) = algorithm epsilon matrix

    putStrLn "--------------------------------- \nTest results:"
    print totalPayoff


main :: IO ()
main = do
    let seed = 42
        gen = mkStdGen seed
        (matrix, _) = generateMatrix gen 1000 1000
    
    -- matrix `seq` return ()

    putStrLn "\n------- sequential test -------"
    runTest jacobiAuctionAlgorithm matrix 0.01

    -- putStrLn "\n------- jacobi test -------"
    -- runTest jacobiAuctionAlgorithm matrix 0.01

    -- putStrLn "\n------- gs test -------"
    -- runTest gsAuctionAlgorithm matrix 0.01


-- Generate a random payoff matrix
generateMatrix :: System.Random.StdGen -> Int -> Int -> (PayoffMatrix, System.Random.StdGen)
generateMatrix gen rows cols = (matrix, finalGen)
    where randomNumbers = take (rows * cols) $ System.Random.randomRs (0.0, 100.0) gen
          (finalGen, _) = System.Random.split gen
          matrix = take rows $ chunksOf cols randomNumbers

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (ys, zs) = splitAt n xs in ys : chunksOf n zs
