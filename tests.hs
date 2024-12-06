module Main (main) where

import SequentialAuction (auctionAlgorithm, optimalAssignment)
import JacobiAuction (jacobiAuctionAlgorithm)
import GSAuction (gsAuctionAlgorithm)
import qualified Data.Map as Map
import Control.Monad (unless)

type Bidder = Int
type Item = Int
type PayoffMatrix = [[Double]]
type Prices = Map.Map Item Double
type Assignment = Map.Map Bidder Item

-- same as in SequentialAuction, but prints without the original matrix
printAuctionResults :: PayoffMatrix -> Assignment -> IO ()
printAuctionResults matrix assignment = do
    putStrLn "Assignment (Bidder -> Item):"
    mapM_ (\(bidder, item) -> putStrLn $ "Bidder " ++ show bidder ++ " -> Item " ++ show item)
          (Map.toList assignment)

    putStrLn "\nTotal Payoff Breakdown:"
    let payoffBreakdown = [(bidder, item, matrix !! bidder !! item)
                           | (bidder, item) <- Map.toList assignment]
    mapM_ (\(b, i, p) -> putStrLn $ "Bidder " ++ show b ++ " -> Item " ++ show i ++ ": " ++ show p)
          payoffBreakdown

    let totalPayoff = sum [matrix !! bidder !! item | (bidder, item) <- Map.toList assignment]
    putStrLn $ "\nTotal Payoff: " ++ show totalPayoff


runTest :: (Double -> PayoffMatrix -> Assignment) -> PayoffMatrix -> Double -> IO ()
runTest algorithm matrix epsilon = do
    let assignment = algorithm epsilon matrix
        optimal = optimalAssignment matrix
        testResult = assignment == optimal
    unless testResult $ do -- unless recommended by vscode for more succinct
        putStrLn "--------------------------------- \nTest failed, printing results..."
        putStrLn "\nPayoff Matrix:"
        mapM_ print matrix
        putStrLn "\nActual..."
        printAuctionResults matrix assignment
        putStrLn "\nExpected..."
        printAuctionResults matrix optimal


main :: IO ()
main = do
    let matrices = [ ([[10.0, 5.0, 8.0],
                       [7.0, 9.0, 5.0],
                       [20.0, 7.0, 10.0]], 0.01),

                     ([[10.0, 15.0, 20.0, 5.0],
                       [5.0, 10.0, 15.0, 20.0],
                       [20.0, 5.0, 10.0, 15.0],
                       [15.0, 20.0, 5.0, 10.0]], 0.01),

                    -- not sure how we would deal with this case anyways
                    --  ([[0.0, 0.0, 0.0],
                    --    [0.0, 0.0, 0.0],
                    --    [0.0, 0.0, 0.0]], 0.1),

                     ([[1.0, 2.0, 3.0, 4.0],
                       [4.0, 3.0, 2.0, 1.0],
                       [0.0, 1.0, 0.0, 1.0],
                       [1.0, 0.0, 1.0, 0.0]], 0.01),

                     ([[10.0, 5.0, 2.0, 1.0],
                       [3.0, 8.0, 7.0, 5.0],
                       [9.0, 6.0, 3.0, 2.0],
                       [8.0, 7.0, 4.0, 1.0]], 0.01)
                   ]

    putStrLn "\n----------------- sequential tests -----------------"
    mapM_ (uncurry (runTest auctionAlgorithm)) matrices -- vscode recommended uncurry

    putStrLn "\n----------------- jacobi tests -----------------"
    mapM_ (uncurry (runTest auctionAlgorithm)) matrices

    putStrLn "\n----------------- GS tests -----------------"
    mapM_ (uncurry (runTest auctionAlgorithm)) matrices
