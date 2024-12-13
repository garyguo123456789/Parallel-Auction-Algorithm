import System.Process (readProcessWithExitCode)
import System.IO (writeFile, appendFile)
import Control.Monad (forM_)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Control.Monad (replicateM)

-- get the value associated with an item in the "Measure-Command" summary
extractValue :: String -> String -> String
extractValue prefix output =
    case filter (prefix `isPrefixOf`) (lines output) of
        (x:_) -> words x !! 2 -- get the third word, the value after the prefix
        _     -> "ISSUE PARSING OUTPUT"

-- get the value of double from the string, using maybe to catch incorrect parsing
parseDouble :: String -> Maybe Double
parseDouble str = readMaybe str :: Maybe Double

main :: IO ()
main = do
    let executable = "C:\\Users\\avape\\Parallel-Auction-Algorithm\\tests.exe"
        outputFile = "execution_times.txt"
        coresRange = [1..10]
        numRuns = 5 -- number of runs to avg over

    writeFile outputFile "Num cores, Average runtime (s)\n"
    -- for loop in monad land
    forM_ coresRange $ \cores -> do
        putStrLn $ "Running with " ++ show cores ++ " core(s)..."
        totalSecondsList <- replicateM numRuns (do
            -- THIS PART DOES NOT WORK IF OPERATING A NON-WINDOWS MACHINE
            (_, stdout, _) <- readProcessWithExitCode
                "powershell"
                ["-Command", "Measure-Command { " ++ executable ++ " +RTS -N" ++ show cores ++ " }"] -- command for Windows
                ""
            let totalSecondsStr = extractValue "TotalSeconds" stdout
            case parseDouble totalSecondsStr of
                Just value -> return value
                Nothing    -> return 0.0) -- just use zero if something went wrong with parsing

        let averageTotalSeconds = sum totalSecondsList / fromIntegral numRuns
        appendFile outputFile $ show cores ++ ", " ++ show averageTotalSeconds ++ "\n"

