import System.IO
import Data.List

prompt :: String -> IO ()
prompt str = (putStr str) >>= (\x -> hFlush stdout)

promptLn :: String -> IO ()
promptLn str = (putStrLn str) >>= (\x -> hFlush stdout)

samp :: [Integer]
samp = [6,9,11,7,4,0,3,12,2]

-- mergesort and its helper methods
merge :: [Integer] -> [Integer] -> [Integer]
merge xs []     = xs
merge [] ys     = ys
merge (x:xs) (y:ys) 
    | x <= y    = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

mergeSort :: [Integer] -> [Integer]
mergeSort []    = []        -- null
mergeSort [x]   = [x]       -- single element
mergeSort xs    = merge (mergeSort (take (div (length xs) 2) xs)) (mergeSort (drop (div (length xs) 2) xs))

-- insertion sort and its helper methods
ins :: Integer -> [Integer] -> [Integer]
ins x []        = [x]
ins x (y:ys) 
    | x < y     = x:y:ys
    | otherwise = y:(ins x ys)

insSort :: [Integer] -> [Integer]
insSort []      = []
insSort (x:xs)  = ins x (insSort xs)

-- selection sort and its helper methods
minElem :: [Integer] -> Integer
minElem [x]         = x
minElem (x:xs)
    | x < (head xs) = minElem $ x:(tail xs)
    | otherwise     = minElem $ xs

delElem :: [Integer] -> Integer -> [Integer]
delElem [] _            = []
delElem xs x
    | x == (head xs)    = tail xs
    | otherwise         = (head xs) : delElem (tail xs) x

selSort :: [Integer] -> [Integer]
selSort [] = []
selSort xs = (minElem xs) : selSort (delElem xs (minElem xs))

sorters :: [Integer] -> IO ()
sorters xs = do
    prompt ">> enter a command: "
    cmd <- getLine
    case cmd of
        "exit" -> return()
        "new"  -> do
            promptLn ">> new array created:"
            promptLn $ show samp
            sorters samp
        "sort" -> do
            prompt ">> insertion[I]/selection[S]/merge[M]?: "
            ctrl <- getLine
            case ctrl of
                "I" -> do
                    promptLn ">> insertion sort..."
                    promptLn $ show $ insSort xs
                    sorters $ insSort xs
                "S" -> do
                    promptLn ">> selection sort..."
                    promptLn $ show $ selSort xs
                    sorters $ selSort xs
                "M" -> do
                    promptLn ">> mergesort..."
                    promptLn $ show $ mergeSort xs
                    sorters $ mergeSort xs
                _   -> do
                    sorters xs
        _     -> do
            prompt ">> invalid command!"
            sorters xs

main :: IO()
main = do
    promptLn "+----------------------------------------------------------------------------------------+"
    promptLn ">> Sorting Algorithms in Haskell"
    sorters samp