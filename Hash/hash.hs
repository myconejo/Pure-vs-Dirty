import System.IO
import Data.List

data Pair   = N | E {key::Int, val::Int} deriving(Show, Eq)

--PROMPT ROUTINES----------------------------------------------------------------------------------------------

prompt :: String -> IO ()
prompt str      = (putStr str)      >>= (\x -> hFlush stdout)

promptLn :: String -> IO ()
promptLn str    = (putStrLn str)    >>= (\x -> hFlush stdout)

--HELPER ROUTINES----------------------------------------------------------------------------------------------

hashcode :: Int -> Int -> Int
hashcode key size
    | (size <= 1)       = 0
    | otherwise         = rem key size

hindex :: Int -> Int -> Int -> Int
hindex key trial size = rem ((hashcode key size) + (trial * (hashcode key s))) size where s = (div size 2) + 7

weight :: [Pair] -> Int
weight []       = 0
weight (p:ps)
    | p == N    = weight ps
    | otherwise = 1 + (weight ps)

trv :: [Pair] -> String
trv []                  = "!! Empty Array"
trv [N]                 = "[_:_]"
trv [(E key val)]       = "[" ++ (show key) ++ ":" ++ (show val) ++ "]"
trv (N:ps)              = "[_:_] " ++ trv ps
trv ((E key val):ps)    = "[" ++ (show key) ++ ":" ++ (show val) ++ "] " ++ trv ps

--CREATE ROUTINES----------------------------------------------------------------------------------------------

makeArray :: Int -> [Pair]
makeArray 0             = []
makeArray size          = N : (makeArray $ size - 1)

--ACTION ROUTINES----------------------------------------------------------------------------------------------

put :: [Pair] -> Int -> Int -> [Pair]
put ps k v
    | k < 0 = ps
    | (weight (add ps 0 k v)) <= (div (length ps) 2) = add ps 0 k v
    | otherwise = extend (add ps 0 k v) (makeArray (2 * (length ps)))

add :: [Pair] -> Int -> Int -> Int -> [Pair]
add ps t k v 
    | let h = hindex k t (length ps), (head $ drop h ps) == N = (take h ps) ++ [(E k v)] ++ (drop (h+1) ps)
    | let h = hindex k t (length ps), (key $ head $ drop h ps) == k = ps
    | t > (25 * (length ps)) = (makeArray (length ps))
    | otherwise = add ps (t+1) k v

extend :: [Pair] -> [Pair] -> [Pair]
extend [] ns    = ns
extend (p:ps) ns
    | p == N    = extend ps ns
    | otherwise = extend ps (put ns (key p) (val p))

pop :: [Pair] -> Int -> [Pair]
pop ps k
    | (weight (del ps 0 k)) > (div (length ps) 4) = del ps 0 k
    | length ps <= 8 = del ps 0 k
    | otherwise = shrink (del ps 0 k) (makeArray (div (length ps) 2))

del :: [Pair] -> Int -> Int -> [Pair]
del ps t k
    | let h = hindex k t (length ps), (head $ drop h ps) == N = ps
    | let h = hindex k t (length ps), (key $ head $ drop h ps) == k = (take h ps) ++ [N] ++ (drop (h+1) ps)
    | t > (25 * (length ps)) = (makeArray (length ps))
    | otherwise = del ps (t+1) k

shrink :: [Pair] -> [Pair] -> [Pair]
shrink [] ns    = ns
shrink (p:ps) ns
    | p == N    = shrink ps ns
    | otherwise = shrink ps (put ns (key p) (val p))

finder :: [Pair] -> Int -> String
finder ps k = search ps 0 k

search :: [Pair] -> Int -> Int -> String
search [] _ _ = "No such key"
search ps t k
    | let h = hindex k t (length ps), (head $ drop h ps) == N = "No such key"
    | let h = hindex k t (length ps), (key $ head $ drop h ps) == k = "Found the key " ++ (show $ k) ++ " and its value " ++ (show $ val $ head $ drop h ps)
    | otherwise = search ps (t+1) k

--MAIN ROUTINE-------------------------------------------------------------------------------------------------

hash :: [Pair] -> IO()
hash ps = do
    prompt ">> Enter a command: "
    cmd <- getLine
    case cmd of 
        "exit"      -> 
            return()
        "reset"     -> 
            (promptLn ">> Resetting the array...")
            >>= (\x -> (promptLn $ trv []))
            >>= (\x -> (hash []))
        "make"      -> do
            prompt ">> Enter a size of the array: "
            s <- getLine
            let size = read s::Int
            promptLn $ trv $ makeArray size
            hash $ makeArray size
        "new"       ->
            (promptLn ">> Creating an empty array with length 7")
            >>= (\x -> (promptLn $ trv $ makeArray 7))
            >>= (\x -> (hash $ makeArray 7))
        "put"       -> do
            if (length ps) < 1 then do 
                promptLn "!! Please create an empty array using <make> or <new> first"
                hash ps
            else do
                promptLn ">> Putting a key-val pair to the list"
                prompt "  $ Enter a key to put: "
                k <- getLine
                prompt "  $ Enter a corresponding value: "
                v <- getLine
                let key  = read k::Int
                let val  = read v::Int        
                promptLn $ trv $ put ps key val   
                hash     $ put ps key val
        "pop"       -> do
            if (length ps) < 1 then do 
                promptLn "!! Please create an empty array using <make> or <new> first"
                hash ps
            else do
                prompt "  $ Enter a key to remove: "
                k <- getLine
                let key = read k::Int
                promptLn $ trv $ pop ps key
                hash     $ pop ps key
        "trv"       ->  do
            promptLn $ trv ps
            hash ps
        "find"      -> do
            promptLn ">> Find the key and get its value"
            prompt "  $ Enter a key to find: "
            k <- getLine
            let key  = read k::Int
            promptLn $ "  # " ++ (finder ps key)
            hash ps
        "--help"    -> do
            promptLn "+---------------------------------------------------------------+"
            promptLn "| $ exit  - exit the program                                    |"                                   
            promptLn "| $ reset - reset the list                                      |"
            promptLn "| $ make  - make an empty list                                  |"
            promptLn "| $ new   - make an empty list with length 7                    |"
            promptLn "| $ put   - put a key-value pair to the list                    |"
            promptLn "| $ pop   - remove a key-value pair from the list               |"
            promptLn "| $ find  - find a key-value pair from the list                 |"
            promptLn "| $ trv   - traverse the list                                   |"
            promptLn "+---------------------------------------------------------------+"
            hash ps
        _           -> do
            promptLn $ "!! <" ++ cmd ++ "> is not a valid command"
            promptLn $ "!! use --help to see the list of commands"
            hash ps

main :: IO ()
main = do
    promptLn "-----------------------------------------------------------------"
    promptLn "Hash with Haskell"
    promptLn "## use --help to see the list of commands"
    hash []
    promptLn "-----------------------------------------------------------------"
