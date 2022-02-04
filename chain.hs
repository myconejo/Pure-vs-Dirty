import System.IO
import Data.List

--SEPARATE CHAINING IN HASKELL--

data Node   = N | E {key::Int, val::Int, next::Node} deriving(Show, Eq)

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

getHead :: Node -> Node
getHead N = N
getHead (E k v l) = (E k v N)

getNode :: Node -> Int -> Node
getNode N _             = N
getNode (E key val n) k    
    | k == key          = (E key val N)
    | otherwise         = getNode n k

trvNode :: Node -> String
trvNode N = "[_:_]"
trvNode (E k v N) = "[" ++ (show k) ++ ":" ++ (show v) ++ "]"
trvNode (E k v l) = "[" ++ (show k) ++ ":" ++ (show v) ++ "] " ++ (trvNode l)

trvList :: [Node] -> String
trvList []  = "null"
trvList [n] = trvNode n
trvList ns  = (trvNode $ head ns) ++ " | " ++ (trvList $ tail ns)

search :: [Node] -> Int -> String
search [] _ = "!! Empty list (no key-value nodes)"
search ns k
    | this == N = (show k) ++ ": Key not found!"
    | otherwise = (trvNode $ this)
    where this = getNode (ns !! (hashcode k (length ns))) k

--CREATE ROUTINES----------------------------------------------------------------------------------------------

makeArray :: Int -> [Node]
makeArray 0     = []
makeArray x     = N : makeArray (x-1) 

--ACTION ROUTINES----------------------------------------------------------------------------------------------

checkDup :: Node -> Int -> Bool
checkDup N k    = False
checkDup (E key val n) k
    | k == key  = True
    | otherwise = checkDup n k

addFirst :: Node -> Int -> Int -> Node
addFirst N k v  = (E k v N)
addFirst l k v  = 
    case bool of
        False   -> (E k v l)
        True    -> l
    where bool = checkDup l k

delFirst :: Node -> Node
delFirst N          = N 
delFirst (E k v l)  = l  

delNode :: Node -> Int -> Node
delNode N _             = N
delNode (E k v l) key
    | k == key          = l
    | otherwise         = (addFirst (delNode l key) k v)

put :: [Node] -> Int -> Int -> [Node]
put [] _ _ = []
put ns k v = (take h ns) ++ [addFirst (ns !! h) k v] ++ (drop (h+1) ns)
    where h = hashcode k (length ns)

pop :: [Node] -> Int -> [Node]
pop [] _ = []
pop ns k = (take h ns) ++ [delNode (ns !! h) k] ++ (drop (h+1) ns)
    where h = hashcode k (length ns)

--MAIN ROUTINE-------------------------------------------------------------------------------------------------

hash :: [Node] -> IO()
hash ns = do
    prompt ">> Enter a command: "
    cmd <- getLine
    case cmd of 
        "exit"      -> 
            return()
        "reset"     -> do
            promptLn ">> Resetting the array..."
            promptLn $ trvList $ makeArray 13
            hash $ makeArray 13
        "make"      -> do
            promptLn ">> Enter a size of the array: "
            size    <- getLine
            let s = read size::Int
            promptLn $ trvList $ makeArray s
            hash $ makeArray s
        "put"       -> do
            promptLn ">> Putting a key-val pair to the list"
            prompt "  $ Enter a key to put: "
            k <- getLine
            prompt "  $ Enter a corresponding value: "
            v <- getLine
            let key  = read k::Int
            let val  = read v::Int  
            promptLn $ trvList $ put ns key val
            hash     $ put ns key val
        "pop"       -> do
            prompt "  $ Enter a key to remove: "
            k <- getLine
            let key = read k::Int
            promptLn $ trvList $ pop ns key
            hash     $ pop ns key
        "trv"       -> do
            promptLn $ trvList ns
            hash ns
        "find"      -> do
            promptLn ">> Find the key and get its value"
            prompt "  $ Enter a key to find: "
            k <- getLine
            let key  = read k::Int
            promptLn $ "  # " ++ (search ns key)
            hash ns
        "--help"    -> do
            promptLn "+---------------------------------------------------------------+"
            promptLn "| $ exit  - exit the program                                    |"                                   
            promptLn "| $ reset - reset the list                                      |"
            promptLn "| $ make  - make an empty list                                  |"
            promptLn "| $ put   - put a key-value pair to the list                    |"
            promptLn "| $ pop   - remove a key-value pair from the list               |"
            promptLn "| $ find  - find a key-value pair from the list                 |"
            promptLn "| $ trv   - traverse the list                                   |"
            promptLn "+---------------------------------------------------------------+"
            hash ns
        _           -> do
            promptLn $ "!! <" ++ cmd ++ "> is not a valid command"
            promptLn $ "!! use --help to see the list of commands"
            hash ns

main :: IO ()
main = do
    promptLn "-----------------------------------------------------------------"
    promptLn "Hash with Haskell"
    promptLn "## use --help to see the list of commands"
    hash $ makeArray 13
    promptLn "-----------------------------------------------------------------"
    
