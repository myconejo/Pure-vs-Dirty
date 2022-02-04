import System.IO

data Tree = N | E Integer Tree Tree deriving(Show, Eq)
-- tree: null | int left right

prompt :: String -> IO ()
prompt str = (putStr str) >>= (\x -> hFlush stdout)

promptLn :: String -> IO ()
promptLn str = (putStrLn str) >>= (\x -> hFlush stdout)

t1 :: Tree
t1 = put (put (put (put (put (put (put (put (put (put N 5) 3) 8) 1) 4) 6) 1) 9) 7) 2

-- getKey: get the key of the tree
getKey :: Tree -> Integer
getKey N = -1
getKey (E x _ _) = x

showKey :: Integer -> String
showKey k
    | k < 0     = "(null)"
    | otherwise = show k

-- getLeft: get the left subtree
getLeft :: Tree -> Tree
getLeft N = N
getLeft (E _ t _) = t

-- getRight: get the right subtree
getRight :: Tree -> Tree
getRight N = N
getRight (E _ _ t) = t

-- contains: return true if the tree has the key, else return false
contains :: Tree -> Integer -> Bool
contains N _ = False
contains (E key lt rt) x 
    | key == x = True              -- search hit
    | key < x  = contains rt x     -- search the right tree
    | key > x  = contains lt x     -- search the left tree

-- put: put the value in the tree as a BST
put :: Tree -> Integer -> Tree
put N x = E x N N
put (E key lt rt) x
    | key == x = E key lt rt
    | key  < x = E key lt (put rt x)
    | key  > x = E key (put lt x) rt

-- lfm: leftmost node of the root
lfm :: Tree -> Tree
lfm (E key N rt) = (E key N rt) 
lfm (E key lt _) = lfm lt       -- recursive case
    
-- suc: find the successor of the root
suc :: Tree -> Tree
suc N               = N            -- null tree has no successor
suc (E key _ N)     = N            -- when there is no successor
suc (E key _ rt)    = lfm rt       -- leftmost of its subtree

-- pop: remove the node with the key = k
pop :: Tree -> Integer -> Tree
pop N _ = N
pop (E key lt rt) x
    | key == x = rep $ E key lt rt
    | key  < x = E key lt (pop rt x)
    | key  > x = E key (pop lt x) rt

-- rep: actual action of removing the node
rep :: Tree -> Tree
-- if the node has a single child, just connect it
rep (E key N rt)    = rt                          
rep (E key lt N)    = lt                        
-- if the node has both child, replace the value with the successor   
rep (E key lt rt)   = (E (getKey $ lfm rt) lt (rep $ lfm rt)) 

-- trv: inorder traversal of the tree
trv :: Tree -> String
trv N   = " empty tree "
trv t   = iot t

iot :: Tree -> String
iot N               = ""
iot (E key lt rt)   = (iot lt) ++ " " ++ (show key) ++ " " ++ (iot rt)

editTree :: Tree -> IO ()
editTree t = do
    prompt ">> enter a command: "
    cmd <- getLine
    case cmd of
        "put"       -> do
            prompt ">> enter an integer to put: "
            key <- getLine
            let k = read key :: Integer
            promptLn $ ">>" ++ (trv $ put t k)
            editTree $ put t k
        "pop"       -> do
            prompt ">> enter the key to pop: "
            key <- getLine
            let k = read key :: Integer
            promptLn $ ">>" ++ (trv $ pop t k)
            editTree $ pop t k
        "root"      -> do
            promptLn $ ">> the key of the root is " ++ (showKey $ getKey t)
            editTree t
        "traverse"  -> do
            promptLn ">> traversing the tree"
            promptLn $ "[" ++ (trv t) ++ "]"
            editTree t
        "reset"     -> do
            promptLn ">> resetting the tree..."
            editTree N
        "exit"      -> return()
        "--help"    -> do
            promptLn "--------------------------------------------------------------------"
            promptLn "    exit                - exit the tree editing"
            promptLn "    traverse            - inorder traversal of the tree "
            promptLn "    reset               - reset the list"
            promptLn "    root                - get the key of the root"
            promptLn "    put                 - put a key into a tree"
            promptLn "    pop                 - pop a given key from the tree"
            promptLn "--------------------------------------------------------------------"
            editTree t
        _           -> do
            promptLn $ ">> no such command <" ++ cmd ++ ">"
            promptLn ">> use --help to see the list of commands"
            editTree t


main :: IO ()
main = do
    promptLn "--------------------------------------------------------------------"
    promptLn "Binary Search Tree in Haskell"
    editTree N
    promptLn "--------------------------------------------------------------------"
