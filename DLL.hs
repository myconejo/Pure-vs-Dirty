import System.IO
import Data.List

data List = N | E Integer List deriving(Show, Eq)
-- E is a void type (Null)
-- N is a node with a integer value and the next node

-- list1: recursively defined list
newlist :: List
newlist = N

list1 :: List
list1 = E 3 $ E 8 $ E 2 $ E 6 $ E 5 $ N 

-- binder: get the internal value from the maybe monad
binder :: Maybe Integer -> Integer
binder mx = case mx of
    Just x -> x
    Nothing -> -1

-- getHead: get the first element of the list
getHead :: List -> Maybe Integer
getHead N = Nothing
getHead (E x _) = Just x

-- getTail: get the tail list of the list
getTail :: List -> List
getTail N = N
getTail (E _ l) = l

-- getLast: get the last element of the list
getLast :: List -> Maybe Integer
getLast N = Nothing
getLast (E x N) = Just x
getLast (E _ n) = getLast(n)

-- addFirst: insert an integer element at the head
addFirst :: List -> Integer -> List
addFirst N x = (E x N)
addFirst l x = (E x l)

-- delFirst: delete the first element of the list
delFirst :: List -> List
delFirst N = N
delFirst (E x l) = l

-- addLast: add the element at the last
addLast :: List -> Integer -> List
addLast N x = (E x N)
addLast l x = addFirst (addLast (delFirst l) x) (binder $ getHead l)

-- delLast: delete the last element of the list
delLast :: List -> List
delLast N = N
delLast (E x N) = N
delLast l = addFirst (delLast $ delFirst l) (binder $ getHead l)

-- findN: find N-th element of the list
findN :: List -> Integer -> Maybe Integer
findN N _ = Nothing
findN l 0 = getHead l
findN l n = findN (delFirst l) (n-1)

-- repN: replace the N-th node's value
repN :: List -> Integer -> Integer -> List
repN N _ _ = N
repN l 0 x = addFirst (delFirst l) x
repN l n x = addFirst (repN (delFirst l) (n-1) x) (binder $ getHead l)

-- addN: add a value at the N-th node (thus, nodes from N to last will be pushed by 1)
addN :: List -> Integer -> Integer -> List
addN N _ x = (E x N)
addN l 0 x = addFirst l x
addN l n x = addFirst (addN (delFirst l) (n-1) x) (binder $ getHead l)

-- delN: delete the N-th node
delN :: List -> Integer -> List
delN N _ = N
delN l 0 = delFirst l
delN l n = addFirst (delN (delFirst l) (n-1)) (binder $ getHead l)

-- toArray: convert the list in to an array
toArray :: List -> [Integer]
toArray N = []
toArray l = sort $ (binder $ getHead l) : (toArray $ getTail l)

-- toList: convert the array in to a list
toList :: [Integer] -> List
toList [] = N
toList xs = addFirst (toList $ tail xs) (head xs)

-- sortList: sort the list
sortList :: List -> List
sortList l = (toList.toArray) l

-- trv: traversing the list
trv :: List -> IO ()
trv N = promptLn ""
trv l = do
    prompt $ show $ binder $ getHead l
    prompt " "
    trv $ delFirst l

prompt :: String -> IO ()
prompt str = (putStr str)
    >>= (\x -> hFlush stdout)

promptLn :: String -> IO ()
promptLn str = (putStrLn str)
    >>= (\x -> hFlush stdout)

editList :: List -> IO ()
editList l = do
    prompt ">> enter a command: "
    cmd <- getLine
    case cmd of 
        "exit"      -> do
            prompt ">> final list: "
            trv l

        "traverse"  -> trv l >>= (\x -> editList l)

        "reset"     -> do
            promptLn ">> are you sure? [Y]/[N]"
            prompt ">> "
            yn <- getLine
            case yn of 
                "Y"     -> do
                    promptLn "resetting the list..."
                    editList newlist
                "yes"   -> do
                    promptLn "resetting the list..."
                    editList newlist
                _       -> do
                    editList l

        "--help"    -> do
            promptLn "--------------------------------------------------------------------"
            promptLn "  exit                - exit the list editing"
            promptLn "  traverse            - traverse the current list"
            promptLn "  reset               - reset the current list to an empty list"
            promptLn "  append              - add an integer at the last of the list"
            promptLn "  pop                 - delete the last element of the list"
            promptLn "  enqueue             - add an integer at the first of the list"
            promptLn "  dequeue             - delete the first element of the list"
            promptLn "  replace             - replace an element in the list"
            promptLn "  insert              - insert an element at the desired index"
            promptLn "  delete              - delete an element at the desired index"
            promptLn "  sort                - sort the list"
            promptLn "--------------------------------------------------------------------"
            editList l

        "append"    -> do
            prompt ">> enter an integer to append: "
            cx <- getLine
            let x = (read cx :: Integer)
            trv $ addLast l x
            editList $ addLast l x

        "pop"       -> do
            promptLn ">> last element popped"
            trv $ delLast l
            editList $ delLast l

        "enqueue"   -> do
            prompt ">> enter an integer to enqueue: "
            cx <- getLine
            let x = (read cx :: Integer)
            trv $ addFirst l x
            editList $ addFirst l x

        "dequeue"   -> do
            promptLn ">> first element dequeued"
            trv $ delFirst l
            editList $ delFirst l

        "insert"    -> do
            prompt ">> enter the index to add an element: "
            cn <- getLine
            prompt $ ">> enter the integer to be added at postion [" ++ cn ++ "]: "
            cx <- getLine
            let n = (read cn :: Integer)
            let x = (read cx :: Integer)
            trv $ addN l n x
            editList $ addN l n x

        "sort"      -> do
            promptLn ">> sorting the list..."
            trv $ sortList l
            editList $ sortList l

        "replace"   -> do
            prompt ">> enter the index to replace an element: "
            cn <- getLine
            prompt $ ">> enter the integer to replace at postion [" ++ cn ++ "]: "
            cx <- getLine
            let n = (read cn :: Integer)
            let x = (read cx :: Integer)
            trv $ repN l n x
            editList $ repN l n x
            
        "delete"    -> do
            prompt ">> enter the index to delete an element: "
            cn <- getLine
            let n = (read cn :: Integer)
            trv $ delN l n
            editList $ delN l n
            
        _           -> do
            promptLn $ ">> no such command <" ++ cmd ++ ">"
            promptLn $ ">> use --help to see the list of the commands"
            editList l
    
main :: IO ()
main = do
    promptLn "--------------------------------------------------------------------"
    promptLn "Dynamic List in Haskell"
    editList newlist
    promptLn "--------------------------------------------------------------------"
    return()
