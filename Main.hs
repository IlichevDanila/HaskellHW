import Data.Char
import System.Environment (getArgs)

type Memory = [Int]
type Instructions = String

getPairedBracketIdx :: Instructions -> Int -> Int
getPairedBracketIdx (']':xs) 1 = 1
getPairedBracketIdx ('[':xs) count = 1 + getPairedBracketIdx xs (count + 1)
getPairedBracketIdx (_:xs) count = 1 + getPairedBracketIdx xs count

extractCycle :: Instructions -> (Instructions, Instructions)
extractCycle x = let (cycle, remain) = splitAt (getPairedBracketIdx x 0) x in
    (tail (take (length cycle - 1) cycle), remain)

data State = State {memory :: Memory}

first :: State -> Int
first = head . memory

init_state :: State
init_state = State $ take 10 $ repeat 0

#ifdef ASCII_IO

get :: State -> IO State
get state = do
    c <- getChar
    return $ let (x:xs) = (memory state) in State ((ord c):xs)

put :: State -> IO State
put state = do
    putChar $ chr $ head $ memory state
    return state

#else

get :: State -> IO State
get state = do
    x <- fmap read getLine
    return $ let (_:xs) = (memory state) in State (x:xs)

put :: State -> IO State
put state = do
    print $ first state
    return state
    
#endif

add :: State -> IO State
add state = do
    let x:xs = memory state in
        if x == 255
        then return $ State (0:xs)
        else return $ State ((x+1):xs)

sub :: State -> IO State
sub state = do
    let x:xs = memory state in
        if x == 0
        then return $ State (255:xs)
        else return $ State ((x-1):xs)

moveRight :: State -> IO State
moveRight state =
    let x:xs = memory state in
        return $ State (xs ++ [x])

moveLeft :: State -> IO State
moveLeft state =
    let mem = memory state in
        let (head, last) = splitAt (length mem - 1) mem in
            return $ State (last ++ head)

process :: State -> Instructions -> IO State

untilZero :: State -> Instructions -> IO State
untilZero state cs
    | first state == 0 = return state
    | otherwise = do
        new_state <- process state cs
        untilZero new_state cs

process state [] = return state

process state ('.':cs) = do
    new_state <- put state
    process new_state cs

process state (',':cs) = do
    new_state <- get state
    process new_state cs

process state ('+':cs) = do
    new_state <- add state
    process new_state cs

process state ('-':cs) = do
    new_state <- sub state
    process new_state cs

process state ('<':cs) = do
    new_state <- moveLeft state
    process new_state cs

process state ('>':cs) = do
    new_state <- moveRight state
    process new_state cs

process state ('[':cs) = do
    let (cycle, remain) = extractCycle ('[':cs) in
        do
            new_state <- untilZero state cycle
            process new_state remain

process state (']':cs) = do
    putStrLn "Excess ']' encountered"
    process state cs


main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
    then putStrLn "Should pass one argument: name of the file"
    else do
        content <- readFile $ head args
        putStrLn content
        _ <- process init_state (filter (`elem` "+-><,.[]") content)
        putStrLn ""
        return ()
