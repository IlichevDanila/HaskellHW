import Data.Char (ord, chr)
import System.Environment (getArgs)

--Объявление типов на случай, если мне захочется добавить свою реализацию
type Memory = [Int]
type Instructions = String

{-
Вспомогательные функции для выделения циклов

getPairedBracketIdx возвращает сколько элементов из строки инструкций занимает цикл
extractCycle возвращает внутренности ближайшего цикла и остаток инструкций

extractCycle требует, чтоб первая инструкция в списке была '['

Пример:
    extractCycle "[-<+>]<.>." вернёт ("-<+>", "<.>.")
-}

--TODO: Исправить ситуацию с (']':xs) count где count < 0
getPairedBracketIdx :: Instructions -> Int -> Int
getPairedBracketIdx [] count = 0
getPairedBracketIdx (']':xs) 1 = 1
getPairedBracketIdx (']':xs) count = 1 + getPairedBracketIdx xs (count - 1)
getPairedBracketIdx ('[':xs) count = 1 + getPairedBracketIdx xs (count + 1)
getPairedBracketIdx (_:xs) count = 1 + getPairedBracketIdx xs count

extractCycle :: Instructions -> (Instructions, Instructions)
extractCycle x = let (cycle, remain) = splitAt (getPairedBracketIdx x 0) x in
    (tail (take (length cycle - 1) cycle), remain)

{-
Тип для "состояния" интерпретации. В данный момент содержит только memory,
но при ином устройстве вычислений может содержать и текущее положение каретки на ленте
-}
data State = State {memory :: Memory}


--Возвращает значение под кареткой на ленте
first :: State -> Int
first = head . memory

--Начальное состояние исполнителя (заполненная нулями лента)
init_state :: State
init_state = State $ take 30000 $ repeat 0

{-
Тут пришлось добавить препроцессирование. Дело в том, что в оригинале исполнитель
должен считывать и выводить ascii символы, а вычисление производить над их кодами.

Однако отлаживать программы с подобным поведением довольно трудно, так что если
не определен флаг ASCII_IO, то интерпретатор работает не с ASCII символами, а с
числами
-}
#ifdef ASCII_IO

--Читает ascii-символ с stdin на ячейку под кареткой
get :: State -> IO State
get state = do
    c <- getChar
    return $ let (x:xs) = (memory state) in State ((ord c):xs)

--Выводит ascii-символ с ячейки под кареткой в stdout
put :: State -> IO State
put state = do
    putChar $ chr $ head $ memory state
    return state

#else

--Читает число с stdin на ячейку под кареткой
get :: State -> IO State
get state = do
    x <- fmap read getLine
    return $ let (_:xs) = (memory state) in State (x:xs)

--Выводит число с ячейки под кареткой в stdout
put :: State -> IO State
put state = do
    print $ first state
    return state
    
#endif

--Увеличивает на 1 значение ячейки под кареткой
add :: State -> IO State
add state = do
    let x:xs = memory state in
        if x == 255
        then return $ State (0:xs)
        else return $ State ((x+1):xs)

--Уменьшает на 1 значение ячейки под кареткой
sub :: State -> IO State
sub state = do
    let x:xs = memory state in
        if x == 0
        then return $ State (255:xs)
        else return $ State ((x-1):xs)

--Сдвигает каретку вправо по ленте
moveRight :: State -> IO State
moveRight state =
    let x:xs = memory state in
        return $ State (xs ++ [x])

--Сдвигает каретку влево по ленте
moveLeft :: State -> IO State
moveLeft state =
    let mem = memory state in
        let (head, last) = splitAt (length mem - 1) mem in
            return $ State (last ++ head)

--Объевление функции, которая применяет к текущему состоянию набор инструкций
process :: State -> Instructions -> IO State


--Повторяет набор инструкций до тех пор, пока значение ячейки под
--кареткой не обнулится
untilZero :: State -> Instructions -> IO State
untilZero state cs
    | first state == 0 = return state
    | otherwise = do
        new_state <- process state cs
        untilZero new_state cs

--Применение пустого набора инструкций ничего не делает
process state [] = return state

--'.' - это инструкция вывода
process state ('.':cs) = do
    new_state <- put state
    process new_state cs

--',' - это инструкция ввода
process state (',':cs) = do
    new_state <- get state
    process new_state cs

--'+' - это инструкция увеличения
process state ('+':cs) = do
    new_state <- add state
    process new_state cs

--'-' - это инструкция уменьшения
process state ('-':cs) = do
    new_state <- sub state
    process new_state cs

--'<' - это инструкция сдвига каретки влево
process state ('<':cs) = do
    new_state <- moveLeft state
    process new_state cs

--'>' - это инструкция сдвига каретки вправо
process state ('>':cs) = do
    new_state <- moveRight state
    process new_state cs

--'[' - это инструкция начала цикла. Производится выделение текущего цикла и его применение
process state ('[':cs) = do
    let (cycle, remain) = extractCycle ('[':cs) in
        do
            new_state <- untilZero state cycle
            process new_state remain

--']' - это инструкция конца цикла. В правильно написанной программе исполнитель не должен
--доходить до этого места, так как все ']', имеющее парное '[' устраняются на стадии выделения
--циклов
process state (']':cs) = do
    putStrLn "Excess ']' encountered"
    process state cs


main :: IO ()
main = do
    --Чтение названия файла из аргументов
    args <- getArgs
    if length args /= 1
    then putStrLn "Should pass one argument: name of the file"
    else do
        --Читаем содержимое файла-программы
        content <- readFile $ head args
        --Отфильтровываем все символы, которые не являются управляющими
        let instructions = (filter (`elem` "+-><,.[]") content) in
            process init_state instructions
        return ()

{-
main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
    then putStrLn "Should pass one argument: name of the file"
    else do
        content <- readFile $ head args
        let instructions = (filter (`elem` "+-><,.[]") content) in
            putStrLn $ instructions ++ "\n" ++ (show (getPairedBracketIdx instructions 0))
-}