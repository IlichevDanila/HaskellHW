import Data.Char (ord, chr)
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- Объявление типов на случай, если мне захочется добавить свою реализацию
type Memory = [Int]
type Instructions = String

{-
Вспомогательные функции для выделения циклов

getPairedBracketIdx возвращает сколько элементов из строки инструкций занимает цикл
extractCycle возвращает внутренности ближайшего цикла и остаток инструкций

extractCycle требует, чтоб первая инструкция в списке была '['

Обе функции возвращают Maybe для обработки ошибочной ситуации

Пример:
    extractCycle "[-<+>]<.>." вернёт Just ("-<+>", "<.>.")
-}

getPairedBracketIdx :: Instructions -> Int -> Maybe Int

getPairedBracketIdx [] 0 = Just 0
getPairedBracketIdx [] count = Nothing -- Достигли конца, не встретив ]. Ошибка
getPairedBracketIdx (']':xs) 1 = Just 1

getPairedBracketIdx (']':xs) count =
    case getPairedBracketIdx xs (count - 1) of
        Nothing -> Nothing
        Just idx -> Just $ 1 + idx

getPairedBracketIdx ('[':xs) count = 
    case getPairedBracketIdx xs (count + 1) of
        Nothing -> Nothing
        Just idx -> Just $ 1 + idx
        
getPairedBracketIdx (_:xs) count = 
    case getPairedBracketIdx xs count of
        Nothing -> Nothing
        Just idx -> Just $ 1 + idx

extractCycle :: Instructions -> Maybe (Instructions, Instructions)
extractCycle x = 
    case getPairedBracketIdx x 0 of
        Nothing -> Nothing
        Just idx -> let (cycle, remain) = splitAt idx x in
            Just (tail (take (length cycle - 1) cycle), remain)

{-
Тип для "состояния" исполнителя. В данный момент содержит memory (состояние ленты) и 
status (статус исполнения: "в процессе" или "ошибка").
При ином устройстве вычислений может содержать и текущее положение каретки на ленте
-}
data Status = RUNNING | ERROR {message :: String}
data State = State {memory :: Memory, status :: Status}


-- Возвращает значение под кареткой
first :: State -> Int
first = head . memory

-- Начальное состояние исполнителя (заполненная нулями лента)
init_state :: State
init_state = State (take 30000 $ repeat 0) RUNNING

{-
Тут пришлось добавить препроцессирование. Дело в том, что в оригинале исполнитель
должен считывать и выводить ascii символы, а вычисление производить над их кодами.

Однако отлаживать программы с подобным поведением довольно трудно, так что если
не определен флаг ASCII_IO, то интерпретатор работает не с ASCII символами, а с
целыми числами из отрезка [0; 255]
-}
#ifdef ASCII_IO

-- Читает ascii-символ с stdin на ячейку под кареткой
get :: State -> IO State
get (State (_:xs) stat) = do
    c <- getChar
    return $ State ((ord c):xs) stat

-- Выводит ascii-символ с ячейки под кареткой в stdout
put :: State -> IO State
put state = do
    putChar $ chr $ first state
    return state

#else

-- Читает число с stdin на ячейку под кареткой
-- Вызывает ошибку, если введено не целое число
get :: State -> IO State
get (State (x:xs) stat) = do
    line <- getLine
    return $ case readMaybe line of
        Nothing -> State (x:xs) (ERROR "Not a number")
        Just n ->  State ((n `mod` 256):xs) stat

-- Выводит число с ячейки под кареткой в stdout
put :: State -> IO State
put state = do
    print $ first state
    return state
    
#endif

-- Увеличивает на 1 значение ячейки под кареткой
add :: State -> IO State
add (State (x:xs) stat) =
    if x == 255
    then return $ State (0:xs) stat
    else return $ State ((x+1):xs) stat

-- Уменьшает на 1 значение ячейки под кареткой
sub :: State -> IO State
sub (State (x:xs) stat) = do
    if x == 0
    then return $ State (255:xs) stat
    else return $ State ((x-1):xs) stat

-- Сдвигает каретку вправо по ленте. В данный момент по факту производит циклический
-- сдвиг ленты влево
moveRight :: State -> IO State
moveRight (State (x:xs) stat) = return $ State (xs ++ [x]) stat

-- Сдвигает каретку влево по ленте. В данный момент по факту производит циклический
-- сдвиг ленты вправо
moveLeft :: State -> IO State
moveLeft (State mem stat) =
    let (head, last) = splitAt (length mem - 1) mem in
        return $ State (last ++ head) stat

-- Объевление функции, которая применяет к текущему состоянию набор инструкций
process :: State -> Instructions -> IO State


-- Повторяет набор инструкций до тех пор, пока значение ячейки под
-- кареткой не обнулится (или пока не возникнет ошибка)
untilZero :: State -> Instructions -> IO State

untilZero state@(State _ (ERROR _)) _ = return state

untilZero state cs
    | first state == 0 = return state
    | otherwise = do
        new_state <- process state cs
        untilZero new_state cs

-- Применение пустого набора инструкций ничего не делает
process state [] = return state

-- Применение любого набора инструкций к программе, поймавшей ошибку,
-- ничего не делает (и прекращает исполнение)
process state@(State _ (ERROR _)) cs = return state

-- '.' - это инструкция вывода
process state ('.':cs) = do
    new_state <- put state
    process new_state cs

-- ',' - это инструкция ввода
process state (',':cs) = do
    new_state <- get state
    process new_state cs

-- '+' - это инструкция увеличения
process state ('+':cs) = do
    new_state <- add state
    process new_state cs

-- '-' - это инструкция уменьшения
process state ('-':cs) = do
    new_state <- sub state
    process new_state cs

-- '<' - это инструкция сдвига каретки влево
process state ('<':cs) = do
    new_state <- moveLeft state
    process new_state cs

-- '>' - это инструкция сдвига каретки вправо
process state ('>':cs) = do
    new_state <- moveRight state
    process new_state cs

-- '[' - это инструкция начала цикла. Производится выделение текущего цикла и его применение
process state@(State mem stat) ('[':cs) = do
    case extractCycle ('[':cs) of 
        Nothing -> return $ State mem (ERROR "Not enought ']'")
        Just (cycle, remain) -> do
            new_state <- untilZero state cycle
            process new_state remain

-- ']' - это инструкция конца цикла. В правильно написанной программе исполнитель не должен
-- доходить до этого места, так как все ']', имеющее парное '[' устраняются на стадии выделения
-- циклов
process (State mem stat) (']':cs) =
    return $ State mem (ERROR "Excess ']' encountered")

-- Сюда уже совсем никогда не должна доходить программа. Это правило на случай, если
-- по какой-либо причине не произойдет фильтрация последовательности инструкций
process (State mem stat) (c:cs) = do
    return $ State mem (ERROR ("Unexpected command:" ++ [c]))

printError :: State -> IO ()
printError (State _ (ERROR msg)) = putStrLn msg
printError _ = return ()

main :: IO ()
main = do
    -- Чтение названия файла из аргументов
    args <- getArgs
    if length args /= 1
    then putStrLn "Should pass one argument: name of the file"
    else do
        -- Читаем содержимое файла-программы
        content <- readFile $ head args
        -- Отфильтровываем все символы, которые не являются управляющими
        result <- let instructions = (filter (`elem` "+-><,.[]") content) in
            process init_state instructions
        printError result

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