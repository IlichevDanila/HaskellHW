## Обоснование выбора языка
Для этой задачи был выбран функционаольный стиль, так как у логической парадигмы скорее
всего возникли бы проблемы с моделированием ленточной памяти. Haskell же был выбран потому
как необходимость хранить "состояние" исполнителя удобне при помощи специального типа данных.
К тому же Haskell попросту удобнее и выразительнее других функциональных языков (на мой вкус).

## Описание сборки
Сборка программы осуществляется при помощи утилиты Make.

Если нужно собрать версию, выполняющую вычисления над ASCII-символами, то
необходимо выполнить
```make ascii```

Если же нужна версия, выполняющая вычисления над целыми числами по модулю
256, то необходимо выполнить просто
```make```

Для сборки требуются: ghc (компилятор) и cpp (стандартный препроцессор gcc)

## Описание тестов

Тесты лучше производить интерпретатором, производящим вычисления над числами (а не символами).
Результаты ASCII интерпретатор довольно трудно понимать при проверке арифметики
(Исключение: HelloWorld.bf - это тест как раз на ASCII интерпретатор)

Все программы-тесты лежат в директории tests:
1. io.bf - проверяет ввод/вывод. Требует ввода числа/символа и выводит на экран то
           же самое число/символ

2. add_sub.bf - проверяет операции сложения и вычитания. Требует ввода числа/символа n
                и выводит на экран значения (n+1), (n+2) и (n-3)

3. moves.bf - проверяет операции смещения коретки. Требует ввода двух чисел/символов и
              выводит их в обратном порядке

4. simple_loops.bf - проверяет одноуровневые циклы. Требует ввода двух чисел/символов и
                     выводит их сумму

5. nested_loops.bf - проверяет вложенные циклы. Требует одно число n (с символами проверять
                     этот тест чрезвычайно неудобно - лучше не использовать ASCII версию иполнителя)
                     и выводит на экран сумму 1 + 2 + ... + n (Лучше не выбирать n > 20)

6. HelloWorld.bf - при использовании ASCII версии интерпретатора выводит на экран строку "Hello World!"

7. invalid_input1.bf и invalid_input2.bf - проверяют исколючительные ситуации при интерпретации.
                                           Требуют ввода числа/символа и выводят строки
                                           "Not enought ']'" и "Excess ']' encountered" соответственно
