-- Nome: Alisson Costa Schmidt
--1
ends :: [Int] -> [Int] 
ends [x] = [x]
ends (x:xs) = x : [last xs]

--2
deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = (2 * x) : deduzame (xs)

--3
deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2
  then x : deduzame2 xs
  else deduzame2 xs

geraTabela :: Int -> [(Int,Int)]
geraTabela 1 = [(1, 1)]
geraTabela n = [(n, n^2)] ++ geraTabela (n-1)

--5
contem :: Char -> String -> Bool
contem c "" = False
contem c (x:xs) = c == x || contem c xs


--6
translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate ((x,y):xs) = [(x+2, y)] ++ translate xs

--7
countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs) = if length x > 5 then 1 + countLongs xs else 0 + countLongs xs

onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs) = if length x > 5 then [x] ++ onlyLongs xs else onlyLongs xs