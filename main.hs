-- import System.IO
-- import System.Environment


-- Smietnik
-- main = getArgs >>= parse

-- parse :: [[Int]] -> IO()
-- parse lists = print lists

-- import Data.List


-- permutations - generates a permutation
-- of a given vector

-- main = parse [[1, 2, 3], [2, 3, 4]]
-- print permutations [1, 2, 3]
-- print permutations [1, 2, 3, 5, 6]

-- prpermutations [1, 2, 3]
-- print cust_permutations [1, 2 ,3]
-- Smietnik

-- not to use Data.List.permutations which would 
-- be more efficient
-- cust_permutations :: Eq a => [a] -> [[a]]
-- cust_permutations [] = [[]]
-- cust_permutations as = do a <- as
--                           let l = delete a as
--                           ls <- cust_permutations l
--                           return $ a : ls

-- main = print cust_permutations 5
-- n == 5
-- cust_permutations [1, 2, 3, 4, 5]
import  Data.List
import System.IO
-- import System.IO


-- -- TODO: zmienic ta implementacje
findLatinSqs :: (Eq a) => [a] -> [[[a]]]
findLatinSqs xs = findLatinSqs' 1 [xs] where
    n = length xs
    findLatinSqs' i rows
        | i == n    = [reverse rows]
        | otherwise = concat [findLatinSqs' (i+1) (row:rows)
                             | row <- findRows (transpose rows) [] xs]
    findRows (col:cols) ls rs = concat [findRows cols (r:ls) (delete r rs)
                                    | r <- rs, r `notElem` col]
    findRows [] ls _ = [reverse ls]

-- filterMatrixes xs do 

-- main = do 
--     let result = findLatinSqs [1, 2, 3]
--     nextFunction result
--     print result



-- listSub :: [Int] -> [Int]

-- listSub [] = []
-- listSub (x:xs) = do
--                 x
--                 listSub xs

-- -- nestedSub :: [[Int]] -> [[Int]]
-- nestedSub [] = []
-- nestedSub (y:ys) = do 
--                   listSub y 
--                   nestedSub ys

tower_heights = [[3, 0, 1, 0], [0, 3, 0, 0], [0, 0, 0, 0], [0, 0, 4, 0]]

witam = [[2, 1, 4, 3], [3, 4, 2, 1], [1, 2, 3, 4], [4, 3, 1, 2]]
enumerate = zip [0..]
elo = [(y, x, tile) | (y, row) <- enumerate witam, (x, tile) <- enumerate row]
-- wezmy te ktore trzeba sprawdzic
papa = [(y, x, tile) | (y, row) <- enumerate tower_heights, (x, tile) <- enumerate row, tile /= 0]

-- loop :: [(Integer, Integer, Integer)] -> [[Integer]] -> () 

-- loop [] _ = []
-- loop (x:xs) _ = loop x
-- loop :: [(Integer, Integer, Integer)] -> ((Integer, Integer, Integer) -> Bool) -> [Integer]
-- loop [] _ = []
-- loop (x:xs) func = 
--                  func x
--                  loop xs func
--


-- sprawdza czy zadany wektor jest uporzadkowany rosnąco
isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all id . map (\(x,y) -> x < y) . zip xs $ tail xs

isSorted' :: (Ord a) => [a] -> Bool
isSorted' xs = all id . map(\(x, y) -> x > y) . zip xs $ tail xs

-- przyjmuje wektor zindeksowanych elementow macierzy, wektor do sprawdzenia
-- funckja_spr :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [Int]
-- funkcja_spr el_m
-- funckja wybiera odpowiedni wektor z macierzy do sprawdzenia z warunkiem "z brzegu" łamigłówki
wybierz_bazujac_na_warunku :: (Integer, Integer, Integer) -> [[Integer]] -> (Integer, [Integer])
wybierz_bazujac_na_warunku (x, y, el) matrix | x == 0 = (el, (transpose matrix !! fromIntegral(y)))
                                             | x == 1 = (el, (reverse(matrix !! fromIntegral(y))))
                                             | x == 2 = (el, (reverse(transpose matrix !! fromIntegral(y))))
                                             | otherwise = (el, (reverse(matrix !! fromIntegral(y))))


-- sprawdzenie w przypadku gdy el == 1 oraz el == n jest trywialne - pierwszy element rowny n, natomiast w 2 czy elementy
-- sa posortowane w porzadku rosnacym 
-- w przypadku gdy el E (0, n) wyznaczamy odpowiednie roznice pomiedzy parami  
-- elementow tzn o indekach {-1, 0}, {1, 0} autor pod indeksem -1 rozumie ideks "sztucznego" elementu o wartosci 0 wystepujacego
-- przed elementem pierwszym - wysokosc pierwszej piramidy, nastepnie  nalezy znalezc ilosc dodatnich elementow po czym porownac 
-- czy jest ona rowna el - jesli jest tzn. wektor jest prawidlowy
sprawdz_czy_spelnia_warunek :: (Integer, [Integer]) -> ()
sprawdz_czy_spelnia_warunek (el, lista) | el == 1 && not(fromIntegral(head(lista)) == n) = error "To nie rozwiazanie 1"
                                        | el == 1 = ()
                                        | fromIntegral(el) == n && (not(isSorted lista) && not(isSorted' lista)) = error "To nie jest rozwiaznie 2"
                                        | fromIntegral(el) == n = ()
                                        | not(sprawdz_roznice_wysokosci el lista)= error "To nie jest rozwiazanie 3"
                                        | otherwise =  ()
                           where n = length lista

-- funkcja realizuja sprawdzenie roznic wysokosci odpowiednich piramid
sprawdz_roznice_wysokosci :: Integer -> [Integer] -> Bool
sprawdz_roznice_wysokosci el lista = length(filter(\n -> n > 0) final_arr) == fromIntegral(el)
       where n = length lista
             initial_arr = [lista !! 0]
             generated = [(lista !! (helper + 1)) - (lista !! (helper)) | helper <- [0..(n - 2)]]
             final_arr = initial_arr ++ generated

-- TODO: dodaac sprawdzanei dla 0 < el < n

-- wybierz_bazujac_na_warunku sprawdz_czy_spelnia_warunek
-- filterek :: (Integer, Integer, Integer) -> Bool
-- filterek (x, y, el) list | el == 3 = True
--                          | otherwise = error "Falsz"


-- main = do 
--       nestedSub [[1, 2, 3], [7, 7, 7]]


