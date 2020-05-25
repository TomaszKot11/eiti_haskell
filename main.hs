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
import Data.Traversable
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

-- set - 2 rozwiazanie
tower_heights_2 = [[2, 1, 2, 2, 3], [3, 3, 0, 0, 0], [0, 0, 0, 0, 0], [2, 3, 3, 1, 4]]
solution_2 =  [[4, 5, 1, 3, 2], [3, 4, 5, 2, 1], [1, 3, 2, 5, 4], [5, 2, 4, 1, 3], [2, 1, 3, 4, 5]]

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

-- main = do
--         forM [0..(length papa - 1)] $ \number -> do
--              let a = wybierz_bazujac_na_warunku (papa !! number) witam
--              sprawdz_czy_spelnia_warunek a


enumerate = zip [0..]
-- dokonuje mapowania wspolczynnikow (licz oznaczajacych liczbe widocznych wiez) na tuple (indeks_x, indeks_y, ilosc_wiez)
-- pomijajac tuple ktorych ilosc_wiez == 0 
transformuj_wspolczynniki :: [[Integer]] -> [(Integer, Integer, Integer)]
transformuj_wspolczynniki wspolczynniki = [(y, x, tile) | (y, row) <- enumerate wspolczynniki, (x, tile) <- enumerate row, tile /= 0]

-- generuj_poczarkowe_permutajce :: Integer -> [Integer]
generuj_poczarkowe_permutajce n = do 
                                   sprawdzaj_permutacje_pierwszego_weektora permutacje
                                   where permutacje = permutations [x | x <- [1..n]]

sprawdzaj_permutacje_pierwszego_weektora :: [[Integer]] -> IO ()

sprawdzaj_permutacje_pierwszego_weektora [] = do 
                                                 print("Koniec")
                                                 return ()
sprawdzaj_permutacje_pierwszego_weektora perm = do 
                                                 let ele = head perm
                                                     elem = delete ele perm
                                                 generuj_macierze_dla_wektora ele
                                                 sprawdzaj_permutacje_pierwszego_weektora elem


generuj_macierze_dla_wektora wektor = do
                                       sprawdzaj_macierze_dla_wektora macierze
                                       where macierze = findLatinSqs wektor


sprawdzaj_macierze_dla_wektora :: [[[Integer]]] -> IO ()
sprawdzaj_macierze_dla_wektora [] = do
                                     print("Koniec 2")
                                     return ()

sprawdzaj_macierze_dla_wektora macierze = do 
                                          let ele = head macierze
                                              elem = delete ele macierze
                                              trans = transformuj_wspolczynniki tower_heights
                                          piramidy_rek trans ele True
                                          sprawdzaj_macierze_dla_wektora elem

                                            

-- piramida_rozwiazania_dla_n :: Integer -> [[[Integer]]]
-- piramida_rozwiazania_dla_n n = do 
--                                 let permutation_vect = 
--                                 let permutacje = findLatinSqs [x | x <-[1..n]]
                                

-- piramidy :: [[Integer]] -> [[Integer]] -> IO [()]
-- piramidy wspl macierz = do 
--                          forM [0..(length wsp_height - 1)] $ \number -> do
--                               let wsp_height = transformuj_wspolczynniki wspl
--                                   a = wybierz_bazujac_na_warunku (wsp_height !! number) macierz
--                                   witam_dwa = sprawdz_czy_spelnia_warunek a 
--                               print "elo"
--                               where wsp_height = transformuj_wspolczynniki wspl


piramidy_rek :: [(Integer, Integer, Integer)] -> [[Integer]] -> Bool -> IO ()
piramidy_rek [] macierz _ = print macierz
piramidy_rek _ _ False = print "nie spelnia"
piramidy_rek wspl macierz poprzednie = do 
                                        let glowa = head wspl
                                            wspl_new = delete glowa wspl
                                            a = wybierz_bazujac_na_warunku glowa macierz
                                            czy_spelnia = sprawdz_czy_spelnia_warunek a
                                        piramidy_rek wspl_new macierz czy_spelnia


wypisz_jesli_spelnia :: Bool -> [[Integer]] -> IO()
wypisz_jesli_spelnia warunek macierz | warunek = print macierz
                                     | otherwise = print ""

-- sprawdza czy zadany wektor jest uporzadkowany rosnąco
isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all id . map (\(x,y) -> x < y) . zip xs $ tail xs

isSorted' :: (Ord a) => [a] -> Bool
isSorted' xs = all id . map(\(x, y) -> x > y) . zip xs $ tail xs

-- przyjmuje wektor zindeksowanych elementow macierzy, wektor do sprawdzenia
-- funckja_spr :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [Int]
-- funkcja_spr el_m
-- funckja wybiera odpowiedni wektor z macierzy do sprawdzenia z warunkiem "z brzegu" łamigłówki
wybierz_bazujac_na_warunku :: (Integer, Integer, Integer) -> [[Integer]] -> (Integer, Integer, Integer, [Integer])
wybierz_bazujac_na_warunku (x, y, el) matrix | x == 0 = (el,x , y, (transpose matrix !! fromIntegral(y)))
                                             | x == 1 = (el, x, y, (reverse(matrix !! fromIntegral(y))))
                                             | x == 2 = (el, x, y, (reverse(transpose matrix !! fromIntegral(y))))
                                             | otherwise = (el, x, y, ((matrix !! fromIntegral(y))))


-- sprawdzenie w przypadku gdy el == 1 oraz el == n jest trywialne - pierwszy element rowny n, natomiast w 2 czy elementy
-- sa posortowane w porzadku rosnacym 
-- w przypadku gdy el E (0, n) wyznaczamy odpowiednie roznice pomiedzy parami  
-- elementow tzn o indekach {-1, 0}, {1, 0} autor pod indeksem -1 rozumie ideks "sztucznego" elementu o wartosci 0 wystepujacego
-- przed elementem pierwszym - wysokosc pierwszej piramidy, nastepnie  nalezy znalezc ilosc dodatnich elementow po czym porownac 
-- czy jest ona rowna el - jesli jest tzn. wektor jest prawidlowy
sprawdz_czy_spelnia_warunek :: (Integer, Integer, Integer, [Integer]) -> Bool
sprawdz_czy_spelnia_warunek (el, x, y, lista) | el == 1 && not(fromIntegral(head(lista)) == n) = False
                                        | el == 1 = True
                                        | fromIntegral(el) == n && (not(isSorted lista) && not(isSorted' lista)) = False
                                        | fromIntegral(el) == n = True
                                        | not(sprawdz_roznice_wysokosci_dwa el 0 glowka lista) = False
                                        | otherwise = True
                           where n = length lista
                                 glowka = head lista

-- funkcja realizuja sprawdzenie roznic wysokosci odpowiednich piramid
-- sprawdz_roznice_wysokosci :: Integer -> [Integer] -> Bool
-- sprawdz_roznice_wysokosci el lista = length(filter(\n -> n > 0) final_arr) == fromIntegral(el)
--        where n = length lista
--              initial_arr = [lista !! 0]
--              generated = [(lista !! (helper + 1)) - (lista !! (helper)) | helper <- [0..(n - 2)]]
--              final_arr = initial_arr ++ generated


sprawdz_roznice_wysokosci_dwa :: Integer -> Integer -> Integer -> [Integer] -> Bool 
zwroc_nowy_counter_i_maxi :: Integer -> Integer -> Integer -> (Integer, Integer)
zwroc_nowy_counter_i_maxi x counter maxi | x >= maxi = (counter + 1, x)
                                         | otherwise = (counter, maxi)

sprawdz_roznice_wysokosci_dwa ele counter maxi [] = counter == ele
sprawdz_roznice_wysokosci_dwa ele counter maxi (x:xs) = do 
                                                    let iks = zwroc_nowy_counter_i_maxi x counter maxi
                                                        new_counter = fst iks
                                                        new_maxi = snd iks
                                                    sprawdz_roznice_wysokosci_dwa ele new_counter new_maxi xs




-- TODO: dodaac sprawdzanei dla 0 < el < n

-- wybierz_bazujac_na_warunku sprawdz_czy_spelnia_warunek
-- filterek :: (Integer, Integer, Integer) -> Bool
-- filterek (x, y, el) list | el == 3 = True
--                          | otherwise = error "Falsz"


-- main = do 
--       nestedSub [[1, 2, 3], [7, 7, 7]]


