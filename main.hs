import  Data.List
import System.IO
import Data.Traversable
import System.Exit (exitSuccess)

-- Rozwiazanie opiera się na fakcie, ze macierze spelniajace warunki postawionego problemu nie moga
-- posiadac zarowno w danym wieerszu jak i kolumnie dwóch takich samych elementów.
-- Po przeszukaniu sieci Internet autor znalazł, ze sa to tzw. macierze łacińskie, za których
-- generacje w projekcie odpowiedzialna jest funkcja generuj_macierze. Nazwy funckji są po polsku, mimo ze 
-- normlanie autor preferuje nazwy angielskie, z racji na chęć zapewnienia większej przejrzystości rozwiązania.
generuj_macierze :: (Eq a) => [a] -> [[[a]]]
generuj_macierze xs = generuj_macierze' 1 [xs] where
    n = length xs
    generuj_macierze' i rows
        | i == n    = [reverse rows]
        | otherwise = concat [generuj_macierze' (i+1) (row:rows)
                             | row <- znajdz_wiersze (transpose rows) [] xs]
    znajdz_wiersze (col:cols) ls rs = concat [znajdz_wiersze cols (r:ls) (delete r rs)
                                    | r <- rs, r `notElem` col]
    znajdz_wiersze [] ls _ = [reverse ls]

-- Funkcja pomocnicza słuzaca wygenerowaniu odpowiednich krotek (ang. tuple) sluzacych do wyznaczenia
-- krotek postaci (indeks_wiersza, indeks_kolumny, wartosc) w funkcji transformuj_wspolczynniki.
enumerate = zip [0..]

-- Funckja słuzaca zaindeksowaniu macierzy tak by kazdy z jej elementów (będący finalnie krotką) miał postać
-- (indeks_wiersza, indeks_kolumny, wartosc)
-- Przykładowe wejście: [[1, 2, 3, 4]]
-- Przykładowe wyjście: [(0,0,1),(0,1,2),(0,2,3),(0,3,4)]
transformuj_wspolczynniki :: [[Integer]] -> [(Integer, Integer, Integer)]
transformuj_wspolczynniki wspolczynniki = [(y, x, tile) | (y, row) <- enumerate wspolczynniki, (x, tile) <- enumerate row, tile /= 0]

-- funcka dla zadanych wspolczynnikow (bedacych outputem f. transformuj_wspolczynniki)
-- generuje permutacje (funckja generuj_macierze dla zadanej kombinacji pierwszego wektora - wiersza)
-- pierwszego wiersza (wektora). Uruchamia ona równiez "pętle" sprawdzaj_permutacje_pierwszefgo_weektora.
generuj_poczarkowe_permutajce n wspl = do 
                                        sprawdzaj_permutacje_pierwszego_weektora permutacje wspl
                                        where permutacje = permutations [x | x <- [1..n]]
-- Funkcja bedaca "petla" ktora w petli dla kazdego wektora generuje jego macierze, po czym uruchamia
-- funckje sprawdzajaca taka macierz.
sprawdzaj_permutacje_pierwszego_weektora :: [[Integer]] -> [[Integer]] -> IO ()
sprawdzaj_permutacje_pierwszego_weektora [] _ = do 
                                                 print("Koniec")
                                                 return ()                                       
sprawdzaj_permutacje_pierwszego_weektora perm wspl = do 
                                                      let ele = head perm
                                                          elem = delete ele perm
                                                      generuj_macierze_dla_wektora ele wspl
                                                      sprawdzaj_permutacje_pierwszego_weektora elem wspl

-- funckja generujaca macierze dla zadanego pierwszego wiersza. Analogicznie jak generuj_poczatkowe_permutajce
-- uruchamia ona "petle" sprawdzania czy dana macierz zgodna jest z "opisem wspolczynnikow".
generuj_macierze_dla_wektora wektor wspl = do
                                            sprawdzaj_macierze_dla_wektora macierze wspl
                                            where macierze = generuj_macierze wektor

-- "petla" uruchamiajaca właściwy mechanizm sprawdzania - f. piramidy_rek. 
sprawdzaj_macierze_dla_wektora :: [[[Integer]]] -> [[Integer]] -> IO ()
sprawdzaj_macierze_dla_wektora [] _ = do
                                       print("Koniec 2")
                                       return ()
sprawdzaj_macierze_dla_wektora macierze wspl = do 
                                                let ele = head macierze
                                                    elem = delete ele macierze
                                                    trans = transformuj_wspolczynniki wspl
                                                piramidy_rek trans ele True
                                                sprawdzaj_macierze_dla_wektora elem wspl

piramidy_rek :: [(Integer, Integer, Integer)] -> [[Integer]] -> Bool -> IO ()
piramidy_rek [] macierz False = print "nie spelnia"
piramidy_rek [] macierz _ = do 
                             print macierz
                             exitSuccess
piramidy_rek _ _ False = print "nie spelnia"
piramidy_rek wspl macierz poprzednie = do 
                                        let glowa = head wspl
                                            wspl_new = delete glowa wspl
                                            a = wybierz_bazujac_na_warunku glowa macierz
                                            czy_spelnia = sprawdz_czy_spelnia_warunek a
                                        print ""
                                        piramidy_rek wspl_new macierz czy_spelnia

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all id . map (\(x,y) -> x < y) . zip xs $ tail xs

isSorted' :: (Ord a) => [a] -> Bool
isSorted' xs = all id . map(\(x, y) -> x > y) . zip xs $ tail xs

wybierz_bazujac_na_warunku :: (Integer, Integer, Integer) -> [[Integer]] -> (Integer, Integer, Integer, [Integer])
wybierz_bazujac_na_warunku (x, y, el) matrix | x == 0 = (el,x , y, (transpose matrix !! fromIntegral(y)))
                                             | x == 1 = (el, x, y, (reverse(matrix !! fromIntegral(y))))
                                             | x == 2 = (el, x, y, (reverse(transpose matrix !! fromIntegral(y))))
                                             | otherwise = (el, x, y, ((matrix !! fromIntegral(y))))


sprawdz_czy_spelnia_warunek :: (Integer, Integer, Integer, [Integer]) -> Bool
sprawdz_czy_spelnia_warunek (el, x, y, lista) | el == 1 && not(fromIntegral(head(lista)) == n) = False
                                        | el == 1 = True
                                        | fromIntegral(el) == n && (not(isSorted lista) && not(isSorted' lista)) = False
                                        | fromIntegral(el) == n = True
                                        | not(sprawdz_roznice_wysokosci_dwa el 0 glowka lista) = False
                                        | otherwise = True
                           where n = length lista
                                 glowka = head lista

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

