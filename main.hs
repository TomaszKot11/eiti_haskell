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

-- funcka dla zadanych wspolczynnikow generuje permutacje 
-- (funckja generuj_macierze dla zadanej kombinacji pierwszego wektora - wiersza)
-- pierwszego wiersza (wektora). Uruchamia ona równiez "pętle" sprawdzaj_permutacje_pierwszefgo_weektora.
generuj_poczarkowe_permutajce n wspl = do 
                                        sprawdzaj_permutacje_pierwszego_wektora permutacje wspl
                                        where permutacje = permutations [x | x <- [1..n]]
-- Funkcja bedaca "petla" ktora w petli dla kazdego wektora generuje jego macierze, po czym uruchamia
-- funckje sprawdzajaca taka macierz.
sprawdzaj_permutacje_pierwszego_wektora :: [[Integer]] -> [[Integer]] -> IO ()
sprawdzaj_permutacje_pierwszego_wektora [] _ = do 
                                                 print("Koniec")
                                                 return ()                                       
sprawdzaj_permutacje_pierwszego_wektora perm wspl = do 
                                                      let ele = head perm
                                                          elem = delete ele perm
                                                      generuj_macierze_dla_wektora ele wspl
                                                      sprawdzaj_permutacje_pierwszego_wektora elem wspl

-- funckja generujaca macierze dla zadanego pierwszego wiersza. Analogicznie jak generuj_poczatkowe_permutajce
-- uruchamia ona "petle" sprawdzania czy dana macierz zgodna jest z "opisem wspolczynnikow".
generuj_macierze_dla_wektora wektor wspl = do
                                            sprawdzaj_macierze_dla_wektora macierze wspl
                                            where macierze = generuj_macierze wektor

-- "petla" uruchamiajaca właściwy mechanizm sprawdzania - f. piramidy_rek.
-- dla kazdej macierzy oraz ich wspolczynnikow. 
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

-- funckja dokonujaca dla zadanego wektora krotek opisow "wspolczynnikow" oraz macierzy
-- oceny czy spelnia ona  opis wspolczynnikow - jesli nie (ostatni pattern matching _ _ False)
-- zwraca napis "nie spelnia" bedacy jednoczesnie oznaczeniem ze program "zyje" i cos liczy
-- jesli sprawdzono wszystkie wspolczynniki (2 pattern matching) wypisywane jest rozwiazanie
-- i celem zaosczedzenia czasu program konczy dzialanie wyjatkiem exitSuccess
-- Pierwszy pattern matching dotyczy sytuacji gdy sprawdzono wszystkie wspolczynniki a koncowo
-- uzyskano wartosc False
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

-- sprawdzenie czy wektor jest posorotwany rosnaca - te dwie funckjie mogłyby zostać
-- zapisane jako jedna
isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all id . map (\(x,y) -> x < y) . zip xs $ tail xs
-- sprawdzenie czy wektor jest posorotwany malejąco
isSorted' :: (Ord a) => [a] -> Bool
isSorted' xs = all id . map(\(x, y) -> x > y) . zip xs $ tail xs

-- funckja dokonująca wyboru odpowiedniego wektora (wiersza/kolumny w prawidłowej/odwrotnej kolejności)
-- do sprawdzenia na podstawie przekazanej krotki.
wybierz_bazujac_na_warunku :: (Integer, Integer, Integer) -> [[Integer]] -> (Integer, Integer, Integer, [Integer])
wybierz_bazujac_na_warunku (x, y, el) matrix | x == 0 = (el,x , y, (transpose matrix !! fromIntegral(y)))
                                             | x == 1 = (el, x, y, (reverse(matrix !! fromIntegral(y))))
                                             | x == 2 = (el, x, y, (reverse(transpose matrix !! fromIntegral(y))))
                                             | otherwise = (el, x, y, ((matrix !! fromIntegral(y))))

-- funkcja sprawdzajaca czy dany wektor dla zadanej kroki spelnia warunek.
-- Wyznacza nastepujace przypadki:
-- 1. el == 1 tzn wymagana widocznosc jednej wiezy, sprawdzamy czy pierwszy element jest rowny wymiarowi macierzy (n)
-- 2. el == n sprawdzamy czy wektor jest posortowany
-- 3. 0 < el < n uruchamiamy funckje sprawdz_roznice_wysokosci_dwa opisana ponizej 
sprawdz_czy_spelnia_warunek :: (Integer, Integer, Integer, [Integer]) -> Bool
sprawdz_czy_spelnia_warunek (el, x, y, lista) | el == 1 && not(fromIntegral(head(lista)) == n) = False
                                        | el == 1 = True
                                        | fromIntegral(el) == n && (not(isSorted lista) && not(isSorted' lista)) = False
                                        | fromIntegral(el) == n = True
                                        | not(sprawdz_roznice_wysokosci_dwa el 0 glowa lista) = False
                                        | otherwise = True
                           where n = length lista
                                 glowa = head lista

-- funckaj pomocnicza dla sprawdz_roznice_wysokosci_dwa zwracajaca krotke bedaca
-- postaci (nowy_counter_dlugosci_rosnacego_ciagu, nowy_element_maksymalny)
-- w zaleznosci od tego czy biezacy element (x) jest wiekszy od poprzedniego
-- najwiekszego elementu (max)
zwroc_nowy_counter_i_max :: Integer -> Integer -> Integer -> (Integer, Integer)
zwroc_nowy_counter_i_max x counter max | x >= max = (counter + 1, x)
                                       | otherwise = (counter, max)

-- funkcja szuka dlugosci rosnacego ciagu w wektorze zaczynajac od pierwszego elementu
-- w nastepiujacy sposob:
-- na poczatku najwiekszy element to po prostu 1 element z wektora, a dlugosc (counter) to 1
-- nastepnie szukamy wystapienia pierwszego wiekszego elementu (jesli taki jest) i podmieniamy element maksymalny oraz
-- zwiekszamy counter
sprawdz_roznice_wysokosci_dwa :: Integer -> Integer -> Integer -> [Integer] -> Bool 
sprawdz_roznice_wysokosci_dwa ele counter max [] = counter == ele
sprawdz_roznice_wysokosci_dwa ele counter max (x:xs) = do 
                                                    let iks = zwroc_nowy_counter_i_max x counter max
                                                        new_counter = fst iks
                                                        new_max = snd iks
                                                    sprawdz_roznice_wysokosci_dwa ele new_counter new_max xs

