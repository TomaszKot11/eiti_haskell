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


main = do
        print el
        where el = sprawdz_roznice_wysokosci_dwa 2 0 2 [2, 3, 1, 5, 4]


























        -- not to use Data.List.permutations which would 
-- be more efficient
-- cust_permutations :: Eq a => [a] -> [[a]]
-- cust_permutations [] = [[]]
-- cust_permutations as = do a <- as
--                           let l = delete a as
--                           ls <- cust_permutations l
--                           return $ a : ls