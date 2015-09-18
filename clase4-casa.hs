suma :: [Integer] -> [Integer] -> [Integer]
suma lst1 lst2 | length lst1 /= length lst2 = error "Length list not equal"
               | length lst1 == 1           = [(head lst1) + (head lst2)]
               | length lst1 /= 1           = [(head lst1) + (head lst2)] ++ suma (tail lst1) (tail lst2)

prodInterno :: [Float] -> [Float] -> Float
prodInterno lst1 lst2 | length lst1 /= length lst2 = error "Length list not equal"
                      | length lst1 == 1           = head lst1 * head lst2
                      | length lst1 /= 1           = (head lst1) * (head lst2) + prodInterno (tail lst1) (tail lst2)

divParcial :: Integer -> Integer -> [Integer]
divParcial n m | m == 1         = [1]
               | mod n m /= 0   = divParcial n (m-1)
               | otherwise      = m : divParcial n (m-1)

divisores :: Integer -> [Integer]
divisores n = divParcial n n

esPrimo :: Integer -> Bool
esPrimo n | length (divisores n) == 2 = True
          | otherwise               = False

division :: Integer -> Integer -> (Integer, Integer)
division a d | a < d      = (0, a)
             | otherwise  = (fst division_ant + 1, snd division_ant)
                            where division_ant = division (a-d) d

division' :: Integer -> Integer -> (Integer, Integer)
division' a d | a > 0 && d > 0 = division a d
              | otherwise      = division (-a) (-d)

noDivisorsTil :: Integer -> Integer -> Bool
noDivisorsTil n m  | length (divParcial n m) == 1  = True
                   | otherwise                    = False

esPrimo' :: Integer -> Bool
esPrimo' n | noDivisorsTil (n) (n-1)    = True
           | otherwise                  = False

