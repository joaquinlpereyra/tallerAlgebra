potencia :: Float -> Integer -> Float
potencia a b | b == 0    = 1
             | otherwise = a * potencia a (b-1)

division :: Integer -> Integer -> (Integer, Integer)
division a b | a - b < b  = (0,a) 
             | otherwise  = (1+(fst last_div), (snd last_div))
                    where last_div = division (a-b) b


div_parcial :: Integer -> Integer -> [Integer]
div_parcial n m | m == 1       = [1]
                | mod n m /= 0 = div_parcial n (m-1)
                | mod n m == 0 = m : div_parcial n (m-1) 

divs :: Integer -> [Integer]
divs n = div_parcial n n

isItPrime n | length(divs n) == 2 = True
            | otherwise          = False

productoria :: [Integer] -> Integer
productoria lst | length lst == 0   = 1
                | otherwise         = head lst * productoria (tail lst)

reverso :: [a] -> [a]
reverso lst | length lst == 2       = tail lst ++ [head lst]
            | otherwise             = reverso (tail lst) ++ [head lst]

capicua :: [Integer] -> Bool
capicua lst | lst == (reverso lst) = True
            | otherwise            = False
