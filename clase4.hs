fib n | n == 1 = 0
      | n == 2 = 1
      | n > 2  = (fib (n - 1)) + (fib (n-2))

par :: Integer -> Bool
par n | n == 1 = False
      | n == 0 = True
      | otherwise = par (n-2)

sumaImpares :: Integer -> Integer
sumaImpares n | n == 1 || n == 0 = n
              | par n == False = n + sumaImpares (n-2)
              | par n == True = n-1 + sumaImpares (n-2)

sumaImpares' :: Integer -> Integer
sumaImpares' n | n == 0 || n == 1 = n
               | otherwise = n*2-1 + sumaImpares'(n - 1)

multiplo3 :: Integer -> Bool
multiplo3 n | n <= 1 && (n /= 0) = False
            | n == 0 = True
            | otherwise = multiplo3 (n-3)

doblefact :: Integer -> Integer
doblefact n | n == 0 || n == 1 = 1
            | otherwise = n * (doblefact (n - 2))

comb :: Integer -> Integer -> Integer
comb n m | n < m            = undefined
         | m == 0 || m == n = 1
         | otherwise        = comb (n-1) m + comb (n-1) (m-1)

sumarara n | par (floor(sqrt n))   = sumaImpares (floor(sqrt(n))-1)
           | otherwise             = sumaImpares (floor(sqrt(n)))

