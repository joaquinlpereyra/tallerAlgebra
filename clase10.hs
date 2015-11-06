data Polinomio = Mono Float Integer
        | Suma Polinomio Polinomio
        | Producto Polinomio Polinomio
        deriving Show

evaluar :: Polinomio -> Float -> Float
evaluar (Mono a n) x     = a*x^n
evaluar (Suma a b) x     = evaluar a x + evaluar b x
evaluar (Producto a b) x = evaluar a x * evaluar b x

coeficientes :: Polinomio -> [Float]
coeficientes (Mono 0 _) = []
coeficientes (Mono x y) | y == 0    = [x]
            | otherwise = [0] ++ coeficientes (Mono x (y-1))
coeficientes (Suma p q) = sumaVector (coeficientes p) (coeficientes q)
coeficientes (Producto p q) = prodListas (coeficientes p) (coeficientes q)

sumaVector :: [Float] -> [Float] -> [Float]
sumaVector [] (x) = x
sumaVector (x) [] = x
sumaVector (x:xs) (y:ys) = [x+y] ++ sumaVector xs ys

prodListas :: [Float] -> [Float] -> [Float]
prodListas _ [] = []
prodListas [] _ = []
prodListas [x] ys = escalarPorVector x ys
prodListas (x:xs) ys = sumaVector (escalarPorVector x ys) ([0] ++ prodListas xs ys) 

escalarPorVector :: Float -> [Float] -> [Float]
escalarPorVector x [y] = [x*y]
escalarPorVector x (y:ys) = escalarPorVector x [y] ++ escalarPorVector x ys

derivada :: Polinomio -> Polinomio
derivada (Mono a n) = Mono (a*(fromInteger(n))) (n-1) 
derivada (Suma p q) = Suma (derivada p) (derivada q)
derivada (Producto p q) = Producto (derivada p) (derivada q)
