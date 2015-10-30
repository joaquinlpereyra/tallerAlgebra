data Polinomio = Mono Float Integer
		| Suma Polinomio Polinomio
		| Producto Polinomio Polinomio
		deriving Show

evaluar :: Polinomio -> Float -> Float
evaluar (Mono a n) x 	 = a*x^n
evaluar (Suma a b) x 	 = evaluar a x + evaluar b x
evaluar (Producto a b) x = evaluar a x * evaluar b x

coeficientes :: Polinomio -> [Float]
coeficientes (Mono 0 _) = []
coeficientes (Mono x y) | y == 0	= [x]
			| otherwise	= [0] ++ coeficientes (Mono x (y-1))
coeficientes (Suma p q) = sumaVector (coeficientes p) (coeficientes q)
coeficientes (Producto p q) = prodListas (coeficientes p) (coeficientes q)

sumaVector :: [Float] -> [Float] -> [Float]
sumaVector [] (x) = x
sumaVector (x) [] = x
sumaVector (x:xs) (y:ys) = [x+y] ++ sumaVector xs ys

prodListas :: [Float] -> [Float] -> [Float]
prodListas [] l = []
prodListas (a:as) (l:ls) = (a*l) : (prodListas (a:as) ls)
prodListas (a:as) l = sumaVector (prodListas [a] l) (0 : (prodListas as l))

