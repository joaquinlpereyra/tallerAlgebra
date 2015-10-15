mcd :: Integer -> Integer -> Integer 
mcd a b | (min a b) == 0    = max a b
        | otherwise         = mcd (mod (max a b) (min a b)) (min a b)

--patern matching now
iguales :: (Int, Int) -> Bool
iguales (x,y) = x == y

take' :: Int -> [Int] -> [Int]
take' 0 _        = []
take' _ []       = []
take' n (x:xs)   = x : take (n-1) xs

first :: (a, b, c) -> a
first (x, y, z) = x

longitud :: [a] -> Integer
longitud [] = 0
longitud (x:[]) = 1
longitud (x:y:[]) = 2
longitud (x:y:z:[]) = 3
longitud (_:_:_:xs) = 3 + toInteger (length xs)

iniciales :: [Char] -> [Char] -> [Char]
iniciales nombre apellido = [n] ++ ". " ++ [a] ++ "."
                            where (n:_) = nombre
                                  (a:_) = apellido

type Racional = (Integer, Integer)
suma :: Racional -> Racional -> Racional
suma (a, b) (c, d) = (a*d + b*c, b*d)

-- type Punto = (Integer, Integer)
--dist :: Punto -> Punto -> Float
--dist (a, b) (c, d) = (x**2 + y**2)*(0.5)
--                        where x = toInteger (a-c)
--                             y = toInteger (b-c)

producto :: Racional -> Racional -> Racional
producto (a, b) (c, d) = ((a*c), (b*d))

igual :: Racional -> Racional -> Bool
igual (a, b) (c, d) | b == d    = a == c
                    | otherwise = igual (a*d, b*d) (c*b, d*b)

mayor :: Racional -> Racional -> Bool
mayor (a,b) (c,d ) | b == d      = a > c
                   | otherwise  = mayor (a*d, b*d) (c*b, d*b)


data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

esFinde :: Dia -> Bool
esFinde Sabado = True
esFinde Domingo = True
esFinde dia    = False

diaHabil :: Dia -> Bool
diaHabil dia = not (esFinde dia)

-- preguntar soloAlgebra
--
-- ejercicios para casa
tuplas :: [a] -> [b] -> [(a,b)]
tuplas (x:xs) (y:ys) | length ys == 1                   = [(x,y)]
                     | otherwise                        = [(x,y)] ++ tuplas xs ys

potencia :: Racional -> Integer -> Racional
potencia x a | a == 1       = x
             | otherwise    = producto x (potencia x (a-1))

type Conjunto = [Integer]

union :: Conjunto -> Conjunto -> Conjunto
union [] bes = bes
union (a:as) bes | esta a bes          = union as bes
                 | otherwise            = [a] ++ union as bes

esta :: Integer -> Conjunto -> Bool
esta a []      = False
esta a (b:bes) | a == b              = True
               | length (b:bes) == 1 = False
               | otherwise           = esta a bes

interseccion :: Conjunto -> Conjunto -> Conjunto
interseccion [] bes = []
interseccion (a:as) bes | not (esta a bes)  = interseccion as bes
                        | otherwise         = [a] ++ interseccion as bes

incluido :: Conjunto -> Conjunto -> Bool
incluido [] bes = True
incluido (a:as) bes | esta a bes    = incluido as bes
                    | otherwise     = False

igualConjuntos :: Conjunto -> Conjunto -> Bool
igualConjuntos as bes | incluido as bes && incluido bes as = True
                      | otherwise                          = False

separar1 :: Integer -> Conjunto -> Conjunto
separar1 x [] = []
separar1 x (a:as) | x >= a       = a : separar1 x as
                  | otherwise    = separar1 x as

separar2 :: Integer -> Conjunto -> Conjunto
separar2 x [] = []
separar2 x (a:as) | x < a       = a : separar2 x as
                  | otherwise   = separar2 x as

separar :: Integer -> Conjunto -> (Conjunto, Conjunto)
separar x as = ((separar1 x as), (separar2 x as)) 
