
data Figura = Rectangulo Float Float Float Float
        | Circulo    Float Float Float deriving Show

c1 :: Figura
c1 = Circulo 0 0 pi 

r1 :: Float -> Figura
r1 x = Rectangulo 0 0 (sin(0.25*pi)*x) (cos(0.25*pi)*x)

area :: Figura -> Float
area (Rectangulo x1 x2 y1 y2) = abs(x1-y1) * abs(x2-y2)
area (Circulo c1 c2 r)   = pi*r**2

data Punto = Point Float Float
data Figura2 = Rectangulo2 Punto Punto 
            | Circulo2 Punto Float

area2 :: Figura2 -> Float
area2 (Rectangulo2 (Point x1 y1) (Point x2 y2)) = abs((x1-x2) * (y1-y2))  
area2 (Circulo2 _ r) = pi*r**2

data ProgAritmetica = Vacio
            | CongruentesA Integer Integer 
instance Show ProgAritmetica where
    show Vacio = "{}"
    show (CongruentesA a b) = "{a en Z / a = " ++ show a ++ " (mod " ++ show b ++ ")} \n{k en Z / a = " ++ show a ++"+"++ show b ++ "k }"
instance Eq ProgAritmetica where
    (CongruentesA a b) == (CongruentesA c d) = (iguales (CongruentesA a b) (CongruentesA c d))
    Vacio == Vacio  = True

esMultiplo :: Integer -> Integer -> Bool
esMultiplo x 0 = False
esMultiplo x y | mod x y == 0   = True
           | otherwise  = False

pertenece :: Integer -> ProgAritmetica -> Bool
pertenece _ Vacio = False
pertenece n (CongruentesA x y) | esMultiplo n (x+y) = True
                   | otherwise    = False 

incluido :: ProgAritmetica -> ProgAritmetica -> Bool
incluido _ Vacio = False
incluido Vacio _ = True 
incluido (CongruentesA a b) (CongruentesA c d) 
                           | (a == 0 || c == 0) && mod d b == 0 = True
                           | (a == 0 || c == 0) && mod d b /= 0 = False
                           | mod b d == 0 && (mod a d == mod c d) = True
                           | otherwise              = False 
                           
suma :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica
suma (CongruentesA a b) (CongruentesA c d) = (CongruentesA (a+c) (mcd b d))

mcd :: Integer -> Integer -> Integer 
mcd a b | (min a b) == 0    = max a b
        | otherwise         = mcd (mod (max a b) (min a b)) (min a b)

interseccion :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica
-- busca la interseccion entre dos progresiones aritmeticas
-- depende FUERTEMENTE del teorema chino del resto y de (casi) todas las funciones de abajo
-- creo que no resolví todavía qué hacer en caso de que no exista inverso multiplicativo, salvo en los casos más fáciles
interseccion _ Vacio = Vacio
interseccion Vacio _ = Vacio
interseccion (CongruentesA a b) (CongruentesA c d) | (b == d) && (mod a b /= mod c d) = Vacio -- checkea el absurdo más obvio
                                                   | iguales (CongruentesA a b) (CongruentesA c d) = (CongruentesA a b) -- checkea que sean iguales
                                                   | mcd b d == 1 = teoremaChino (CongruentesA a b) (CongruentesA c d) -- si mcd b d == 1, puedo aplicar directamente el TCR
                                                   | (a == c) && esMultiplo b d = (CongruentesA a b) -- los cuatro siguientes son las implicaciones más claras
                                                   | (a == c) && esMultiplo d b = (CongruentesA c d)
                                                   | esMultiplo a d && esMultiplo c a   = (CongruentesA a b)
                                                   | esMultiplo c b && esMultiplo a c   = (CongruentesA c d)
                                                   | otherwise    = teoremaChinoNoCoprimo (coprimizarModulos (CongruentesA a b) (CongruentesA c d)) -- tengo que coprimizar todos los módulos y aplicar el TCR

teoremaChino :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica
-- Teorema Chino del Resto, versión clásica
-- Depende de "inverso"
teoremaChino (CongruentesA a b) (CongruentesA c d) = (CongruentesA (d*(inverso d b)*a + b*(inverso b d)*c) (b*d))

teoremaChinoNoCoprimo :: [ProgAritmetica] -> ProgAritmetica
-- TCM, versión modificada recursivamente para que pueda usarse con las más de dos progresiones que resultan de coprimizar los módulos
-- depende de teoremaChino, estaEstaProg
teoremaChinoNoCoprimo progs | length progs == 1                         = head progs
                            | estaEstaProg (head progs) (tail progs)    = teoremaChinoNoCoprimo (tail progs)
                            | length progs == 2                         = teoremaChino (head progs) (head (tail progs))
                            | otherwise                                 = teoremaChinoNoCoprimo (([teoremaChino (head progs) (head (tail progs))]) ++ (tail (tail progs)))

inverso :: Integer -> Integer -> Integer
-- busca el inverso de a módulo b
inverso a b | mcd a b /= 1  = error "¡No es inversible!"
            | otherwise     = head (tail (euclidesExt a b))


euclidesExt :: Integer -> Integer -> [Integer]
-- Algoritmo extendido de euclides
-- lo cierto es que no sé bien cómo funciona.
euclidesExt r0 r1 = euclidesExt' r0 r1 1 0 0 1

euclidesExt' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
euclidesExt' r0 r1 u0 v0 u1 v1 | r1 == 0     = [r0, u0, v0]
                               | otherwise   = euclidesExt' r1 (mod r0 r1) u1 v1 (u0-(div r0 r1)*u1) (v0-(div r0 r1)*v1)

coprimizarModulos :: ProgAritmetica -> ProgAritmetica -> [ProgAritmetica]
-- 'coprimiza' los módulos: recibe dos progresiones aritméticas y
-- manda cada módulo a ser factorizado tal como le sirve al TCR
-- depende de coprimizarProg, factoresUtiles
coprimizarModulos (CongruentesA a b) (CongruentesA c d) 
                  | mcd b d == 1  = error "No deberías estar en esta función"
                  | otherwise     = (coprimizarProg (CongruentesA a b) (factoresUtiles b)) ++ (coprimizarProg (CongruentesA c d) (factoresUtiles d))

coprimizarProg :: ProgAritmetica -> [Integer] -> [ProgAritmetica]
coprimizarProg (CongruentesA a b) factores 
                | length factores == 0       = []
                | otherwise                  = (CongruentesA a (head factores)) : coprimizarProg (CongruentesA a b) (tail factores)
                

factoresUtiles :: Integer -> [Integer]
-- encuentra los factores 'utiles' para el TCR. ie: multiplica los primos iguales
-- ejemplo: factoresUtiles 12 = [4, 3]
-- depende de primalizaLista
factoresUtiles x = multiplicarIguales (primalizaLista [x])

primalizaLista :: [Integer] -> [Integer]
-- una función que toma una lista y parte en primos cada número
-- depende de primaliza, esPrimo
primalizaLista xs | xs == primalizaLista' xs = xs
                  | otherwise               = primalizaLista (primalizaLista' xs)

primalizaLista' :: [Integer] -> [Integer]
primalizaLista' []   = []
primalizaLista' (x:xs) | esPrimo x           = x : primalizaLista' xs
                      | otherwise           = primaliza x ++ primalizaLista' xs

primaliza :: Integer -> [Integer]
-- da los factores (no necesariamente primos) de un n
-- depende de divisores, divisoresProp, esPrimo
primaliza n | esPrimo n = tail (divisores n)
            | length (divisoresProp n) == 1 = primaliza' n (head (divisoresProp n))
            | otherwise = primaliza' n (head (tail (divisoresProp n)))

primaliza' :: Integer -> Integer -> [Integer]
primaliza' n i | i == 1          = []
               | mod n i == 0    = i : (primaliza' (div n i) (div n i))
               | otherwise       = primaliza' n (i-1)

divisores :: Integer -> [Integer]
divisores 2 = [1, 2]
divisores 3 = [1, 3]
divisores a = divisores' a 1

divisores' :: Integer -> Integer -> [Integer]
divisores' a b  | a == 1 || b > div a 2                                       = [a]
                | mod a b   == 0                                              = b : divisores' a (b+1)
                | otherwise                                                   = divisores' a (b+1)

divisoresProp :: Integer -> [Integer]
-- depende de divisores
divisoresProp x = tail (init ( divisores x))

multiplicarIguales :: [Integer] -> [Integer]
-- multiplica elementos iguales en una lista
-- multiplicarIguales' espera una lista ordenada :)
-- depende de ordenarLista, howManyTimes
multiplicarIguales xs = multiplicarIguales' (ordenarLista xs)

multiplicarIguales' :: [Integer] -> [Integer]
multiplicarIguales' [] = []
multiplicarIguales' (x:xs)  | esta x xs    = x^(howManyTimes x (x:xs)) : multiplicarIguales' ( remueveCabezaIgual x (x:xs))
                           | otherwise    = x : multiplicarIguales' xs

howManyTimes :: Integer -> [Integer] -> Integer
-- cuantas veces aparece x en ys
howManyTimes x ys | length ys == 0        = 0
                  | x == (head ys)        = 1 + howManyTimes x (tail ys)
                  | otherwise             = howManyTimes x (tail ys) 

remueveCabezaIgual :: Integer -> [Integer] -> [Integer]
-- remueve x de la cabeza de ys cuantas veces seguidas aparezca
-- remueveCabezaIgual 3 [3,3,3,2,1,3] = [2,1,3]
remueveCabezaIgual x ys | length ys == 0    = ys
                        | head ys /= x      = ys
                        | otherwise         = remueveCabezaIgual x (tail ys)

ordenarLista :: [Integer] -> [Integer]
-- ordena de mayor a menos
-- depende de maximo
ordenarLista xa    | length (xa) == 0           = []
                   | otherwise                  = ordenarLista(quitar (maximo xa) xa) ++ [maximo xa]
quitar :: Integer -> [Integer] -> [Integer]
-- quita el primer elemento x de una lista con x repetidos
quitar x xs | length xs == 0      = []
            | x == (head xs)    = tail xs 
            | x /= (head xs)    = head xs : (quitar x (tail xs))

maximo :: [Integer] -> Integer
maximo xs | length xs == 1    = head xs
          | length xs == 0    = error "Lista vacía campeón"
          | otherwise         = maxi (head xs) (maximo (tail xs))

maxi a b | a >= b = a
         | b > a  = b


esta :: Integer -> [Integer] -> Bool
esta n [] = False
esta n xs | n == head xs    = True
          | otherwise       = esta n (tail xs)

estaEstaProg :: ProgAritmetica -> [ProgAritmetica] -> Bool
estaEstaProg prog [] = False
estaEstaProg prog progs | prog == head progs    = True
                        | otherwise             = estaEstaProg prog (tail progs)

esPrimo :: Integer -> Bool
esPrimo x | length (divisores x) == 2  = True
          | otherwise           = False

iguales :: ProgAritmetica -> ProgAritmetica -> Bool
iguales prog1 prog2 | incluido prog1 prog2 && incluido prog2 prog1 = True
                    | otherwise                                    = False
