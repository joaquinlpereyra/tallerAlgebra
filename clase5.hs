pertenece :: Integer -> [Integer] -> Bool
pertenece x xs  | length xs == 0  = False
                | head xs == x    = True
                | otherwise   = pertenece x (tail (xs))

perteneceBis :: Integer -> [Integer] -> Bool
perteneceBis x xs  | length xs == 0 = False
                   | otherwise    = (head xs == x)|| perteneceBis x (tail (xs))

hayRepetidos :: [Integer] -> Bool
hayRepetidos xs | length xs == 1    = False
                | pertenece (head xs) (tail xs) = True
                | otherwise     = hayRepetidos (tail xs) 

menores :: Integer -> [Integer] -> [Integer]
menores x xs  | length xs == 0    = []
              | head xs < x   = head xs : menores x (tail xs)
              | head xs >= x    = menores x (tail xs)

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

--opcionales
--
--
enBase' :: Integer -> Integer -> [Integer]
-- n en base 'base'. rem: remainder. enBase' is backwards, enBase fixes that :)
enBase' n base | (div n base) == 0     = [(rem n base)]
               | otherwise             = ((rem n base) : (enBase' (div n base) (base)))

enBase :: Integer -> Integer -> [Integer]
enBase n base = reverse (enBase' n base) 

deBase :: Integer -> [Integer] -> Integer
-- normaliza un numero: cualquiera sea la base, lo retorna como un int de base 10
deBase base digitos | length digitos == 1   = (head digitos)
                    | otherwise             = ((head digitos)*(potencia)) + (deBase base (tail digitos))
                                                where potencia = base ^ (length digitos - 1)
                                                -- apparently ^ is for integers, ** for floating points. 
                                                -- took me only 30 minutes to find out ;)
capicuaPara :: [Integer] -> [Integer]
capicuaPara digitos | digitos == (reverse digitos)   = digitos
                    | otherwise                      = capicuaPara (enBase new_digitos 10)
                                                        where new_digitos = deBase 10 (sumaInterna digitos (reverse digitos))
                                                        -- this is probably not the nicest but it works :)
                                                        -- btw whats the status for 21119161636? machine doesn't seem to be happy with it :)
                                                        
sumaInterna xa xb | length xa /= length xb  = error "List length not equal"
                  | length xa == 1          = [head xa + head xb]
                  | otherwise               = [head xa + head xb] ++ sumaInterna (tail xa) (tail xb)

changeBase :: Integer -> Integer -> [Integer] -> [Integer]
changeBase base1 base2 digitos = enBase digitos_nuevos base2
                                  where digitos_nuevos = deBase base1 digitos

isItGettingBigger :: [Integer] -> Bool
isItGettingBigger xs | length xs == 1           = True
                     | head xs > head (tail xs) = False
                     | otherwise                = isItGettingBigger (tail xs)

removeNotLast :: [Integer] -> [Integer]
-- remove every repetition except last
removeNotLast xs | length xs == 1                           = xs
                 | pertenece (head xs) (tail xs)            = removeNotLast (tail xs)
                 | otherwise                                = head xs : removeNotLast (tail xs)

removeNotFirst :: [Integer] -> [Integer]
-- remove every repetition except first
removeNotFirst xs = reverse (removeNotLast (reverse xs))

ordenarLista :: [Integer] -> [Integer]
ordenarLista xa    | length (xa) == 0           = []
                   | otherwise                  = ordenarLista(quitar (maximo xa) xa) ++ [maximo xa]

join :: [a] -> [a] -> [a]
join xa xb = xa ++ xb

ordenarDosListas :: [Integer] -> [Integer] -> [Integer]
ordenarDosListas xa xb = ordenarLista (join xa xb)
