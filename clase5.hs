pertenece :: Integer -> [Integer] -> Bool
pertenece x xs  | length xs == 0	= False
		| head xs == x		= True
		| otherwise		= pertenece x (tail (xs))

perteneceBis :: Integer -> [Integer] -> Bool
perteneceBis x xs  | length xs == 0	= False
	           | otherwise 		= (head xs == x)|| perteneceBis x (tail (xs))

hayRepetidos :: [Integer] -> Bool
hayRepetidos xs | length xs == 1	 	= False
		| pertenece (head xs) (tail xs) = True
		| otherwise		 	= hayRepetidos (tail xs) 

menores :: Integer -> [Integer] -> [Integer]
menores x xs	| length xs == 0  	= []
		| head xs < x		= head xs : menores x (tail xs)
		| head xs >= x		= menores x (tail xs)

quitar :: Integer -> [Integer] -> [Integer]
-- quita el primer elemento de una lista
quitar x xs | length xs == 0  		= []
	    | x == (head xs)		= tail xs 
	    | x /= (head xs)		= head xs : (quitar x (tail xs))

maximo :: [Integer] -> Integer
maximo xs | length xs == 1		= head xs
	  | length xs == 0		= error "Lista vacía campeón"
	  | otherwise			= maxi (head xs) (maximo (tail xs))


maxi a b | a >= b = a
	 | b > a  = b

capicuaPara :: [Integer] -> [Integer]
capicuaPara xs | xs == (reverse xs)	= (reverse xs)
	       | otherwise		= capicuaPara (xs (reverse xs)) 

str2lst :: a -> [a]
str2lst str    | length str == 1	= str
	       | otherwise		= head str : str2lst (tail str)





sumaInterna :: [Integer] -> [Integer] -> [Integer]
sumaInterna xa xb | length xa /= length xb = error "length desigual"
	          | length xa == 1         = [(head xa) + (head xb)]
		  | otherwise		   = (head xa) + (head xb) : sumaInterna (tail xa) (tail xb)
