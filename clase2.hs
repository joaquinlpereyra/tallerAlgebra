crearPar :: value -> value' -> (value, value')
crearPar a b = (a,b)

invertir :: (value,value') -> (value',value)
invertir (v1, v2) = (v2, v1)

distancia :: (Float,Float) -> (Float, Float) -> Float
distancia (x1, y1) (x2, y2) = sqrt((x1-x2)**2 + (y1-y2) **2) 

roots :: Float -> Float -> Float -> (Float, Float)
roots a b c = ( (-b+(sqrt(b**2-4*a*c)))/(2*a), (-b-(sqrt(b**2-4*a*c)))/(2*a) 
listaDecreciente = [1, 0 .. -100]

listar :: a -> a -> a -> [a]
listar a1 a2 a3 = [a1, a2, a3]

rangodePaso :: Integer -> Integer -> Integer -> [Integer]
rangodePaso n1 n2 n3 = if n1 <= n2 then [n1, n1+n3 .. n2] else [n1, (n1-n3) .. n2]

slope :: (Float, Float) -> (Float, Float) -> Float
slope (x1, y1) (x2, y2) = (y2-y1)/(x2-x1)

initials :: String -> String -> String
initials nombre apellido = [head nombre] ++ ". " ++ [head apellido] ++ "."


