data Arbol = Hoja Integer | Ramificacion Arbol Integer Arbol 
	deriving (Eq, Show)

esHoja :: Arbol -> Bool
esHoja (Hoja _) = True
esHoja _ = False

sumaNodos :: Arbol -> Integer
sumaNodos (Hoja x) = x
sumaNodos (Ramificacion arbol1 int arbol2) = int + sumaNodos arbol1 + sumaNodos arbol2

altura :: Arbol -> Integer
altura (Hoja _) = 1
altura (Ramificacion arbol1 _ (Hoja _)) = 1 + altura arbol1
altura (Ramificacion (Hoja _) _ arbol2) = 1 + altura arbol2
altura (Ramificacion arbol1 _ arbol2) = 1 + altura arbol1 + altura arbol2

altura' :: Arbol -> Integer
altura' (Hoja _) = 1
altura' (Ramificacion arbol1 _ arbol2) = 1 + max (altura arbol1) (altura arbol2)

pertenece :: Integer -> Arbol -> Bool
pertenece n (Hoja x) | n == x = True
		     | otherwise = False

pertenece n (Ramificacion arbol1 int arbol2) | n == int	= True
					     | (pertenece n arbol1 || pertenece n arbol2) = True
					     | otherwise = False 

data Dir = Der | Izq
busqueda :: [Dir] -> Arbol -> Integer
busqueda [] (Ramificacion (Hoja _) x (Hoja _)) = x
busqueda [] (Hoja x) = x 
busqueda (Der:dirs) (Ramificacion arbol1 _ arbol2) = busqueda dirs arbol2
busqueda (Izq:dirs) (Ramificacion arbol1 _ arbol2) = busqueda dirs arbol1
busqueda _ _ = error "El arbol se termino antes de terminar tu busqueda!"

