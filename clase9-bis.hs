data Arbol t = Hoja t | Ramif (Arbol t) t (Arbol t) deriving (Show, Eq)

esHoja :: Arbol a -> Bool
esHoja (Hoja _) = True
esHoja _ = False

altura :: Arbol t -> Integer
altura (Hoja _) = 1
altura (Ramif arbol1 _ arbol2) = 1 + max (altura arbol1) (altura arbol2)

maximo :: Ord a => Arbol a -> a
maximo (Hoja x) = x
maximo (Ramif arbol1 y arbol2) | (y > (maximo arbol1) && y > (maximo arbol2)) = y
			       | otherwise				      =  max (maximo arbol1) (maximo arbol2)

raiz :: Arbol a -> a
raiz (Hoja x) = x
raiz (Ramif _  x _) = x

todosIguales :: Eq a => Arbol a -> Bool
todosIguales (Hoja _) = True
todosIguales (Ramif arbol1 x arbol2) | (x == raiz arbol1 && x == raiz arbol2) && (todosIguales arbol1) && (todosIguales arbol2) = True
				     | otherwise	= False

espejar :: Arbol a -> Arbol a
espejar (Hoja a) = Hoja a
espejar (Ramif arbol1 x arbol2) = Ramif arbol2 x arbol1

esHeap ::Ord a =>  Arbol a -> Bool
esHeap (Hoja _) = True
esHeap (Ramif arbol1 x arbol2) | (x <= raiz arbol1 && x <= raiz arbol2) && (esHeap arbol1) && (esHeap arbol2) = True
			       | otherwise = False

data Lista a = Vacia | Agregar a (Lista a) 
instance (Show a) => Show (Lista a) where
	show Vacia = "[]"
	show (Agregar a Vacia) = (show a)
	show (Agregar a b) = "[" ++ (show a) ++ "," ++  (show b) ++ "]"

vacia :: Lista a -> Bool 
vacia (Vacia) = True
vacia _	= False

suma :: Lista Float -> Float
suma (Vacia) = 0
suma (Agregar x lista) = x + suma lista

enPosicion :: Lista a -> Integer -> a 
enPosicion (Vacia) _ = error "El elemento no existe"
enPosicion (Agregar x lista) (0) = x
enPosicion (Agregar x lista) n =  enPosicion lista (n-1)

iguales :: Eq a => Lista a -> Lista a -> Bool
iguales (Vacia) (Vacia) = True
iguales (Agregar x lista1) (Agregar y lista2) | x /= y	= False
					    | otherwise	= iguales lista1 lista2
iguales _ _ = False

juntar :: Lista a -> Lista b -> Lista (a,b)
juntar (Vacia) (Vacia) = Vacia
juntar (Agregar x lista1) (Agregar y lista2) = Agregar (x,y) (juntar lista1 lista2)



