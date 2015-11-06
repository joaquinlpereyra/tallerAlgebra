data RT t = Rose t [RT t] deriving Show

r1 = Rose 0 []
r2 = Rose 1 [Rose 0 [], Rose 1 [], Rose 2[]]
r3 = Rose 2 [Rose 0 [], Rose 1 [Rose 0 [], Rose 1 []], Rose 2 [], Rose 3[]]
r4 = Rose 1 [Rose 0 [Rose 2 [Rose 4 [], Rose 3 [], Rose 1 [] ]], Rose 4 []]
r7 = Rose 0 [Rose 1 [Rose 2 [Rose 3 [Rose 4 [ Rose 5 [Rose 6 []]]]]]]

raiz :: RT t -> t
raiz (Rose x _) = x

hijos :: RT t -> [RT t]
hijos (Rose _ x) = x

sumarTodo :: Num t => RT t -> t
sumarTodo (Rose x []) = x
sumarTodo (Rose x rosetrees) = x + sumarTodo' rosetrees

sumarTodo' :: Num t => [RT t] -> t
sumarTodo' [] = 0
sumarTodo' (Rose x xs:rts) = x + sumarTodo' xs + sumarTodo' rts

hojas :: RT t -> [t]
hojas (Rose x []) = [x]
hojas (Rose x rosetrees) = x : hojas' rosetrees

hojas' :: [RT t] -> [t]
hojas' [] = []
hojas' (Rose x xs:rts) = x : hojas' xs ++ hojas' rts

espejar :: RT t -> RT t
espejar (Rose x []) = Rose x []
espejar (Rose main rosetrees) = Rose main (espejar' rosetrees)

espejar' :: [RT t] -> [RT t]
espejar' [] = []
espejar' (rose:rosetrees) = espejar' rosetrees ++ [espejar rose]

altura :: RT t -> Integer
altura (Rose x []) = 1
altura (Rose node rosetrees) = 1 + altura' rosetrees

altura' :: [RT t] -> Integer
altura' [] = 0
altura' (rose:rosetrees) = max (altura rose) (altura' rosetrees)

maximo :: Ord t => RT t -> t
maximo (Rose x []) = x
maximo (Rose node rosetrees) = getBigger (node : (maximo' rosetrees))

maximo' :: Ord t => [RT t] -> [t]
maximo' [] = []
maximo' (rose:rosetrees) = maximo rose : (maximo' rosetrees)

getBigger :: Ord t => [t] -> t
getBigger [] = error "Empty list"
getBigger [x] = x
getBigger (x:xs) | x >= getBigger xs    = x
                 | otherwise            = getBigger xs
