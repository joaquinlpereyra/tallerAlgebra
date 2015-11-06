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
espejar (Rose x (rosetrees:z)) = Rose x (z : espejar' rosetrees)
