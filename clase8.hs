data Figura = Rectangulo Float Float Float Float
	    | Circulo	 Float Float Float deriving Show

c1 :: Figura
c1 = Circulo 0 0 pi 

r1 :: Float -> Figura
r1 x = Rectangulo 0 0 (sin(0.25*pi)*x) (cos(0.25*pi)*x)

area :: Figura -> Float
area (Rectangulo x1 x2 y1 y2) = abs(x1-y1) * abs(x2-y2)
area (Circulo c1 c2 r)	 = pi*r**2

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
	show (CongruentesA a b) = "{a en Z | a = 3 (mod 8)}"

esMultiplo :: Integer -> Integer -> Bool
esMultiplo x y | mod x y == 0	= True
	       | otherwise	= False

pertenece :: Integer -> ProgAritmetica -> Bool
pertenece _ Vacio = False
pertenece n (CongruentesA x y) | esMultiplo n (x+y) = True
			       | otherwise	  = False 

incluido :: ProgAritmetica -> ProgAritmetica -> Bool
incluido _ Vacio = False
incluido Vacio _ = True 
incluido (CongruentesA a b) (CongruentesA c d) | (a == 0 || c == 0) && mod d b == 0 = True
					       | (a == 0 || c == 0) && mod d b /= 0 = False
					       | mod b d == 0 && (mod a d == mod c d) = True
					       | otherwise			    = False 
					       

