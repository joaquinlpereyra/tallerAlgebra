doble x = 2*(x)
suma x y = x + y
normaVectorial x y = sqrt(x**y + y**2)
funcionConstante x = 8
respuestaATodo = 42
signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1
absolute n | n >= 0 = n
           | n < 0 = n * (-1)

absolute_weird n = (n**2)**1/2

absolute_weird_2 n | n < 0 = n - n - n
                   | n >= 0 = n

sumaa x y = x + y
third_sum x y z = (sumaa x y) + z

absolute_weird_3 n | signo n >= 0 = n
                   | signo n < 0 = -1 * n

funcion_rara n1 n2 n3 | n1 < 10 = n1
                      | n2 >= 10 = n1 + n3

nand True True = False
nand _ _ = True

nor False False = True
nor _ _ = False

root1 a b c = (-b+(sqrt(b**2 - 4*a*c)))/(2*a)
root2 a b c = (-b-(sqrt(b**2 - 4*a*c)))/(2*a)
roots a b c = [root1 a b c] ++ [root2 a b c]
isPithagorean a b c = a ** 2 + b ** 2 == c ** 2
