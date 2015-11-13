-- DATOS Y SHOW
type Pixel = (Integer, Integer, Integer)
type PixelDelta = (Integer, Integer, Integer)
type Frame = [[Pixel]]
type FrameComprimido = [(Integer, Integer, PixelDelta)]

data Video = Iniciar Frame | Agregar Frame Video deriving Eq
instance Show Video
   where show (Iniciar f) = mostrarFrame f
         show (Agregar f v) = (mostrarFrame f) ++ "\n" ++ (show v)

data VideoComprimido = IniciarComp Frame | AgregarNormal Frame VideoComprimido | AgregarComprimido FrameComprimido VideoComprimido
instance Show VideoComprimido
   where show (IniciarComp f) = "INICIAL \n" ++ mostrarFrame f
         show (AgregarNormal f v) = "NO COMPRIMIDO \n" ++ (mostrarFrame f) ++ "\n" ++ (show v)
         show (AgregarComprimido f v) = "COMPRIMIDO \n" ++ (mostrarFrameComprimido f) ++ "\n" ++ (show v)

mostrarFrame :: Frame -> String
mostrarFrame [] = ""
mostrarFrame (x:xs) = (show x) ++ "\n" ++ (mostrarFrame xs)

mostrarFrameComprimido :: FrameComprimido -> String
mostrarFrameComprimido [] = ""
mostrarFrameComprimido (x:xs) = "\t" ++ (show x) ++ "\n" ++ (mostrarFrameComprimido xs)

-- Ejercicio 1/5
ultimoFrame :: Video -> Frame
ultimoFrame (Iniciar frame) = frame 
ultimoFrame (Agregar frame _) = frame

-- *Main> ultimoFrame video0 == f1
-- True

-- Ejercicio 2/5
norma :: (Integer, Integer, Integer) -> Float
norma (a, b, c) = (sqrt (fromInteger(a^2 + b^2 + c^2)))
-- *Main> norma (10, 20, 30)
-- 37.416573

-- Ejercicio 3/5
{- La idea fue achicar el problema de un frame hacia solamente una fila de pixeles.
 - El problema principal fue calcular la posicion de cada pixel, por eso necesitamos pixelsDiferentesenFrame' 
 - Con esa función, logramos que se tenga en cuenta la cantidad de iteraciones
 - pixelsDiferentesEnLinea, por otra parte, simplemente resuelve el problema en sí:
 - si didTheyChange es True, nos devuelve las posiciones (y,x) y el cambio delta
 - Notar que (y,x) está en ese orden porque en principio asumimos que y era 
 - la posición vertical & x la posición horizontal, pero el ejercicio menciona
 - que el primer valor de la tupla son las filas (pos. vertical) y el segundo
 - las columnas (pos. horizontal) así que en vez de cambiar todo el código simplemente
 - invertimos eso al final :)
 -}
pixelsDiferentesEnFrame :: Frame -> Frame -> Float -> FrameComprimido
pixelsDiferentesEnFrame []  _ _ = []
pixelsDiferentesEnFrame frames1 frames2 u =
    diferentesEnFrame' frames1 frames2 u 0 0

diferentesEnFrame' :: Frame -> Frame -> Float -> Integer -> Integer -> FrameComprimido
-- helper para pixelsDiferentesEnFrame. Toma coordenadas (x,y)
diferentesEnFrame' [] _ _ _ _ = []
diferentesEnFrame' (f1:frames1) (f2:frames2) u x y =
    diferentesEnLinea f1 f2 u x y ++ difEnOtrasLineas
    where difEnOtrasLineas = diferentesEnFrame' frames1 frames2 u 0 (y+1) 

diferentesEnLinea :: [Pixel] -> [Pixel] -> Float -> Integer -> Integer -> FrameComprimido
-- se encarga del problema reducido de comparar frames y compara listas de píxeles. 
diferentesEnLinea [] _ _ _ _ = []
diferentesEnLinea (p1:ps) (p2:pz) u x y 
    | didTheyChange p1 p2 u = (y,x,restaPixels p1 p2) : restoDelaLinea
    | otherwise             = restoDelaLinea 
    where restoDelaLinea = diferentesEnLinea ps pz u (x+1) y

didTheyChange :: Pixel -> Pixel -> Float -> Bool
-- ¿cambiaron los píxeles según el umbral?
didTheyChange pixel1 pixel2 umbral 
    | norma (restaPixels pixel1 pixel2) > umbral = True
    | otherwise                                  = False

restaPixels :: Pixel -> Pixel -> PixelDelta
-- devuelve el pixelDelta entre dos pixeles
restaPixels (a,b,c) (d,e,f) = (a-d, b-e, c-f)

comprimirFrame :: Frame -> Frame -> Float -> FrameComprimido
-- solo un nombre más claro y conciso para pixelsDiferentesEnFrame
comprimirFrame f1 f2 umbral = pixelsDiferentesEnFrame f1 f2 umbral


-- *Main> pixelsDiferentesEnFrame v1f1 v2f2 1
-- [(0,0,(3,3,3)),(0,1,(3,3,3)),(1,0,(3,3,3)),(1,2,(-3,-3,-3)),(2,1,(-3,-3,-3)),(2,2,(-3,-3,-3))]


-- Ejercicio 4/5
{- Los casos de comprimir uno y dos frames son bastante fáciles, ya que el primer frame nunca se puede comprimir
 - La función sonMuyDistintos nos permite saber si habría que gasterse en comprimir dos frames o no
 - Si son muy distintos, directamente guardamos el frame normal. Si, no comprimimos con pixelsDiferentesEnFrame
 - El tercer caso (más de dos frames) usa recursión para checkear si el penúltimo frame puede ser comprimido
 - usando el último. 
 - Si es así, lo comprime y se llama a sí misma pero ya sin el último frame. 
 - Si no fuese así, simlemente lo agrega como está y no lo checkea más
 - Aunque en principio parece que el segundo y tercer caso se podrían poner en una sola función,
 - como el tipo de datos Video no permite estar vacío el tercero no incluye al segundo. 
 -}

comprimir :: Video -> Float -> Integer -> VideoComprimido
comprimir (Iniciar frame) _ _ = IniciarComp frame

comprimir (Agregar ultimoF (Iniciar primerF)) uDelta uCantidad
    | distintos      = AgregarNormal ultimoF (IniciarComp primerF)
    | not distintos  = AgregarComprimido ultimoFComp (IniciarComp primerF)

    where ultimoFComp  = comprimirFrame ultimoF primerF uDelta 
          distintos    = sonMuyDistintos ultimoF primerF uDelta uCantidad

comprimir (Agregar ultimoF (Agregar penultimoF (video))) uDelta uCantidad
    | distintos      = AgregarNormal ultimoF (comprimirResto)
    | not distintos  = AgregarComprimido ultimoFComp (comprimirResto)

    where ultimoFComp    = comprimirFrame ultimoF penultimoF uDelta 
          videoMasChico  = (Agregar penultimoF (video))
          comprimirResto = comprimir (videoMasChico) uDelta uCantidad
          distintos      = sonMuyDistintos penultimoF ultimoF uDelta uCantidad


sonMuyDistintos :: Frame -> Frame -> Float -> Integer -> Bool
sonMuyDistintos f1 f2 uDelta uCantidad 
    | fromIntegral (length pixelsDiferentes) > uCantidad = True
    | otherwise                                          = False
    where pixelsDiferentes = pixelsDiferentesEnFrame f1 f2 uDelta 


-- Ejercicio 5/5
descomprimir :: VideoComprimido -> Video
 {-
 - las funciones provistas por la cátedra ayudan mucho
 - los tres primeros casos son triviales
 - el cuarto caso es el más complejo:
 - si se encuentra con dos frames comprimidos seguidos, 
 - sumará sus diferencias hasta que encuentre un frame no comprimido
 - del que podrá reconstruir la información del original
 - probablemente no sea la manera más eficiente, porque tiene que hacer
 - una recursión por todos los frames para recuperar solo uno. 
 - sospechamos que hay una mejor manera, que involucraría 
 - recuperar la información del anterior al original primero
 - y con ese recuperar la info del anterior al anterior y así sucesivamente
 - el problema es que eso involucraría varias (potencialmente muchas)
 - funciones auxiliares que lidien con frames, framesComprimidos y listas
 - de frames y cosas así, así que para mantener el programa simple
 - elegimos sacrificar algo de eficiencia :)
 - el quinto caso es bastante más simple:
 - si hay un frame comprimido y luego uno normal, recupera los cambios
 - y sigue con el resto del video recursivamente.
 -}
descomprimir (IniciarComp inicial) = (Iniciar inicial)
descomprimir (AgregarNormal f (video)) = Agregar f (descomprimir video)
descomprimir (AgregarComprimido fComp (IniciarComp inicial)) = 
    Agregar original (Iniciar inicial)
    where original = aplicarCambio inicial fComp

descomprimir (AgregarComprimido fComp1 (AgregarComprimido fComp2 (video))) = 
    Agregar frameOriginal (descomprimir restoVideo)
    where frameOriginal = ultimoFrame (descomprimir sumatoria)
          sumatoria = AgregarComprimido (sumarCambios fComp1 fComp2) (video)
          restoVideo = AgregarComprimido fComp2 (video)

descomprimir (AgregarComprimido fComp (AgregarNormal fNormal (video))) =
    Agregar original (descomprimir restoVideo)
    where original = aplicarCambio fNormal fComp
          restoVideo = AgregarNormal fNormal (video)

-- Funciones provistas por la cátedra
sumarCambios :: FrameComprimido -> FrameComprimido -> FrameComprimido
sumarCambios fc1 fc2 = [(i, j, sumar deltas (busqueda i j fc2)) | (i, j, deltas) <- fc1] ++
                       [(i, j, deltas) | (i, j, deltas) <- fc2, busqueda i j fc1 == (0,0,0)]
-- *Main> sumarCambios [(1,1,(2,2,2)),(2,2,(0,0,-1))] [(1,1,(-3,-3,-3)), (1,2,(1,1,1))]
-- [(1,1,(-1,-1,-1)),(2,2,(0,0,-1)),(1,2,(1,1,1))]

aplicarCambio :: Frame -> FrameComprimido -> Frame
-- aplicar cambio dado un frame y un comprimido
-- te devuelve el frame original 
aplicarCambio f fc = [ [nuevoVal f i j fc| j <- [0..length (f !! i) - 1]] | i <- [0..length f - 1]]
  where nuevoVal f i j fc = sumar ((f !! i) !! j) (busqueda (fromIntegral i) (fromIntegral j) fc)
--  *Main> aplicarCambio [[(1,1,1),(2,2,2)],[(3,3,3),(4,4,4)]] [(0, 1, (1,2,3))]
--  [[(1,1,1),(3,4,5)],[(3,3,3),(4,4,4)]]

busqueda :: Integer -> Integer -> FrameComprimido -> PixelDelta
busqueda i j [] = (0, 0, 0)
busqueda i j ((x, y, c) : cs) | x == i && j == y = c
                            | otherwise = busqueda i j cs

sumar :: PixelDelta -> PixelDelta -> PixelDelta
sumar (x,y,z) (x2,y2,z2) =  (x+x2,y+y2,z+z2)

-- PRUEBAS
p3 :: Pixel
p3 = (3,3,3)

p2 :: Pixel
p2 = (2,2,2)

p1 :: Pixel
p1 = (1,1,1)

p0 :: Pixel
p0 = (0,0,0)

-- Video 0:
f0 = [[p0, p0, p0], [p3, p3, p3]]
f1 = [[p3, p3, p3], [p3, p3, p3]]
video0 = Agregar f1 (Agregar f0 (Iniciar f0))

videoCustom = Agregar f1 (Agregar f0 (Agregar f0 (Agregar f1 (Agregar f0 (Agregar f0 (Iniciar f0))))))

-- Video 1:  En la versión comprimida, todos los frames son comprimidos (salvo el inicial)

v1f1 :: Frame
v1f1 = [[p3, p3, p0, p0, p0],
      [p3, p3, p0, p0, p0],
      [p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p0]]

v1f2 :: Frame
v1f2 = [[p0, p0, p0, p0, p0],
      [p0, p3, p3, p0, p0],
      [p0, p3, p3, p0, p0],
      [p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p0]]

v1f3 :: Frame
v1f3 = [[p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p0],
      [p0, p0, p3, p3, p0],
      [p0, p0, p3, p3, p0],
      [p0, p0, p0, p0, p0]]

v1f4 :: Frame
v1f4 = [[p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p0],
      [p0, p0, p0, p3, p3],
      [p0, p0, p0, p3, p3]]


v1 :: Video
v1 = Agregar v1f4 (Agregar v1f3 (Agregar v1f2 (Iniciar v1f1)))

v1Comp :: VideoComprimido
v1Comp = comprimir v1 1 6


-- Video 2:  En la versión comprimida, sólo los frames 2 y 4 son comprimidos

v2f1 :: Frame
v2f1 = [[p3, p3, p0, p0, p0],
      [p3, p3, p0, p0, p0],
      [p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p0]]

v2f2 :: Frame
v2f2 = [[p0, p0, p0, p0, p0],
      [p0, p3, p3, p0, p0],
      [p0, p3, p3, p0, p0],
      [p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p0]]

v2f3 :: Frame
v2f3 = [[p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p0],
      [p0, p0, p3, p3, p3],
      [p0, p0, p3, p3, p0],
      [p0, p0, p0, p0, p0]]

v2f4 :: Frame
v2f4 = [[p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p0],
      [p0, p0, p0, p0, p3],
      [p0, p0, p0, p3, p3],
      [p0, p0, p0, p3, p3]]


v2 :: Video
v2 = Agregar v2f4 (Agregar v2f3 (Agregar v2f2 (Iniciar v2f1)))

v2Comp :: VideoComprimido
v2Comp = comprimir v2 1 6
