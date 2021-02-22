module Tablero (
Tablero,
Casilla(Vacia,Ocupada),
ColorPieza(CP),
Pieza(Peon,Torre,Caballo,Alfil,Reina,Rey),
Color(Blanca,Negra),
Posicion,
nuevo_tablero,posiblesMovmientos,piezas_enroque,minimax,color,comparaTableros,cambioDiccionario,
esPeonYPosicionesCorrectas,esJaqueMate,parseoPosicion,verifica,siguienteMovimiento,cambiarFicha,movTorreEnroque,
sacaPeon,tablas
)where


import Data.Matrix as Ma
import Data.Maybe
import qualified Data.Map as M

type Tablero    = Matrix Casilla
data Casilla    = Vacia | Ocupada ColorPieza
                    deriving (Eq)
data ColorPieza = CP (Color,Pieza) 
                    deriving (Eq)
data Pieza      = Peon | Torre | Caballo | Alfil | Rey | Reina 
                    deriving (Eq)

data Color      = Blanca | Negra
                    deriving(Eq,Show)

type Posicion   = (Int, Int)

data Direccion = Izquierda | Derecha
                    deriving(Eq)

------------------------------------------------------ TABLERO INICIAL------------------------------------------------------------------

{- 
A partir de una posición construimos una casilla que puede estar ocupada por una pieza con un color o vacía.
-}
tablero_inicial :: Posicion -> Casilla
tablero_inicial (i, j)
    | (i==2) = Ocupada (CP ( Negra,Peon ))
    | (i==7) = Ocupada (CP ( Blanca ,Peon ))

    | (i==1) && (j==1 || j==8)  = Ocupada (CP ( Negra, Torre))
    | (i==1) && (j==2 || j==7)  = Ocupada (CP ( Negra, Caballo))
    | (i==1) && (j==3 || j==6)  = Ocupada (CP ( Negra, Alfil))

    | (i==8) && (j==1 || j==8)  = Ocupada (CP ( Blanca, Torre))
    | (i==8) && (j==2 || j==7)  = Ocupada (CP ( Blanca, Caballo))
    | (i==8) && (j==3 || j==6)  = Ocupada (CP ( Blanca, Alfil))

    | (i==1) && (j==4)          = Ocupada (CP ( Negra, Reina))
    | (i==1) && (j==5)          = Ocupada (CP ( Negra, Rey))
    
    | (i==8) && (j==4)          = Ocupada (CP ( Blanca, Reina))
    | (i==8) && (j==5)          = Ocupada (CP ( Blanca, Rey))

    | otherwise = Vacia

{- 
Creamos el nuevo tablero con matrix a partir de la funcion tablero_inicial. Recorre cada posición desde (1,1) hasta (8,8) e incluye 
lo que nosotros le hayamos dicho.
-}
nuevo_tablero :: Tablero
nuevo_tablero = (matrix 8 8) tablero_inicial

  ------------------------------------------------------ MOVIMIENTOS------------------------------------------------------------------

{- 
En esta función le pasamos el tablero, color, una posición y el diccionario. Lo que hace es ponernos en una lista de string
los movimientos posibles que puede ejecutar dada una posició. filtramos por los posibles movimientos y devolvemos la clave. Parseamos
para que el usuario en lugar de meter un posición pueda meter las posiciones específicas del ajedrez como "a3". Hacemos concat
para agrupar todo en una lista.
-}
posiblesMovmientos :: Tablero -> Color -> Posicion -> M.Map [Char] [Bool] -> [String]
posiblesMovmientos t c pos mp = concat[ M.keys (M.filter (==q) ter) | q <- movimiento_con_chequeo_jaque t c pos mp]
   where listaAjedrez = [ [x] ++ [y]   | y <- reverse['1'..'8'], x <- ['a'..'h'] ]
         posAjedrez = [  (x,y)   | x <- [1..8] , y <- [1..8]]
         ter = parseo listaAjedrez posAjedrez

{- 
Hemos puesto una función genérica movimientos que dependiendo de la pieza nos redirige a otra función específica.
-}
movimientos :: ColorPieza -> Posicion -> [[Posicion]]
movimientos t@(CP (x,p)) pos
    | p == Peon = mov_peon t pos
    | p == Torre = mov_torre t pos
    | p == Caballo = mov_caballo t pos
    | p == Alfil = mov_alfil t pos
    | p == Reina = mov_reina t pos
    | p == Rey = mov_rey t pos

{- 
El peon si es blanca pues llama muevePeonBlanco que puede moverse 1 o 2 casillas si está en la salida y solamente 1 si está en otra 
posición. Siempre para adelante. Llama a movimiento_valido para asegurarnos que está en el tablero. Lo mismo para el muevePeonNegro. 
pero con distintas posiciones de inicio.
-}
mov_peon :: ColorPieza -> Posicion -> [[Posicion]]
mov_peon (CP (x, Peon)) p@(i,j) = if x == Blanca then muevePeonBlanco p else muevePeonNegro  p
    where   muevePeonNegro  (i,j) | i==2 = [[(i+t,j)] | t <- [1,2], movimiento_valido (i+t,j)]
                                  | otherwise = [[(i+t,j)] | t <- [1], movimiento_valido (i+t,j)]
            muevePeonBlanco (i,j) | i==7 = [[(i-t,j)] | t <- [1,2], movimiento_valido (i-t,j)]
                                  | otherwise = [[(i-t,j)] | t <- [1], movimiento_valido (i-t,j)]

{- 
La torre se mueve para cuatro lados: arriba,abajo,izquierda y derecha. Cada uno es una lista por compresión. Algunos hacemos reverse
cuando va en sentido contrario del incremento de i o de j. Así siempre nos da el primer valor, el primer pasito.
-}
mov_torre :: ColorPieza -> Posicion -> [[Posicion]]
mov_torre (CP (x, Torre)) p@(i,j) = [arriba, abajo, izquierda, derecha]
    where   arriba = [ (t,j) | t <- [i+1..8]]
            abajo = [ (t,j) | t <- reverse[1..i-1]]
            izquierda = [ (i,t) | t <- reverse[1..j-1]]
            derecha = [ (i,t) | t <- [j+1..8]]

{- 
El caballo se mueve en forma de L. Hemos puesto una lista que son todos los movimientos posibles del caballo en esa forma. Lo que hacemos
es verificar si el movimiento es valido y, si lo es, lo añadimos como una lista.
-}
mov_caballo :: ColorPieza -> Posicion -> [[Posicion]]
mov_caballo (CP (x, Caballo)) p@(i,j) = [ [(i+dx,j+dy)] | (dx,dy) <- movimiento_posibles, movimiento_valido (i+dx,j+dy)]
    where  movimiento_posibles = [(2,1), (2,-1), (-2,1), (-2,-1),(1,2), (1,-2), (-1,2), (-1,-2)]

{- 
El alfil se mueve en 4 direcciones: Arriba-derecha, Arriba-izquierda, Abajo-derecha, Abajo-izquierda. Al igual que con la torre, realizamos
una lista para cada uno y verificamos siempre que se encuentre el movimiento en el tablero.
-}
mov_alfil :: ColorPieza -> Posicion -> [[Posicion]]
mov_alfil (CP (x, Alfil)) p@(i,j) = [arriba_derecha,arriba_izquierda,abajo_derecha,abajo_izquierda]
    where   arriba_derecha = [(i+t,j+t) | t <- [1..7], movimiento_valido (i+t,j+t)]
            arriba_izquierda = [(i+t,j-t) | t <- [1..7],movimiento_valido (i+t,j-t)]
            abajo_derecha = [(i-t,j+t) | t <- [1..7], movimiento_valido (i-t,j+t)]
            abajo_izquierda = [(i-t,j-t) | t <- [1..7],movimiento_valido (i-t,j-t)]

{- 
El movimiento de la reina es el movimientos del Alfil más el movimiento de la torre. 
-}
mov_reina :: ColorPieza -> Posicion -> [[Posicion]]
mov_reina (CP (x, Reina)) p = mov_alfil (CP (x, Alfil)) p ++ mov_torre (CP (x, Torre)) p

{- 
El movimiento del rey es como el movimiento de la reina pero solo una casilla de distancia. Por eso cogemos la primera posición de cada 
lista, si existen elementos en ella.
-}
mov_rey :: ColorPieza -> Posicion -> [[Posicion]]
mov_rey (CP (x, Rey)) p = [ [head p]     | p <- mov_dama, p /= []]
    where mov_dama = mov_reina (CP (x, Reina)) p

{- 
Esta función es para comprobar si se encuentra en el tablero.
-}
movimiento_valido :: Posicion -> Bool
movimiento_valido (i,j) = elem i [1..8] && elem j [1..8]

{- 
Aquí verificamos si una casilla está vacía o no.
-}
esPosicionLibre :: Tablero -> Posicion -> Bool
esPosicionLibre t p = posicionTablero t p == Vacia

{- 
Aquí devolvemos una casilla en función de la posición en el tablero. Accedemos mediante ! a la casilla en esa posicion del tablero.
-}
posicionTablero :: Tablero -> Posicion -> Casilla
posicionTablero t p = t ! p

{- 
A partir de una casilla devolvemos una tupla con la pieza y su respectivo color. Solamente llamamos a esta función cuando estamos
seguro de que la casilla está ocupada, de lo contrario nos daría una excepcion por no ser exhaustivo.
-}
pieza_color :: Casilla -> ColorPieza
pieza_color (Ocupada c) = c

{- 
Esta es una función principal que dado un tablero, color y posición nos devuelve una lista de posiciones. Lo realizamos con un 
concatMap para que nos de solamente una lista. Le pasamos un filtro donde esa posicion esté disponible a partir de cada lista de
lista que devuelve la funcion movimientos.
-}
movimientos_posibles_generales :: Tablero -> Color -> Posicion -> [Posicion]
movimientos_posibles_generales t c p = concatMap (posicionesDisponiblesFiltro t c) (movimientos colorPieza p)
  where   colorPieza  = pieza_color casilla_ficha
          casilla_ficha = posicionTablero t p

{- 
Esta función a partir del tablero, color y lista de posiciones llamada en la función anterior nos filtra las posiciones para que no 
sobrepasemos ni una pieza nuestra ni la del enemigo. Coge en libres todos aquellos hasta que se encuentre un posicion que no lo es. Coge 
en ocupados el resto de posiciones partiendo del primero que está libre. Ahora si ocupados no está vacio y la primera posicion de 
ocupados hay un enemigo entonces añade a libres el primero de ocupados. En caso contrario me devuelve libres. Principalmente captura 
a otra pieza.
-}
posicionesDisponiblesFiltro :: Tablero -> Color -> [Posicion] -> [Posicion]
posicionesDisponiblesFiltro t c p 
  | not (null ocupados) && esPosicionOcupadaEnemigo t c (head ocupados) = (head ocupados):libres
  | otherwise = libres
  where libres = takeWhile (esPosicionLibre t) p
        ocupados = dropWhile (esPosicionLibre t) p

{- 
Mira si la posición en ese tablero es ocupada por el enemigo.
-}          
esPosicionOcupadaEnemigo :: Tablero -> Color -> Posicion -> Bool
esPosicionOcupadaEnemigo t c p = casilla_ocupada_enemigo (posicionTablero t p) c
  
{- 
En esta función mira si la casilla es ocupada por el enemigo. Se llama anteriormente para ver si la posicion está ocupada por el contrincante.
La hemos separado porque posteriormente vamos a necesitar una u otra dependiendo de los parámetros que necesitemos.
-}
casilla_ocupada_enemigo :: Casilla -> Color -> Bool
casilla_ocupada_enemigo Vacia _ = False
casilla_ocupada_enemigo (Ocupada (CP (c1, p))) c2
  | c1 /= c2 = True
  | otherwise = False

------------------------------------------------------ MOVIMIENTOS ESPECIALES--------------------------------------------------------

{- 
Esta función es especial ya que solamente hay un caso en el que el rey se mueve dos casillas, el enroque. Por ello hemos añadido 
posiciones_enroque que te comprueba varias cosas: Te comprueba tanto por la derecha como por la izquierda que la torre no se haya movido,
que no haya piezas entre el rey y la torre con la que se quiere hacer enroque y que no estén ni el rey amenazado ni las casillas intermedias
Si se cumple por la derecha y por la izquierda, el rey puede hacer enroque a la derecha o a la izquierda. Si se cumple por la derecha 
puede para la derecha, si se cumple por la izquierda puede para la izquierda y si no se cumple pues lista vacia. Así añadimos a los 
movimientos del rey la posibilidad de hacer enroque. 
-}
movimientos_posibles_rey :: Tablero -> Color -> Posicion -> M.Map [Char] [Bool] -> [Posicion]
movimientos_posibles_rey t c p@(x,y) dic = posiciones_normales ++ posiciones_enroque
  where posiciones_normales = movimientos_posibles_generales t c p
        comprobacionesDerecha = (compruebaEnMap p dic Derecha) && (compruebaContiguos t p Derecha) && (not(compruebaPosAmenazada t p Derecha dic))
        comprobacionesIzquierda = (compruebaEnMap p dic Izquierda) && (compruebaContiguos t p Izquierda) && (not(compruebaPosAmenazada t p Izquierda dic))
        posiciones_enroque | comprobacionesDerecha && comprobacionesIzquierda = [(x,y-2),(x,y+2)] 
                           | comprobacionesDerecha = [(x,y+2)]
                           | comprobacionesIzquierda = [(x,y-2)]
                           | otherwise = []

{- 
Esto es un movimiento especial del peón. Cuando una pieza enemiga esta en diagonal puede capturarlo y se mueve diagonal. Por eso usamos
posiciones_captura para que filtre primero el cruce como movimiento valido y tambien filtre si la posicion a la que quieres llegar es 
ocupada por el enemigo. Dependiendo del color restará o sumara para moverse diagonal.
-}
movimientos_posibles_peon :: Tablero -> Color -> Posicion -> [Posicion]
movimientos_posibles_peon t c p@(x,y) = posiciones_normales ++ posiciones_captura
  where posiciones_normales = takeWhile (esPosicionLibre t) ( concat ( movimientos (pieza_color (posicionTablero t p)) p))
        posiciones_captura = filter (esPosicionOcupadaEnemigo t c) (filtra_cruce)
        cruce   | c == Blanca = -1
                | otherwise = 1
        filtra_cruce = filter movimiento_valido [(x+cruce, y-1), (x+cruce, y+1)]

------------------------------------------------------ PARSEO------------------------------------------------------------------

{- 
Hemos utilizado Map para crear un diccionario donde las claves van a ser las posiciones exactas en el ajedrez como "a1" y el valor
será la posición asociada.
-}
parseo :: [String] -> [Posicion] -> M.Map String Posicion
parseo [] _ = M.empty
parseo (x:xs) p = M.insert x (head p) (parseo xs (tail p)) 

{- 
Aquí parseamos la posición exacta. A partir del diccionario anterior devolvemos la posición que corresponde con la posición exacta del 
ajedrez. Por ejemplo. si ponemos "a1" corresponderá a la posición (8,1). 
-}
parseoPosicion :: String -> Maybe Posicion
parseoPosicion t = M.lookup t (parseo listaAjedrez posAjedrez)
  where listaAjedrez = [ [x] ++ [y]   | y <- reverse['1'..'8'], x <- ['a'..'h'] ]
        posAjedrez = [  (x,y)   | x <- [1..8] , y <- [1..8]]

------------------------------------------------------ACTUALIZAR TABLERO------------------------------------------------

{- 
Si el elemento destino existe en los movimientos posibles entonces te devulve un tablero posible, si no existe, devuelve Nothing.
Para los movimientos posibles llamamos a movimiento_con_chequeo_jaque que se encarga de ver si está amenazado el rey para que no
puedas mover piezas que no salven al rey de estar en jaque.
-}
--Es el siguiente movimiento que puede una ficha hacer generando un nuevo tablero.
siguienteMovimiento ::Tablero -> Color -> Posicion -> Posicion -> M.Map [Char] [Bool] -> Maybe Tablero
siguienteMovimiento t c partida destino mp
    | elem destino movimiento_posibles = Just (mueveFicha t partida destino)
    | otherwise = Nothing
    where movimiento_posibles  = movimiento_con_chequeo_jaque t c partida mp
                                                                            
{- 
En chequeo vemos si el rey está amenazado a partir de la posicion del rey observamos que esa posicion no sea un movimiento del enemigo.
Cuando comprobamos eso por cada movimiento si está amenazado entonces devolvemos la lista como estaba y si no pues añadimos ese movimiento
a la lista. Esos movimientos vendrán de movimientos_libre_segun_ficha que hará una cosa u otra dependiendo de la pieza. 
-}
movimiento_con_chequeo_jaque :: Tablero -> Color -> Posicion -> M.Map [Char] [Bool] -> [Posicion]
movimiento_con_chequeo_jaque t c p mp = foldr (\x y -> if chequeo (nuevoTablero x) c then y else x:y) [] mov 
  where mov = movimientos_libres_segun_ficha t c p mp
        nuevoTablero x = mueveFicha t p x
        chequeo t c = amenazaRey t c mp

{- 
Si no es un movimiento legal, por ejemplo, movemos una ficha que no es nuestra o la casilla está vacía pues lista vacia.
Si la pieza es un peon pues llamamos a los movimientos especiales del peon por si puede efectuar el movimiento especial y lo mismo para el 
rey. En otro caso pues llama a movimientos generales.  
-}
movimientos_libres_segun_ficha :: Tablero -> Color -> Posicion -> M.Map [Char] [Bool] -> [Posicion]
movimientos_libres_segun_ficha t c p mp
  | not (movimiento_legal t c p) = []
  | pieza == Peon = movimientos_posibles_peon    t c p
  | pieza == Rey  = movimientos_posibles_rey t c p mp
  | otherwise                           = movimientos_posibles_generales t c p
  where (Ocupada (CP (color,pieza))) = posicionTablero t p
        movimiento_legal t c p    | casilla == Vacia = False
                                  | otherwise = not (casilla_ocupada_enemigo casilla c)
        casilla = posicionTablero t p 

{- 
Primero duplica la ficha con la posicion de destino y despues deja Vacia la posicion de partida.
-}
mueveFicha :: Tablero -> Posicion -> Posicion -> Tablero
mueveFicha t partida destino = setElem Vacia partida (ficha_duplicada (posicionTablero t partida))
  where ficha_duplicada cas = setElem cas destino t 

------------------------------------------------------JAQUE MATE O TABLAS------------------------------------------------

{- 
Aquí comprobamos simplemente si al que le toca mover tiene o no movimientos.
-}
compruebaFinal :: Tablero -> Color -> M.Map [Char] [Bool] -> Bool
compruebaFinal t c mp = null $ concat[ movimiento_con_chequeo_jaque t c pos mp | pos <- posEquipo t c]
  where posEquipo t c = lista_pos_enemigos t (color c) --cambio color para que sean mis piezas

{- 
Comprobamos si la posición del rey está amenazada por algún movimiento del enemigo.
-}
amenazaRey :: Tablero -> Color ->  M.Map [Char] [Bool] -> Bool
amenazaRey t c mp = chequeo t c
  where chequeo t c =  posicionAmenazada t c (posicionRey t c Rey)
        posicionRey t c pieza = head (foldr (\x y -> if pieza_color (posicionTablero t x) == (CP (c,pieza)) then x:y else y ) [] [  (p,q)  | p <- [1..8], q <- [1..8], not (esPosicionLibre t (p,q)) ])
        posicionAmenazada t c p = elem p (movimientosEnemigos t c mp)

{- 
Si no hay movimiento y el rey está amenazado es Jaque Mate.
-} 
esJaqueMate :: Tablero -> Color ->  M.Map [Char] [Bool] -> Bool
esJaqueMate t c mp = compruebaFinal t c mp && amenazaRey t c mp

{- 
Si no hay movimientos posibles y el rey no está amenazado, entonces es tablas.
-}
tablas :: Tablero -> Color ->  M.Map [Char] [Bool] -> Bool
tablas t c mp = compruebaFinal t c mp && not (amenazaRey t c mp)

------------------------------------------------------FUNCIONES AUXILIARES------------------------------------------------
{- 
Son todos los movimientos que el enemigo puede hacer con todas las piezas.
-}
movimientosEnemigos :: Tablero -> Color -> M.Map [Char] [Bool] -> [Posicion]
movimientosEnemigos t c mp = concat $ foldr (\x y -> (movimientos_libres_segun_ficha t (color c) x mp):y) [] (lista_pos_enemigos t c)

{-
A partir de todo el tablero encuentra todas las posiciones de las fichas del enemigo. Aunque esta función se pensó para los enemigos,
la hemos utilizado a veces para localizar las piezas del propio equipo cambiando el color.
-}
lista_pos_enemigos :: Tablero -> Color -> [Posicion] 
lista_pos_enemigos t c = [ (p,q)  | p <- take 8 [1..] , q <- take 8 [1..], esPosicionOcupadaEnemigo t c (p,q)] 

{-
Función auxiliar para que dado un color me de el del adversario.
-}
color :: Color -> Color
color c = if c == Blanca then Negra else Blanca


---------------------------FUNCIONES ENROQUE, PROMOCIÓN PEÓN Y CAPTURA PEÓN AL PASO--------------------------------------

{-
Se crea el diccionario que nos dará la información necesaria para saber si se puede realizar enroque o no.
-}
piezas_enroque :: M.Map [Char] [Bool]
piezas_enroque = M.insert "Negra" [True,True,True] (M.insert "Blanca" [True,True,True] M.empty)

{-
Esta función comprobará la posición del movimiento realizado y cambiará el diccionario en caso de que el movimiento implique no poder 
realizar enroque, ya sea por haber movido una torre o el rey.
-}
cambioDiccionario :: Posicion -> M.Map [Char] [Bool] -> M.Map [Char] [Bool]
cambioDiccionario p q 
  | p == (1,1) = M.adjustWithKey (funcionI) "Negra" q
  | p == (1,5) = M.adjustWithKey (funcionC) "Negra" q
  | p == (1,8) = M.adjustWithKey (funcionD) "Negra" q
  | p == (8,1) = M.adjustWithKey (funcionI) "Blanca" q
  | p == (8,5) = M.adjustWithKey (funcionC) "Blanca" q
  | p == (8,8) = M.adjustWithKey (funcionD) "Blanca" q
  | otherwise = q
  where funcionI k v = [ if i == 0 then False else t | (i,t) <- zip [0..] v]
        funcionC k v = [ if i == 1 then False else t | (i,t) <- zip [0..] v]
        funcionD k v = [ if i == 2 then False else t | (i,t) <- zip [0..] v]

{-
Comprueba si las casillas entre el rey y la torre están vacías.
-}
compruebaContiguos :: Tablero -> Posicion -> Direccion -> Bool
compruebaContiguos t p s
  | p == (1,5) && s == Izquierda = casilla (1,2) == Vacia && casilla (1,3) == Vacia && casilla (1,4) == Vacia 
  | p == (1,5) && s == Derecha   = casilla (1,7) == Vacia && casilla (1,6) == Vacia
  | p == (8,5) && s == Izquierda = casilla (8,2) == Vacia && casilla (8,3) == Vacia && casilla (8,4) == Vacia
  | p == (8,5) && s == Derecha   = casilla (8,7) == Vacia && casilla (8,6) == Vacia
  | otherwise = False
  where casilla np = posicionTablero t np 
        
{-
Comprueba si las casillas entre el rey y la torre están amenazadas.
-}
compruebaPosAmenazada :: Tablero -> Posicion -> Direccion -> M.Map [Char] [Bool] -> Bool
compruebaPosAmenazada t p s mp
  | p == (1,5) && s == Izquierda  = posicionAmenazada (1,2) Negra  && posicionAmenazada (1,3) Negra  && posicionAmenazada (1,4) Negra
  | p == (1,5) && s == Derecha    = posicionAmenazada (1,7) Negra  && posicionAmenazada (1,6) Negra
  | p == (8,5) && s == Izquierda  = posicionAmenazada (8,2) Blanca && posicionAmenazada (8,3) Blanca && posicionAmenazada (8,4) Blanca
  | p == (8,5) && s == Derecha    = posicionAmenazada (8,7) Blanca && posicionAmenazada (8,6) Blanca
  | otherwise = False
  where posicionAmenazada np c = elem np (movimientosEnemigos t c mp)

{-
Comprueba en el diccionario si el rey y las torres no se han movido para realizar el enroque.
-}
compruebaEnMap :: Posicion -> M.Map [Char] [Bool] -> Direccion -> Bool
compruebaEnMap p m s
  | p == (1,5) && s == Izquierda = ng !! 0 && ng !! 1
  | p == (1,5) && s == Derecha   = ng !! 2 && ng !! 1
  | p == (8,5) && s == Izquierda = bl !! 0 && bl !! 1
  | p == (8,5) && s == Derecha   = bl !! 2 && bl !! 1
  | otherwise = False
  where (Just ng) =  M.lookup "Negra" m
        (Just bl) =  M.lookup "Blanca" m

{-
Comprueba si el peón ha llegado a la última fila para promocionar.
-}
sacaPeon :: Tablero -> Color -> Posicion -> Bool
sacaPeon t c p@(i,j)
  | c == Blanca = comparaFichaColor && i==1
  | otherwise = comparaFichaColor && i==8
  where (Ocupada (CP (color,pieza))) = posicionTablero t p
        comparaFichaColor =  pieza == Peon && c == color 

{-
Cambia el peón promocionado por la pieza indicada.
-}
cambiarFicha :: Tablero -> Color -> String -> Posicion -> Tablero
cambiarFicha t c p destino = setElem (Ocupada (CP (c,pieza))) destino t
    where   pieza | p == "Torre" = Torre
                  | p == "Caballo" = Caballo
                  | p == "Alfil" = Alfil
                  | otherwise = Reina

{-
Esta función principalmente comprueba si un peón puede hacer la captura al paso. En el primero si el peon en la jugada anterior no
se ha movido dos movimientos será Nothing y por tanto devuelve el tablero. En el segundo si el destino es Nothing, es decir, 
estamos viendo que movimientos puede realizar la ficha, entonces tiene que estar el peón en la misma fila y una columna de distancia
así como en la fila adecuada dependiendo del color. Si por el contrario tenemos un destino entonces está haciendo el movimiento y debemos 
controlar la columna a la que vamos.
-}
verifica :: Tablero -> Color -> (Posicion,Maybe Posicion) -> Maybe Posicion -> Tablero
verifica t c ((i1,j1),Nothing) _ = t
verifica t c ((i1,j1), (Just p@(i2,j2))) Nothing
  | i1 == i2 && abs(j2-j1) == 1 && c == Negra  && i1 == 5 = mueveFicha t p (i2+1,j2)
  | i1 == i2 && abs(j2-j1) == 1 && c == Blanca && i1 == 4 = mueveFicha t p (i2-1,j2)
  | otherwise = t
verifica t c ((i1,j1), (Just p@(i2,j2))) (Just p2)
  | i1 == i2 && abs(j2-j1) == 1 && c == Negra  && i1 == 5 && p2 == (i2+1,j2) = mueveFicha t p (i2+1,j2)
  | i1 == i2 && abs(j2-j1) == 1 && c == Blanca && i1 == 4 && p2 == (i2-1,j2) = mueveFicha t p (i2-1,j2)
  | otherwise = t

{-
Comprueba con el movimiento de partida y el de destino si la pieza movida es un peón y si acaba de salir con un movimiento de 2 casillas.
-}
esPeonYPosicionesCorrectas :: Tablero -> Color -> Posicion -> Posicion -> Bool
esPeonYPosicionesCorrectas t c partida@(i1,j1) destino@(i2,j2)
  | c == Negra  && i1 == 2 && i2 == 4 && pieza == Peon = True
  | c == Blanca && i1 == 7 && i2 == 5 && pieza == Peon = True
  | otherwise = False
  where (Ocupada (CP (color,pieza))) = posicionTablero t partida

{-
Realiza el movimiento de la torre en el enroque.
-}
movTorreEnroque :: Tablero -> Posicion -> Posicion -> Tablero
movTorreEnroque t scr1 dst1
  | pieza == Rey && scr1 == (1,5) && dst1 == (1,7) = mueveFicha t (1,8) (1,6)
  | pieza == Rey && scr1 == (1,5) && dst1 == (1,3) = mueveFicha t (1,1) (1,4)
  | pieza == Rey && scr1 == (8,5) && dst1 == (8,7) = mueveFicha t (8,8) (8,6)
  | pieza == Rey && scr1 == (8,5) && dst1 == (8,3) = mueveFicha t (8,1) (8,4)
  | otherwise = t
  where CP (color,pieza) = pieza_color (posicionTablero t dst1) 


------------------------------------------------------INTELIGENCIA ARTIFICIAL------------------------------------------------

{-
Devuelve el valor asignado a cada pieza.
-}
obtenValorPieza :: Pieza -> Int
obtenValorPieza p =
  case p of
    Peon -> 100
    Caballo -> 300
    Alfil -> 350
    Torre -> 500
    Reina -> 900
    Rey -> 90000

{-
Dado un tablero, recorre todas las casillas y si está vacía devuelve 0, si está ocupada por las blancas suma y si por el contrario
está ocupada por las negras entonces resta. En definitiva devolvemos la diferencia de material en el tablero.
-}
evaluacion :: Tablero -> Int
evaluacion t = sum[ if casilla r == Vacia then 0 else if  (fst $ ter (casilla r)) == Blanca then obtenValorPieza (snd $ ter (casilla r)) else -(obtenValorPieza (snd $ ter (casilla r)))   | r <- posicionesPosibles]
  where casilla r = posicionTablero t r 
        ter (Ocupada (CP (color, pieza))) = (color,pieza)
        posicionesPosibles = [ (x,y)   | x <- [1..8], y <- [1..8]]

{-
Son dos métodos que se van llamando recursivamente. Dado un color recorre todas las posiciones posibles buscando el maximo, es decir,
la mejor. Esa función maximo o minimo la hemos creado para que si llega un momento que es Jaque Mate entonces te devuelve el tablero 
anterior con un valor sumamente grande o pequeño para que lo seleccione posteriormente. Antes de pasarle el tablero comprobamos
si puede enrocarse y tambien comprobamos si el diccionario se cambia porque ha movido el rey o alguna torre. Al final devuelve
el tablero optimo y la mejor evaluación. Para la IA solamente hemos utilizado la diferencia de material.
Si somos las blancas y la inteligencia son las negras entonces negamos la evaluación. Si por el contrario empieza la inteligencia
artificial como las blancas entonces dejamos la evaluación tal cual.
-}
minimax :: Int -> Tablero -> Color -> M.Map [Char] [Bool] -> Bool -> (Tablero, Int)
minimax 0 t _ _ True = (t,-(evaluacion t))
minimax 0 t _ _ False = (t,(evaluacion t))
minimax n t c mp b =  maximum' (t,-99999999) [((tablPosEnroque x), (snd $ minimax' (n-1) (tablPosEnroque x) (color c) (dic2 (tablPosEnroque x)) b ))   | x <- todasPos t c mp]
  where dic2 x = cambioDiccionario (fst (dev x)) mp
        dev x = comparaTableros t x (color c) mp
        tablPosEnroque x = movTorreEnroque x (fst (dev x)) (snd (dev x))

minimax' 0 t _ _ True = (t,-(evaluacion t))
minimax' 0 t _ _ False = (t,(evaluacion t))
minimax' n t c mp b = minimum' (t,99999999) [((tablPosEnroque y), (snd $ minimax (n-1) (tablPosEnroque y) (color c) (dic2 (tablPosEnroque y)) b ))   | y <- todasPos t c mp]
  where dic2 y = cambioDiccionario (fst (dev y)) mp
        dev y = comparaTableros t y (color c) mp
        tablPosEnroque y = movTorreEnroque y (fst (dev y)) (snd (dev y))

{-
Estas dos funciones devuelven una tupla con el tablero y la evaluacion. Si la lista es vacía es porque no hay movimientos y por tanto
te devuelve el tablero anterior con su respectiva valoración. Si por el contrario tiene elementos entonces buscamos la máxima evaluación.
-}
maximum' :: Ord a => (t,a) -> [(t, a)] -> (t, a)
maximum' tablero []     = tablero
maximum' tablero (x:xs) = maximo x xs
  where maximo t [] = t
        maximo (tablero', evaluacion) (t:ts)
          | evaluacion < (snd t) = maximo t ts
          | otherwise   = maximo (tablero', evaluacion) ts

minimum' :: Ord a => (t,a) -> [(t, a)] -> (t, a)
minimum' tablero []     = tablero
minimum' tablero (x:xs) = minimo x xs
  where minimo t [] = t
        minimo (tablero', evaluacion) (t:ts)
          | evaluacion > (snd t) = minimo t ts
          | otherwise   = minimo (tablero', evaluacion) ts

{-
Lo usamos para encontrar todas los movimientos de todas las fichas del color que le toca jugar en el tablero.
-}
todasPos :: Tablero -> Color -> M.Map [Char] [Bool] -> [Tablero]
todasPos t c mp = concat[ rs x  | x <- posiciones, not $ null $ rs x]
  where posiciones = lista_pos_enemigos t color
        color = if c == Blanca then Negra else Blanca
        rs x = [ mueveFicha t x destino | destino <- movimiento_con_chequeo_jaque t c x mp] 


------------------------------------------------------POSICIONES INTELIGENCIA ARTIFICIAL------------------------------------------------

{-
Como nosotros no movemos la IA entonces necesitamos obtener la posición de partida y de destino para hacer comprobaciones ya sea
para el enroque o para la captura del peón. Por ello recorremos todas las posiciones del tablero y comprobamamos que dos fichas estén
distintas. Como cuando haces un movimiento lo haces al destino comprobamos si el primer elemento de la lista está y si está pues devolvemos
una tupla con el último y el primero. En caso contrario devolvemos el primero y el último. Así nos aseguramos (partida, destino) en la IA.
-}
comparaTableros:: Tablero -> Tablero -> Color -> M.Map [Char] [Bool] -> (Posicion,Posicion)
comparaTableros t nt c mp = if elem (head posIA) mov then (last posIA, head posIA) else (head posIA, last posIA)
  where posIA = [l | l <- lista, posicionTablero t l /= posicionTablero nt l]
        lista = [(x,y) | x <- [1..8], y <- [1..8]]
        mov = movimientosEnemigos t c mp
