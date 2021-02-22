import Tablero
import Data.Maybe
import Data.Matrix as Ma
import qualified Data.Map as M
import Data.List.Split (endBy)

{-
Comienzo del juego. En este punto se eligen el número de jugadores y en caso de ser 1 jugador, qué color empieza.
-}
inicioJugadores:: IO()
inicioJugadores = do 
  putStrLn ("Numero de jugadores: 1 o 2")
  num <- getLine
  if num == "2"
    then do 
      juego1 (parseaColor "Blanca") num nuevo_tablero piezas_enroque Nothing True
  else if num == "1"
    then do 
      putStrLn ("Introduzca el color: Blanca o Negra")
      color <- getLine
      if color == "Blanca"
        then do 
          juego1 (parseaColor "Blanca") num nuevo_tablero piezas_enroque Nothing True
      else do 
        juego2 (parseaColor "Negra") num nuevo_tablero piezas_enroque Nothing False
  else do
    putStrLn ("Numero de jugadores incorrecto. Introduzca 1 o 2 jugadores")
    inicioJugadores

{-
Tratamiento de entrada y salida de forma recursiva de la partida de ajedrez, alternando las llamadas según le toque a un jugador o a la IA, y
si el juego es de 2 jugadores, se llama a juegoJugador para diferenciar entre una partida con IA y sin ella.
-}
juego1 :: Color -> String -> Tablero -> M.Map [Char] [Bool] -> Maybe Posicion -> Bool -> IO ()
juego1 c n t m p b = do 
  if n == "2" 
    then do juegoJugador n c t m p b
  else
    if c == Blanca 
      then do juegoJugador n c t m p b
    else do putStrLn ""
            putStrLn "    a  b  c  d  e  f  g  h "
            putStrLn "   ------------------------ "
            imprimeTablero t
            putStrLn "   ------------------------ "
            putStrLn "    a  b  c  d  e  f  g  h "
            putStrLn ""
            putStrLn "Mueve la inteligencia Artificial"
            
            let (game1,puntuacion) = minimax 3 t c m b
            let nuevo_turno = color c            
            let (src1,dst1) = comparaTableros t game1 nuevo_turno m
            let dic2 = cambioDiccionario src1 m
            let pos_anterior = if esPeonYPosicionesCorrectas game1 nuevo_turno src1 dst1 then Just dst1 else Nothing
            juego1 nuevo_turno n game1 dic2 pos_anterior b

juego2 :: Color -> String -> Tablero -> M.Map [Char] [Bool] -> Maybe Posicion -> Bool -> IO ()
juego2 c n t m p b = do 
  if c == Blanca 
    then do 
      let nuevo_turno = color c
      juegoJugador n nuevo_turno t m p b
  else do putStrLn ""
          putStrLn "    a  b  c  d  e  f  g  h "
          putStrLn "   ------------------------ "
          imprimeTablero t
          putStrLn "   ------------------------ "
          putStrLn "    a  b  c  d  e  f  g  h "
          putStrLn ""
          putStrLn "Mueve la inteligencia Artificial"
          let nuevo_turno = color c
          let (game1,puntuacion) = minimax 3 t nuevo_turno m b
          let (src1,dst1) = comparaTableros t game1 c m
          let dic2 = cambioDiccionario src1 m
          let pos_anterior = if esPeonYPosicionesCorrectas game1 c src1 dst1 then Just dst1 else Nothing
          juego2 nuevo_turno n game1 dic2 pos_anterior b

compruebaColor :: Color -> Bool
compruebaColor c 
  | c == Blanca = True
  | otherwise = False

{-
Dado un String por consola referente a un color ("Blanca" o "Negra") devuelve su tipo color correspondiente.
-}
parseaColor :: String -> Color
parseaColor s | s == "Blanca" = Blanca
              | otherwise = Negra

{-
En esta función juega el jugador en todo momento. Puede salir, ver los movimientos que puede hacer dado un punto de partida 
o hacer el movimiento. Si comete errores entonces se llamará recursivamente dando un mensaje de error. También controla la captura al
paso del peón así como el enroque. Además, si un peón llega al final, podemos elegir la pieza que queremos para promocionar.
-}
juegoJugador :: String -> Color -> Tablero -> M.Map [Char] [Bool] -> Maybe Posicion -> Bool -> IO ()
juegoJugador n turno game dic posicionAnterior b = do
    putStrLn ""
    putStrLn "    a  b  c  d  e  f  g  h "
    putStrLn "   ------------------------ "
    imprimeTablero game
    putStrLn "   ------------------------ "
    putStrLn "    a  b  c  d  e  f  g  h "
    putStrLn ""
    
    let contrario = if turno == Negra then "Blancas" else "Negras"
    if esJaqueMate game turno dic
      then putStrLn (contrario ++ " ganan por Jaque Mate.") 
    else if tablas game turno dic 
      then putStrLn ("Tablas por Rey ahogado.")
    else do
      putStrLn ("Mueven las " ++ if turno == Negra then "Negras" else "Blancas")
      putStrLn "Los comandos son: movimientos, mover, salir"
      userInput <- getLine
      case divideComando userInput of
          ("salir" ,         _) -> return ()
          ("movimientos",     pos:_) -> do
            if (parseoPosicion pos) == Nothing
              then do
                putStrLn "posición incorrecta"
            else do
              let (Just posE) = parseoPosicion pos
              let game1 = verifica game turno (posE, posicionAnterior) Nothing
              putStrLn ("Posibles movimientos son: " ++ show(posiblesMovmientos game1 turno (posE) dic))
            juegoJugador n turno game dic posicionAnterior b
          ("mover" , src:dst:_) -> do

            if (parseoPosicion src)==Nothing || (parseoPosicion dst)==Nothing
              then do 
                putStrLn "partida o salida mal escritos"
                juegoJugador n turno game dic posicionAnterior b
            else do
              let (Just src1) = parseoPosicion src
              let destino@(Just dst1) = parseoPosicion dst
              
              let game1 = verifica game turno (src1, posicionAnterior) destino
              let nuevo_juego = siguienteMovimiento game1 turno (src1) (dst1) dic
              
              if (nuevo_juego == Nothing)
                then do 
                  putStrLn "movimiento ilegal. Intentar de nuevo"
                  juegoJugador n turno game dic posicionAnterior b
              else do
                let (Just nj) = nuevo_juego
                let posicionAnterior = if esPeonYPosicionesCorrectas game1 turno src1 dst1 then Just dst1 else Nothing
                if sacaPeon nj turno dst1 
                  then do
                    putStrLn "Introduce la pieza que quieres cambiar: Torre, Caballo, Alfil, Reina"
                    inputPromocion <- getLine
                    let promoFicha = cambiarFicha nj turno inputPromocion dst1
                    let nuevo_turno = (color turno)
                    let dic2 = cambioDiccionario src1 dic
                    juegoJugador n nuevo_turno promoFicha dic2 posicionAnterior b
                else do
                   
                  let tablero_posible_enroque = movTorreEnroque nj src1 dst1
                  let nuevo_turno = (color turno)
                  let dic2 = cambioDiccionario src1 dic

                  case b of
                    True -> juego1 nuevo_turno n tablero_posible_enroque dic2 posicionAnterior b
                    False -> juego2 turno n tablero_posible_enroque dic2 posicionAnterior b
                  

          _ -> do
            putStrLn "Comando mal introducido. Intentar de nuevo"
            juegoJugador n turno game dic posicionAnterior b

{-
Muestra el contenido de una casilla en pantalla.
-}
imprime :: Casilla -> IO()
imprime Vacia = putStr " _ "
imprime (Ocupada (CP (c,p))) =
  case c of
    Negra -> putStr ("-" ++ pieza p)
    Blanca -> putStr ("+" ++ pieza p)
  where pieza Peon = "P "
        pieza Torre = "T "
        pieza Caballo = "C "
        pieza Alfil = "A "
        pieza Rey = "R "
        pieza Reina = "D "

{-
imprimeTablero nos devuelve una lista de listas ya que cada fila sería una lista. Por ello, esa lista para convertirlo en IO le ponemos
sequence_ y así nos representa lo que queramos. Cuando j es 0 o 9 entonces nos representa un barra y, por el contrario, llama a imprime 
para devolver la casilla.   
-}
imprimeTablero :: Tablero -> IO()
imprimeTablero t = do
  sequence_ [ sequence_ [ if j == 0 then putStr ((show (9-i) ++ " |")) else if j == 9 then putStrLn ("| " ++ (show (9-i))) else imprime (t Ma.! (i,j))  | j <- [0..9]] | i <- [1..8] ]

{- 
Con endBy podemos dividir lo que metamos por consola. Si se separa por espacio entonces nos coge como primero el comando específico ya sea
mover, movimientos o salir y como resto los parámetros que espera dependiendo de ese comando específico.
-}
divideComando :: String -> (String,[String])
divideComando ps = (primero,resto)
  where primero:resto = endBy " " ps

main :: IO()
main = inicioJugadores