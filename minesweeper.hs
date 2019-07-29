import System.Environment 
import Data.List
import Text.Printf

type Fila = [Char]
type Tablero = [Fila]

-- Fila de "l" de libres
filaL :: Int -> Fila
filaL l = replicate l '.'

-- Tablero de "n" filas de "l" libres
tabLibres :: Int -> Int -> Tablero
tabLibres n l = replicate n $ filaL l

-- Fila de "m" de minas
filaM :: Int -> Fila
filaM m = replicate m '*'

-- Tablero de "n" filas de "m" minas
tabMinas :: Int -> Int -> Tablero
tabMinas n m = replicate n $ filaM m

-- Fila de "m" minas seguida de "l" libres
filaML :: Int -> Int -> Fila
filaML m l = filaM m ++ filaL l

-- Tablero de "n" filas, cada una de "m" minas seguida de "l" libres
tablero :: Int -> Int -> Int -> Tablero
tablero n m l = replicate n $ filaML m l

dosFilas :: Int -> Int -> Maybe Tablero
dosFilas 1 0 = Just [".","."] -- tablero con 2 filas de 1 columna, sin minas
dosFilas c m
  | 2*c-m == 2 || odd m = Nothing -- con solo 2 libres imposible ganar o si es impar imposible ponerlos en 2 filas
  | otherwise = Just $ tablero 2 ms (c-ms) where ms = div m 2 -- divido la cantidad de minas entre las 2 filas, el resto libres

ultimasTresFilas :: Int -> (Int,Int) -> Tablero
ultimasTresFilas c (l, 0) = tablero 3 (c-l) l -- entran justo, 3 filas, rellenas con minas y "l" libres
ultimasTresFilas c (l, 1) = [filaML (c-l+1) (l-1)] ++ tablero 2 (c-l-1) (l+1) -- sobra 1, se pone en la última fila y se baja otra de la antepenúltima a la anteúltima fila
ultimasTresFilas c (l, 2) = [filaML (c-l) l] ++ tablero 2 (c-l-1) (l+1) -- sobran 2, se ponen en las 2 ultimas filas

tresFilas :: Int -> Int -> Int -> Maybe Tablero
tresFilas f c libres
  | elem libres [2,3,5,7] = Nothing -- con solamente 2,3,5,7 libres, no hay forma de acomodarlas para ganar
  | otherwise = Just $ tabMinas (f-3) c ++ (ultimasTresFilas c $ divMod libres 3) -- se rellena con minas, y se acomodan las últimas 3 filas

resGral :: Int -> Int -> (Int, Int) -> Tablero
resGral f c (l,0) = tabMinas (f-l) c ++ tabLibres l c -- entran justo, se rellena con minas y "l" filas libres
resGral f c (l,1) = tabMinas (f-l-1) c ++ [filaML (c-2) 2, filaML 1 (c-1)] ++ tabLibres (l-1) c -- sobra 1, se sube 1 de la fila de abajo, y se ponen las 2 juntas
resGral f c (l,s) = tabMinas (f-l-1) c ++ [filaML (c-s) s] ++ tabLibres l c -- sobran mas de 1, se rellena con minas y se ponen las "s" sobrantes en la fila

resolver :: Int -> Int -> Int -> Maybe Tablero
resolver f c m | f*c-m == 1 = Just $ tabMinas f c -- una sola libre, generamos el tablero con todas minas, total despues se sobreescribe la ultima celda con el click
resolver 1 c m = Just $ tablero 1 m (c-m) -- generamos tablero de 1 fila con m minas y el resto de libres
resolver f 1 m = fmap transpose $ resolver 1 f m -- con 1 columna, invertimos parametros para generar con 1 fila, y sacamos la transpuesta
resolver 2 c m = dosFilas c m
resolver f 2 m = fmap transpose $ resolver 2 f m
resolver f c m
  | m>=c*(f-3) = tresFilas f c libres -- si las minas llegan a tocar las últimas 3 filas, manejamos ese caso
  | otherwise = Just $ resGral f c $ divMod libres c -- el tablero tiene mas de 3 filas, y las minas no llegan a tocar las últimas 3, resolvemos el caso general
  where libres = f*c-m

ponerClick :: Tablero -> Tablero
ponerClick t = init t ++ [init (last t) ++ "c"]

getResultado :: Maybe Tablero -> String
getResultado Nothing = "Impossible"
getResultado (Just tablero) = unlines $ ponerClick tablero

resolverCaso :: [Int] -> String
resolverCaso [f, c, m] = getResultado $ resolver f c m

printCaso :: [String] -> Int -> [Char]
printCaso cs n = printf "Case #%d:\n" (n+1) ++ (resolverCaso $ map read $ words $ cs!!n)

resolverCasos :: [String] -> [String]
resolverCasos (n:cs) = map (printCaso cs) [0..(read n)-1]

larges :: [String]
larges = ["L", "l", "large", "Large"]

-- la idea es recibir por parametro l o L y que levante el archivo large, cualquier otra cosa levanta el small
elegirFile :: [String] -> (String, String)
elegirFile args
  | any (flip elem larges) args = ("C-large-practice.in", "C-large-practice.out")
  | otherwise = ("C-small-practice.in", "C-small-practice.out")

main :: IO ()
main = do
  args <- getArgs
  let (fIn, fOut) = elegirFile args
  casos <- readFile fIn
  writeFile fOut (unlines $ resolverCasos $ lines casos)
  putStrLn $ "Generado out: "++fOut
