import Data.List ((\\), sort)
import Control.Monad
data Cell = I | O deriving (Eq, Show)

celdas :: Int -> Int -> Int -> [Cell]
celdas r c m = (replicate m I) ++ (replicate (r*c-m) O)

permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)

permutaciones_1 :: Eq a => [a] -> [[a]]
permutaciones_1 [] = [[]]
permutaciones_1 xs = [a:p | a <- xs, p <- permutaciones_1(xs \\ [a])]

deleteDuplicate :: (Eq a) => [a] -> [a]
deleteDuplicate [] = []
deleteDuplicate (x:xs) = x : deleteDuplicate (filter (/= x) xs)

eliminaDuplicados1 :: Eq a => [a] -> [a]
eliminaDuplicados1 [] = []
eliminaDuplicados1 (x:xs) = x : eliminaDuplicados1 (elimina x xs)

elimina :: Eq a => a -> [a] -> [a]
elimina x [] = []
elimina x (y:ys) = if (x==y) then 
                          elimina x ys
                        else
                          y : elimina x ys

tableros r c m= eliminaDuplicados1 $ permutations $ celdas r c m

cellUp (r,c) =  (r-1,c)
cellUR (r,c) =  (r-1,c+1)
cellUL (r,c) =  (r-1,c-1)
cellDw (r,c) =  (r+1,c)
cellDL (r,c) =  (r+1,c-1)
cellDR (r,c) =  (r+1,c+1)
cellLt (r,c) =  (r,c-1)
cellRt (r,c) =  (r,c+1)


clickEn :: (Int,Int) -> [[Cell]] -> Cell
clickEn (r, c) t = t !! r !! c

resultado (r,c) t = if clickEn (r,c) t==I then
                   Nothing
                 else
                    Just O
                    
fromDecimal :: Int -> [Int]
fromDecimal 0 = [0]
fromDecimal n = mod n 2 : fromDecimal (div n 2)

--t= [[I,I,O,O],[O,I,I,O],[I,O,I,O],[O,O,I,I],[O,I,O,I],[I,O,O,I]]

--tableroGraph :: [[Cell]] -> String
--tableroGraph [] = ""
--tableroGraph (x:xs) = linea x ++ tableroGraph xs

--linea :: [Cell] -> String
--linea = foldl (\x acc -> if x==I 
--			then  (++) "B" acc 
--			else  (++) "."  acc )
--			O


tapada = 0
bomba = 1
destapada = 2
click = 3

data Tablero = T Int Int [Int] deriving (Show)


libresPos :: Tablero -> [Int]
libresPos (T f c ts) = foldr (\c r n -> if c == tapada then n:(r (n+1)) else r (n+1)) (\n -> []) ts 0

generar :: Int -> Int -> Int -> [Tablero]
generar r c m = map (\t -> T r c t) (filter (\xs -> length (filter (==1) xs) == m ) $ replicateM (r*c) [bomba,tapada])

--[0,0,0,0,0]


winner :: [Tablero] -> Maybe Tablero
winner = foldr (\t acc -> case acc of
                                Nothing -> isWinner t
                                Just t -> Just t) Nothing

isWinner :: Tablero -> Maybe Tablero
isWinner t = foldr (\p acc -> case acc of
                                   Nothing -> isWinnerIn p t
                                   Just t -> Just t) Nothing (libresPos t)
isWinnerIn 0 T
isWinnerIn 4 T
isWinnerIn 6 T

                                   
isWinnerIn :: Int -> Tablero -> Maybe Tablero
isWinnerIn p (T f c ts) = foldr (\t acc -> case acc of
                                                Nothing -> Just (destapar p t) -- checkear si no hay libres
                                                Just t -> Just t) Nothing  ts
--p: son todas las posiciones libres                                                
                                                

destapar :: Int -> Tablero -> Tablero
destapar p t = t

sing :: Bool -> [Int] -> [Int]
sing b l = if b then l else []

vecinas_old :: Int -> Tablero -> [Int]
vecinas_old p (T f c ts) = map (+p) (
                       sing (p>c-1) [-c]
                    ++ sing ((p>c-1) && (mod p c) > 0) [-c-1]
                    ++ sing ((p>c-1) && (mod p c) < c-1) [-c+1]
                    ++ sing ((mod p c) > 0) [-1]
                    ++ sing ((mod p c) < c-1) [1]
                    ++ sing (p<c*f-c && (mod p c) > 0) [c-1]
                    ++ sing (p<c*f-c) [c]
                    ++ sing (p<c*f-c  && (mod p c) < c-1) [c+1])
     
vecinas :: Int -> Tablero -> [Int]
vecinas p t = vecinaArribaIzquierda p t ++ vecinaArriba p t ++ vecinaArribaDerecha p t 
                        ++ vecinaDerecha p t ++ vecinaAbajoDerecha p t ++ vecinaAbajo p t 
                        ++ vecinaAbajoIzquierda p t ++ vecinaIzquierda p t 

  -- donde el primer p  de un tablero es 0
vecinaArribaIzquierda p (T f c ts) = if (p > c-1) && (mod p c)>0  then [p-(c+1)] else []

vecinaArriba p (T f c ts) = if (p > c-1) then [p-c] else []

vecinaArribaDerecha p (T f c ts) = if (p > c-1 && (mod (p+1) c)>0  ) then [p-c+1] else []

vecinaDerecha p (T f c ts) = if (mod (p+1) c)>0 then [p+1] else []

vecinaAbajoDerecha p (T f c ts) = if p<(f*c-1)-(c-1) && (mod (p+1) c)>0 then [p+1] else []

vecinaAbajo p (T f c ts) = if p<(f*c-1)-(c-1)  then [p+c] else []

vecinaAbajoIzquierda p (T f c ts) = if p<(f*c-1)-(c-1) && (mod p c) >0 then [p+(c-1)] else []

vecinaIzquierda p (T f c ts) = if (mod p c)>0 then [p-1] else []




areEqual a b = sort a == sort b

tableroTest = T 3 3 [0,0,0,0,0,0,0,0,0]
vecinasTest = 
    map (\(p, r) -> if areEqual (vecinas p tableroTest) r then "OK" else "Er: "++show p)
    [(0, [1,3,4]),
    (1, [0,2,3,4,5]),
    (2, [1,4,5]),
    (3, [0,1,4,6,7]),
    (4, [0,1,2,3,5,6,7,8]),
    (5, [1,2,4,7,8]),
    (6, [3,4,7]),
    (7, [3,4,5,6,8]),
    (8, [4,5,7])]


tableroTest2 = T 3 1 [0,0,0]
vecinasTest2 = 
    map (\(p, r) -> if areEqual (vecinas p tableroTest) r then "OK" else "Er: "++show p)
    [(0, [1]),
    (1, [0,2]),
    (2, [1])]


