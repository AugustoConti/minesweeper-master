import Data.List ((\\))
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











generar :: Int -> Int -> Int -> [[Int]]
generar r c m = filter (\xs -> length (filter (==1) xs) == m ) $ replicateM (r*c) [1,0]



tieneUnaIsla :: [Int] -> Bool
tieneUnaIsla xs =  


int isSafe(int M[][COL], int row, int col, bool visited[][COL]) 
{ 
    return (row >= 0) && (row < ROW) && (col >= 0) && (col < COL) && (M[row][col] && !visited[row][col]); 
} 
  
void DFS(int M[][COL], int row, int col, bool visited[][COL]) 
{ 
    static int rowNbr[] = { -1, -1, -1, 0, 0, 1, 1, 1 }; 
    static int colNbr[] = { -1, 0, 1, -1, 1, -1, 0, 1 }; 
  
    visited[row][col] = true; 
  
    for (int k = 0; k < 8; ++k) 
        if (isSafe(M, row + rowNbr[k], col + colNbr[k], visited)) 
            DFS(M, row + rowNbr[k], col + colNbr[k], visited); 
} 

int countIslands(int M[][COL]) 
{ 
    bool visited[ROW][COL]; 
    memset(visited, 0, sizeof(visited)); 
      int count = 0; 
    for (int i = 0; i < ROW; ++i) 
        for (int j = 0; j < COL; ++j) 
            if (M[i][j] && !visited[i][j]) { 
                DFS(M, i, j, visited); 
                ++count; 
            } 
    return count; 
} 







