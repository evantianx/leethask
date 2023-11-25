import Control.Monad (guard)

nQueens :: Int -> [[Int]]
nQueens n = solve [] where
  solve xs
    | length xs == n = return xs
    | otherwise = do
        x <- [1..n]
        guard $ not $ any (threatens x) $ zip [1..] xs
        solve (x:xs)
  threatens x (i, y) = x == y || abs (x - y) == abs (i - 1)

formatSolution :: [Int] -> [String]
formatSolution = map (\x -> replicate (x - 1) '.' ++ "Q" ++ replicate (n - x) '.')

solveNQueens :: Int -> [[String]]
solveNQueens = map formatSolution . nQueens
