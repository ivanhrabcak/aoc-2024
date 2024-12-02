import Data.List
import System.IO

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let inputLines = lines contents
  let input = map (map (read :: String -> Int) . words) inputLines
  print (part1 input)
  print (part2 input)

part1 :: [[Int]] -> Int
part1 input = length (filter id (zipWith (curry (\x -> uncurry (||) (fst x) && snd x)) (zip allIncreasing allDecreasing) inCorrectRange))
  where
    differences = map (\x -> zipWith (-) x (tail x)) input
    allIncreasing = map (all (>= 0)) differences
    allDecreasing = map (all (<= 0)) differences
    inCorrectRange = map (all ((\x -> x <= 3 && x >= 1) . abs)) differences

part2 :: [[Int]] -> Int
part2 reports = length (filter isSafe reports)
  where
    isSafe :: [Int] -> Bool
    isSafe x = isDirectlySafe || any (((== 1) . part1 . (: []) . (\(lst, r) -> remove r lst)) . (x,)) [0 .. length x - 1]
      where
        isDirectlySafe = part1 [x] == 1
        remove i (x : xs)
          | null xs = [x | i /= 0]
          | i == 0 = remove (i - 1) xs
          | otherwise = x : remove (i - 1) xs
