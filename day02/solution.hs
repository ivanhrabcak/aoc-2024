import Data.List
import System.IO

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let inputLines = lines contents
  let input = map (map (read :: String -> Int) . words) inputLines
  print (part1 input)

part1 :: [[Int]] -> Int
part1 input = length (filter id (zipWith (curry (\x -> uncurry (||) (fst x) && snd x)) (zip allIncreasing allDecreasing) inCorrectRange))
  where
    differences = map (\x -> zipWith (-) x (tail x)) input
    allIncreasing = map (all (>= 0)) differences
    allDecreasing = map (all (<= 0)) differences
    inCorrectRange = map (all ((\x -> x <= 3 && x >= 1) . abs)) differences

part2 :: [[Int]] -> Int
part2 input = zip (zip3 numberIncreasing numberDecreasing numberInCorrectRange) lengths
  where
    differences = map (\x -> zipWith (-) x (tail x)) input
    numberIncreasing = map (length . filter (>= 0)) differences
    numberDecreasing = map (length . filter (<= 0)) differences
    lengths = map length differences
    numberInCorrectRange = map (length . filter ((\x -> x <= 3 && x >= 1) . abs)) differences