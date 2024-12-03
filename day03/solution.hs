import Data.Either (fromLeft)
import Data.List
import Data.Maybe
import System.IO
import Text.Read (readMaybe)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let input = (foldl1 (++) . lines) contents
  print (part1 input)
  print (part2 input)

nextMulCall :: [Char] -> Either ((Int, Int), Int) Int
nextMulCall s
  | not startsWithMul || length colonIndices /= 1 || isNothing a || isNothing b = Right continueFrom
  | (Just x, Just y) <- (a, b) = Left ((x, y), continueFrom)
  where
    continueFrom = length s - length offsetString + 1
    offsetString = dropWhile (/= 'm') s
    wholeCall = takeWhile (/= ')') offsetString
    colonIndices = elemIndices ',' wholeCall
    colonIndex = head colonIndices
    startsWithMul = take 4 wholeCall == "mul("
    (aNumber, bNumber) = splitAt (colonIndex - 4) (drop 4 wholeCall)

    a = readMaybe aNumber :: Maybe Int
    b = readMaybe (tail bNumber) :: Maybe Int

nextDoCall :: [Char] -> Either (Bool, Int) Int
nextDoCall s
  | wholeCall == "do(" || wholeCall == "don't(" = Left (wholeCall == "do(", continueFrom)
  | otherwise = Right continueFrom
  where
    continueFrom = length s - length offsetString + 1
    offsetString = dropWhile (/= 'd') s
    wholeCall = takeWhile (/= ')') offsetString

part1 :: String -> Int
part1 s
  | s == "" = 0
  | Left ((x, y), continueFrom) <- mul = x * y + part1 (drop continueFrom s)
  | Right continueFrom <- mul = part1 (drop continueFrom s)
  where
    mul = nextMulCall s

part2 :: String -> Int
part2 s
  | s == "" = 0
  | otherwise = evaluate s True
  where
    getContinueFrom :: Either (a, Int) Int -> Int
    getContinueFrom k
      | (Left (_, continueFrom)) <- k = continueFrom
      | (Right continueFrom) <- k = continueFrom

    evaluateMul k
      | Left ((x, y), continueFrom) <- k = (x * y, continueFrom)
      | Right continueFrom <- k = (0, continueFrom)

    evaluate s isEnabled
      | s == "" = 0
      | closestCall == continueFromMul && isEnabled = multiplicationResult + evaluate (drop n s) isEnabled
      | Left (enable, continueFrom) <- doCall = evaluate (drop continueFrom s) enable
      | Right continueFrom <- doCall = evaluate (drop continueFrom s) isEnabled
      where
        mulCall = nextMulCall s
        continueFromMul = getContinueFrom mulCall
        doCall = nextDoCall s
        continueFromDo = getContinueFrom doCall
        closestCall = min continueFromDo continueFromMul
        (multiplicationResult, n) = evaluateMul mulCall