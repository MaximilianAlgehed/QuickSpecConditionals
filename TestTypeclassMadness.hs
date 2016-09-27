import TypeclassMadness
import Test.QuickCheck
import QuickSpec
import Data.Dynamic

sig =
  signature {
    maxTermSize = Just 10,
    constants = [
       constant "[]" ([] :: [Int]),
       constant ":"  ((:) :: Int -> [Int] -> [Int]),
       constant "++" ((++) :: [Int] -> [Int] -> [Int]),
       constant "head" (head :: [Int] -> Int)
    ]
   }

prds = [predicate ((not . null) :: [Int] -> Bool)]

main = quickSpec $ predicateSig sig prds
