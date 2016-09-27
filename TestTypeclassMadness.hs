import TypeclassMadness
import Test.QuickCheck
import QuickSpec
import Data.Dynamic

sig =
  signature {
    maxTermSize = Just 10,
    instances = [names (NamesFor ["p"] :: NamesFor Predicates),
                 baseType (undefined :: Predicates)
                ],
    constants = [
       constant "[]" ([] :: [Int]),
       constant ":"  ((:) :: Int -> [Int] -> [Int]),
       constant "++" ((++) :: [Int] -> [Int] -> [Int]),
       constant "head" (head :: [Int] -> Int),
       constant "acc0" ((\(P preds) -> fromJust $ fromDynamic (preds `at` 0) :: [Int]) :: Predicates -> [Int])
    ]-- ++(snd (preds prds))
   }

prds = [predicate ((not . null) :: [Int] -> Bool)]

instance Arbitrary Predicates where
    arbitrary = fst $ preds prds

main = quickSpec sig
