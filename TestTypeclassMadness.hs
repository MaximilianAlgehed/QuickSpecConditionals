import TypeclassMadness
import Test.QuickCheck
import QuickSpec
import Data.Dynamic

sig =
  signature {
    maxTermSize = Just 10,
    instances = [baseType (undefined :: Predicates)], -- This is only here because the real generator breaks QuickSpec
    constants = [
       constant "[]" ([] :: [Int]),
       constant ":"  ((:) :: Int -> [Int] -> [Int]),
       constant "++" ((++) :: [Int] -> [Int] -> [Int]),
       constant "head" (head :: [Int] -> Int)
    ]
   }

prds = [predicate ((not . null) :: [Int] -> Bool)]

instance Arbitrary Predicates where
    -- This is broken in QuickSpec, but running "ghci> fmap (extract 0) Test.QuickCheck.generate arbitrary :: IO [Int]" runs fine
    -- and generates an actual value, what is the problem here?!
    arbitrary = fst $ preds prds {-do
                    a <- arbitrary `suchThat` (not . null) :: Gen [Int]
                    return $ P [(1, [toDyn a])]
                    -}

main = quickSpec $ predicateSig sig prds
