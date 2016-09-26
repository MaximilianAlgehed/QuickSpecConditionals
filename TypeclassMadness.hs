{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import QuickSpec
import Data.Dynamic

fromJust (Just x) = x

class Predicateable a where
    toPredicates :: a -> Gen (Maybe [Dynamic]) -- This becomes really problematic
                                               -- when we have a lot of predicates
                                               -- because we have to do backtracking many times
    getters      :: a -> [Int -> Constant]

instance Predicateable Bool where
    toPredicates True  = return (Just [])
    toPredicates False = return Nothing
    getters    = const []

instance (Predicateable b, Typeable a, Arbitrary a) => Predicateable (a -> b) where
    getters _ =
        (:)
        (\i ->
            constant
                ("acc"++show i)
                ((\(P preds) -> fromJust (fromDynamic (preds `at` i))) :: Predicates -> a))
        (map (\f -> f . (1+)) (getters (undefined :: b)))

    toPredicates predicate = do
                                a <- arbitrary
                                dyns <- toPredicates (predicate a)
                                case dyns of
                                    Nothing -> return Nothing
                                    Just xs -> return $ Just $ (toDyn a):xs

at :: [(Int, [a])] -> Int -> a
at [] _ = undefined
at ((j, xs):tl) i
    | i < j     = xs !! i
    | otherwise = tl `at` (i-j)

-- A type to hold _all_ predicates,
-- I imagine we will keep this type
-- hidden in QuickSpec
newtype Predicates = P [(Int, [Dynamic])] -- Arity + arguments

-- Dummy instances, don't matter since we never inspect
-- the type (only it's projections)
instance Eq Predicates where
    p == q = False

instance Ord Predicates where
    compare p q = LT
