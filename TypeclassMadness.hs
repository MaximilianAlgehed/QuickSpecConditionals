{-# LANGUAGE ScopedTypeVariables #-}
module TypeclassMadness where
import Test.QuickCheck
import QuickSpec
import Data.Dynamic

fromJust (Just x) = x

class Predicateable a where
    toPredicates :: a -> Gen (Maybe [Dynamic]) -- This becomes really problematic
                                               -- when we have a lot of predicates
                                               -- because we have to do backtracking many times
    getters      :: Int -> String -> a -> [Int -> Constant]

    size         :: a -> Int

instance Predicateable Bool where
    toPredicates True  = return (Just [])
    toPredicates False = return Nothing
    getters _ _ _ = []
    size _ = 0

instance (Predicateable b, Typeable a, Arbitrary a) => Predicateable (a -> b) where
    getters indx name _ =
        (:)
        (\i ->
            constant
                (name++show indx)
                (extract i :: Predicates -> a))
        (map (\f -> f . (1+)) (getters (indx+1) name (undefined :: b)))

    -- here is where we could do the lazy predicate stuff for an instance
    toPredicates predicate = do
                                a <- arbitrary
                                dyns <- toPredicates (predicate a)
                                case dyns of
                                    Nothing -> return Nothing
                                    Just xs -> return $ Just $ (toDyn a):xs

    size _ = 1 + size (undefined :: b)

extract :: (Typeable a) => Int -> Predicates -> a
extract i (P preds) = fromJust $ fromDynamic $ preds `at` i

at :: [(Int, [a])] -> Int -> a
at [] _ = undefined
at ((j, xs):tl) i
    | i < j     = xs !! i
    | otherwise = tl `at` (i-j)

-- A type to hold _all_ predicates,
-- I imagine we will keep this type
-- hidden in QuickSpec
data Predicates = P {unP :: [(Int, [Dynamic])]} deriving(Show)-- Arity + arguments

-- Dummy instances, don't matter since we never inspect
-- the type (only it's projections)
instance Eq Predicates where
    p == q = False

instance Ord Predicates where
    compare p q = LT

type PredicateRep = ((Int, Gen (Maybe [Dynamic])), [Int -> Constant])

predicate :: (Predicateable a) => String -> a -> PredicateRep
predicate name p = ((size p, toPredicates p), getters 0 name p)

preds :: [PredicateRep] -> (Gen Predicates, [Constant])
preds xs = resolvePredicates $ unzip xs

resolvePredicates :: ([(Int, Gen (Maybe [Dynamic]))], [[Int -> Constant]]) -> (Gen Predicates, [Constant])
resolvePredicates (gen, getters) = (makeGen, concat $ zipWith (\fs i -> map ($i) fs) getters [0..])
    where
        makeOneGen :: (Int, Gen (Maybe [Dynamic])) -> Gen (Int, [Dynamic])
        makeOneGen (i, generator) = do
                                     v <- backtracking generator
                                     return (i, v)
        
        makeGen = fmap P $ sequence $ map makeOneGen gen

backtracking :: Gen (Maybe a) -> Gen a
backtracking g = do
                    x <- g
                    i <- resize 100 arbitrary
                    case x of
                        Nothing -> backtracking (scale (\j -> max (j+i) 0) g) -- We failed
                                                                              -- so we arbitrarily increase the size
                                                                              -- which is probably a bad idea in general
                        Just y  -> return y

predicateSig :: Signature -> [PredicateRep] -> Signature
predicateSig sig ps = let (gen, consts) = preds ps in
                        sig {constants = constants sig ++ consts,
                             instances = instances sig ++ [makeInstance (\() -> gen :: Gen Predicates),
                                                           names (NamesFor ["p"] :: NamesFor Predicates)
                                                          ]
                            }
