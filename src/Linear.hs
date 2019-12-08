{-# LANGUAGE NamedFieldPuns, RecordWildCards, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-missing-methods #-}
module Linear (Linear(..)) where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

data Linear a b = Linear { constant :: a, variables :: Map b a } deriving (Eq, Show)

instance (Num a, Eq a, Ord b) => Num (Linear a b) where
    Linear c1 v1 + Linear c2 v2 = Linear (c1 + c2) $
        Map.filter (/= 0) $ Map.unionWith (+) v1 v2
    Linear c1 v1 * Linear c2 v2
      | Map.null v1 = Linear (c1 * c2) . Map.filter (/= 0) $ (c1 *) <$> v2
      | Map.null v2 = Linear (c1 * c2) . Map.filter (/= 0) $ (* c2) <$> v1
    abs Linear {..} | Map.null variables = Linear (abs constant) Map.empty
    signum Linear {..} | Map.null variables = Linear (signum constant) Map.empty
    fromInteger (fromInteger -> constant) = Linear { variables = Map.empty, .. }
    negate Linear {..} = Linear (negate constant) $ negate <$> variables

instance (Ord a, Ord b) => Ord (Linear a b) where
    Linear c1 v1 `compare` Linear c2 v2
      | Map.null v1, Map.null v2 = compare c1 c2

instance (Enum a, Ord b) => Enum (Linear a b) where
    succ linear@Linear {constant} = linear {constant = succ constant}
    pred linear@Linear {constant} = linear {constant = pred constant}
    toEnum (toEnum -> constant) = Linear { variables = Map.empty, .. }
    fromEnum Linear {..} | Map.null variables = fromEnum constant

instance (Real a, Ord b) => Real (Linear a b) where
    toRational Linear {..} | Map.null variables = toRational constant

instance (Integral a, Ord b) => Integral (Linear a b) where
    Linear c1 v1 `quotRem` Linear c2 v2
      | Map.null v1, Map.null v2, (q, r) <- c1 `quotRem` c2
      = (Linear q Map.empty, Linear r Map.empty)
    Linear c1 v1 `divMod` Linear c2 v2
      | Map.null v1, Map.null v2, (d, m) <- c1 `divMod` c2
      = (Linear d Map.empty, Linear m Map.empty)
    toInteger Linear {..} | Map.null variables = toInteger constant
