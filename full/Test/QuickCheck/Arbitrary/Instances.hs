{-# LANGUAGE CPP #-}
#ifndef NO_POLYKINDS
{-# LANGUAGE PolyKinds #-}
#endif
module Test.QuickCheck.Arbitrary.Instances where

import Test.QuickCheck.Light.Arbitrary
import Control.Monad(liftM2)

#ifndef NO_PROXY
import Data.Proxy (Proxy (..))
#endif

#ifndef NO_NONEMPTY
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (mapMaybe)
#endif

#ifndef NO_NATURALS
import Numeric.Natural
#endif

#ifndef NO_PROXY
instance Arbitrary1 Proxy where
  liftArbitrary _ = return Proxy
  liftShrink _ _ = []

instance Arbitrary (Proxy a) where
  arbitrary = return Proxy
  shrink _  = []

instance CoArbitrary (Proxy a) where
  coarbitrary _ = id
#endif

#ifndef NO_NONEMPTY
instance Arbitrary1 NonEmpty where
  liftArbitrary arb = liftM2 (:|) arb (liftArbitrary arb)
  liftShrink shr (x :| xs) = mapMaybe nonEmpty . liftShrink shr $ x : xs

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance CoArbitrary a => CoArbitrary (NonEmpty a) where
  coarbitrary (x :| xs) = coarbitrary (x, xs)
#endif

#ifndef NO_NATURALS
instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral

instance CoArbitrary Natural where
  coarbitrary = coarbitraryIntegral
#endif
