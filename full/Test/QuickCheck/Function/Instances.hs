{-# LANGUAGE CPP #-}

#ifndef NO_POLYKINDS
{-# LANGUAGE PolyKinds #-}
#endif

module Test.QuickCheck.Function.Instances where

import Test.QuickCheck.Light.Function

#ifndef NO_NATURALS
import Numeric.Natural
#endif

#ifndef NO_PROXY
import Data.Proxy (Proxy (..))
#endif

#ifndef NO_NONEMPTY
import Data.List.NonEmpty(NonEmpty(..))
#endif

#ifndef NO_NATURALS
instance Function Natural where
  function = functionIntegral
#endif

#ifndef NO_NONEMPTY
instance Function a => Function (NonEmpty a) where
  function = functionMap g h
   where
     g (x :| xs) = (x,   xs)
     h (x,   xs) =  x :| xs
#endif

#ifndef NO_PROXY
instance Function (Proxy a) where
  function = functionMap (const ()) (const Proxy)
#endif
