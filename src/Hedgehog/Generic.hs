{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{- |

Generic implementation of a generator. Example usage:

@
data Foo = Foo
  { _fooX :: X
  , _fooY :: Y
  } deriving (Generic)

genFoo :: Gen Foo
genFoo = hgen
@

The generated generator is equivalent to

@Foo \<$\> hgen \<*\> hgen@.

-}

module Hedgehog.Generic
  ( HGen(..)
  , hgen
  ) where

import Control.Applicative (liftA2)
import Data.Proxy (Proxy(..))
import GHC.Generics
import GHC.TypeLits
import Hedgehog
import qualified Hedgehog.Gen as Gen

-- | A class used to generate generators for types implementing 'Generic'.
class HGen a where
  hgen' :: Gen (a x)

instance HGen U1 where
  hgen' = pure U1

instance (Generic c, HGen (Rep c)) => HGen (K1 i c) where
  hgen' = K1 <$> hgen

instance HGen f => HGen (M1 i c f) where
  hgen' = M1 <$> hgen'

instance (HGen a, HGen b) => HGen (a :*: b) where
  hgen' = liftA2 (:*:) hgen' hgen'

instance
  forall a b. (KnownNat (SumLen a), KnownNat (SumLen b), HGen a, HGen b)
  => HGen (a :+: b) where
  hgen' = Gen.frequency
    [ (lfreq, L1 <$> hgen')
    , (rfreq, R1 <$> hgen')
    ]
    where
      lfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen a))
      rfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen b))

type family SumLen a :: Nat where
  SumLen (a :+: b) = SumLen a + SumLen b
  SumLen _ = 1

-- | If your type implements 'Generic', you can get a generator for
--   your type 'for free' using this function.
hgen :: (Generic a, HGen (Rep a)) => Gen a
hgen = to <$> hgen'
