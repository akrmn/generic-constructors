{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Constructors.Tagged
  ( constructors
  , constructors'
  ) where

import Control.Applicative
import GHC.Generics
import Data.Proxy
import Data.Kind
import Data.Tagged

class Constructors (f :: Type -> Type) where
  type Q f x y :: Type
  k :: forall x y. (f () -> x) -> (Q f x y -> y)

instance Constructors f => Constructors (D1 d f) where
  type Q (D1 d f) x y = Q f x y
  k :: forall x y. (D1 d f () -> x) -> (Q f x y -> y)
  k inj = k (inj . M1)

instance Constructors V1 where
  type Q V1 x y = y
  k :: forall x y. (V1 () -> x) -> (y -> y)
  k _ = id

instance (Constructors f, Constructors g) => Constructors (f :+: g) where
  type Q (f :+: g) x y = Q f x (Q g x y)
  k :: forall x y. ((f :+: g) () -> x) -> (Q f x (Q g x y) -> y)
  k inj =
    let
      r0 = k @g @x @       y  (inj . R1)
      l0 = k @f @x @(Q g x y) (inj . L1)
    in
      r0 . l0

class Arguments (f :: Type -> Type) where
  type Q0 f x :: Type
  k0 :: forall x. (f () -> x) -> Q0 f x

instance Arguments f => Constructors (C1 ('MetaCons n fx s) f) where
  type Q (C1 ('MetaCons n fx s) f) x y = Tagged n (Q0 f x) -> y
  k :: forall x y. (C1 ('MetaCons n fx s) f () -> x) -> ((Tagged n (Q0 f x) -> y) -> y)
  k inj f = f (Tagged (k0 (inj . M1)))

instance (Arguments f, Arguments g) => Arguments (f :*: g) where
  type Q0 (f :*: g) x = Q0 f (Q0 g x)
  k0 :: forall x. ((f :*: g) () -> x) -> Q0 f (Q0 g x)
  k0 inj = k0 (\f -> k0 (\g -> inj (f :*: g)))

instance Arguments f => Arguments (S1 s f) where
  type Q0 (S1 s f) x = Q0 f x
  k0 :: forall x. (S1 s f () -> x) -> Q0 f x
  k0 inj = k0 (inj . M1)

instance Arguments U1 where
  type Q0 U1 x = x
  k0 :: forall x y. (U1 () -> x) -> x
  k0 inj = inj U1

instance Arguments (K1 i t) where
  type Q0 (K1 i t) x = t -> x
  k0 :: forall x. (K1 i t () -> x) -> (t -> x)
  k0 inj = inj . K1

constructors :: forall t y. (Generic t, Constructors (Rep t)) => Q (Rep t) t y -> y
constructors = k @(Rep t) @t to

constructors' :: forall t y. (Generic t, Constructors (Rep t)) => Proxy t -> Q (Rep t) t y -> y
constructors' _ = constructors @t
