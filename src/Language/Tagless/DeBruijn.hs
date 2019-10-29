{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Tagless.DeBruijn where


import Language.Tagless.Common.Apply

class (forall e . Apply_ (expr e)) => Lambda_ expr where 

  -- | Lambda abstraction
  lamDB :: expr (e,a) b -> expr e (a -> b)

  -- | The zero'th (most-recently bound) variable
  v0 :: expr (e,a) a

  -- | Extend environment.
  weaken :: expr e a -> expr (e,x) a


lam :: Lambda_ expr
  => (forall x. expr (e,a) a -> expr (e,a) b)
  -> expr e (a -> b)
lam f = lamDB $ f v0

lam2 :: Lambda_ expr
  => (forall x y. expr ((e,x),y) a -> expr ((e,x),y) b -> expr ((e,x),y) c)
  -> expr e (a -> b -> c)
lam2 f = lam $ \x -> lam $ \y -> f (var x) (var y)

lam3 :: Lambda_ expr
  => (forall x y z. expr (((e,x),y),z) a -> expr (((e,x),y),z) b -> expr (((e,x),y),z) c -> expr (((e,x),y),z) d)
  -> expr e (a -> b -> c -> d)
lam3 f = lam2 $ \x y -> lam $ \z -> f (var x) (var y) (var z)

lamM :: (Functor f, Lambda_ expr)
  => (forall x. expr (e,x) a -> f (expr (e,x) b))
  -> f (expr e (a -> b))
lamM f = lamDB <$> f v0

lamM2 :: (Functor f, Lambda_ expr)
  => (forall x y. expr ((e,x),y) a -> expr ((e,x),y) b -> f (expr ((e,x),y) c))
  -> f (expr e (a -> b -> c))
lamM2 f = lamM $ \x -> lamM $ \y -> f (var x) (var y)

lamM3 :: (Functor f, Lambda_ expr)
  => (forall x y z. expr (((e,x),y),z) a -> expr (((e,x),y),z) b -> expr (((e,x),y),z) c -> f (expr (((e,x),y),z) d))
  -> f (expr e (a -> b -> c -> d))
lamM3 f = lamM2 $ \x y -> lamM $ \z -> f (var x) (var y) (var z)

infixr 9 .:
(.:) :: Lambda_ expr
  => expr e (b -> c)
  -> expr e (a -> b)
  -> expr e (a -> c)
-- | Ordinary (non-strict) object-language function composition.
f .: g = lam $ \x -> var f $: (var g $: var x)

class Extends m n where
  var :: (Lambda_ expr) => expr m a -> expr n a

instance {-# OVERLAPS #-} Extends m m where
  var = id

instance (Extends m n, x ~ (n, e)) => Extends m x where
  var = weaken . var

-- Some useful target language functions

const_ :: Lambda_ expr => expr e (a -> b -> a)
const_ = lam $ \x -> lam $ \_ -> var x

flip_ :: Lambda_ expr => expr e ((a -> b -> c) -> b -> a -> c)
flip_ = lam $ \f -> lam $ \x -> lam $ \y -> var f $: var y $: var x

id_ :: Lambda_ expr => expr e (a -> a)
id_ = lam id
