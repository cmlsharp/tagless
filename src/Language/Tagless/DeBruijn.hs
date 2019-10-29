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
  lamDB :: expr (e, a) b -> expr e (a -> b)

  -- | The zero'th (most-recently bound) variable
  v0 :: expr (e, a) a

  -- | Extend environment.
  weaken :: expr e a -> expr (e, x) a


lam :: Lambda_ expr
  => (forall x. expr (e,a) a -> expr (e,a) b)
  -> expr e (a -> b)
lam body = lamDB $ body v0

lam2 :: Lambda_ expr
  => (forall x y. expr ((e,x),y) a -> expr ((e,x),y) b -> expr ((e,x),y) c)
  -> expr e (a -> b -> c)
lam2 body = lam $ \x -> lam $ \y -> body (var x) (var y)

lamM :: (Functor f, Lambda_ expr)
  => (forall x. expr (e,x) a -> f (expr (e,x) b))
  -> f (expr e (a -> b))
lamM body = lamDB <$> body v0

lamM2 :: (Functor f, Lambda_ expr)
  => (forall x y. expr ((e,x),y) a -> expr ((e,x),y) b -> f (expr ((e,x),y) c))
  -> f (expr e (a -> b -> c))
lamM2 body = lamM $ \x -> lamM $ \y -> body (var x) (var y)


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
