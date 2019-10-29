module Language.Tagless.Simple where


import Language.Tagless.Common.Apply

class Apply_ expr => Lambda_ expr where 
  lam :: (expr a -> expr b) -> expr (a -> b)


infixr 9 .:
(.:) :: Lambda_ expr
  => expr (b -> c)
  -> expr (a -> b)
  -> expr (a -> c)
-- | Ordinary (non-strict) object-language function composition.
f .: g = lam $ \x -> f $: (g $: x)

-- for consistency with DeBruijn
var :: expr a -> expr a
var = id

const_ :: Lambda_ expr => expr (a -> b -> a)
const_ = lam $ \x -> lam $ \_ -> x

flip_ :: Lambda_ expr => expr ((a -> b -> c) -> b -> a -> c)
flip_ = lam $ \f -> lam $ \x -> lam $ \y -> f $: y $: x

id_ :: Lambda_ expr => expr (a -> a)
id_ = lam id
