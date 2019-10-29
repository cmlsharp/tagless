module Language.Tagless.Common.Lift where

class Lift_ expr a where
  lift_ :: a -> expr a

{- 

-- I want to write something like this, but I know I can't...
-- Pretty-printers / expression size counters screw it up

instance (Lift_ expr a, List_ expr) => Lift_ expr [a] where
  lift_ [] = nil_
  lift_ (x: xs) = cons $: (lift_ x) $: lift_ xs

-}
