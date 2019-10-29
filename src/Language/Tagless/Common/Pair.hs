module Language.Tagless.Common.Pair where

class Pair_ expr where
  pair_ :: expr (a -> b -> (a,b))
  fst_  :: expr ((a,b) -> a)
  snd_  :: expr ((a, b) -> b)
