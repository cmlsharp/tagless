module Language.Tagless.Common.Apply where

class Apply_ expr where
  infixl 1 $:
  ($:) :: expr (a -> b) -> expr a -> expr b
