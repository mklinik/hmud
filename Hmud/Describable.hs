module Hmud.Describable where

class Describable a where
  describe :: a -> String
  name :: a -> String
