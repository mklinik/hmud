module Hmud.Item where

import Hmud.Describable

data Item = Item
  { itemName :: String
  , itemDescription :: String
  }
  deriving (Eq, Show, Read)

instance Describable Item where
  name = itemName
  describe = itemDescription
