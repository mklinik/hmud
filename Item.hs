module Item where

import Describable

data Item = Item
  { itemName :: String
  , itemDescription :: String
  }
  deriving Eq

instance Describable Item where
  name = itemName
  describe = itemDescription
