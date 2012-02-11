module Item where

import Describable

data Item = Item
  { itemName :: String
  , itemDescription :: String
  }

instance Describable Item where
  name = itemName
  describe = itemDescription
