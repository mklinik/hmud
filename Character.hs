module Character where

import Describable
import Item

data Race = Human | Elven | Dwarf
  deriving (Eq, Show, Enum, Bounded)

data Role = Fool
          | Wizard 
          | Thief
          | Servant
          | Fisherman
          | Peasant
          | Blacksmith
          | Merchant
          | Priest
          | Warrior
          | Prince
          | King
  deriving (Eq, Show, Enum, Bounded)

data Gender = Male | Female
  deriving (Eq, Show, Enum, Bounded)

data Character = Character
  { charName :: String
  , charRace :: Race
  , charRole :: Role
  , charGender :: Gender
  , charLevel :: Int
  }

instance Describable Character where
  name = charName
  describe char = "a level " ++ (show $ charLevel char)
                       ++ " " ++ (show $ charGender char)
                       ++ " " ++ (show $ charRace char)
                       ++ " " ++ (show $ charRole char)
