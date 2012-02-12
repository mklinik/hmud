module Hmud.TestData where

import Hmud.Item
import Hmud.Describable
import Hmud.Character
import Hmud.Room
import Hmud.World
import Hmud.Util

amulet = Item { itemName = "Emperor's Amulet", itemDescription = "a shiny amulet, probably with magic powers" }

tavern = mkRoom
  "The Black Unicorn"
  "a dusty dark tavern. It smells of delicious food. You hear cheery background music."
  [(name townSquare)]

townSquare = mkRoom
  "town square"
  "the central meeting place of the town. There is a fountain, trees and flowers, and lots of people that are busy with their daily routine. The sun shines, birds sing and everybody is quite happy."
  [(name tavern), (name ivoryTower)]

ivoryTower = mkRoom
  "ivory tower"
  "a tall white building with long hallways, large laboratories and a big library. Inside it is completely quiet, except for the occasional reverberant sound of footsteps. In this place, scholars develop new crazy magic spells. You are at the very top of the tower, in a small chamber with windows to all sides. You can see the whole town from up here."
  [(name townSquare)]

npc0 = Character
  { charName = "Chris"
  , charRace = Human
  , charRole = Fool
  , charGender = Male
  , charLevel = 17
  }

world = World { worldRooms = [tavern, townSquare, ivoryTower] }
