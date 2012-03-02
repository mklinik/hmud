module Hmud.TestData where

import Hmud.Item
import Hmud.Describable
import Hmud.Character
import Hmud.Room
import Hmud.World

amulet :: Item
amulet = Item { itemName = "Emperor's Amulet", itemDescription = "a shiny amulet, probably with magic powers" }
theOneRing :: Item
theOneRing = Item { itemName = "The One Ring", itemDescription = "precioussssssss" }
scroll0 :: Item
scroll0 = Item { itemName = "scroll", itemDescription = "that has incomprehensible things scribbled on it: boxes connected with lines. The scroll is titled: \"GTF design documents.\"" }
scroll1 :: Item
scroll1 = Item { itemName = "scroll of forgery", itemDescription = "enables the forge command" }
beer :: Item
beer = Item { itemName = "mug of beer", itemDescription = "filled with fresh, foaming, delicious beer." }

tavern :: Room
tavern = mkRoom
  "Black Unicorn"
  "a dusty dark tavern. It smells of delicious food. You hear cheery background music."
  [(name townSquare)]
  []

townSquare :: Room
townSquare = mkRoom
  "town square"
  "the central meeting place of the town. There is a fountain, trees and flowers, and lots of people that are busy with their daily routine. The sun shines, birds sing and everybody is quite happy."
  [(name tavern), (name ivoryTower)]
  []

ivoryTower :: Room
ivoryTower = mkRoom
  "ivory tower"
  "a tall white building with long hallways, large laboratories and a big library. Inside it is completely quiet, except for the occasional reverberant sound of footsteps. In this place, scholars develop new crazy magic spells. You are at the very top of the tower, in a small chamber with windows to all sides. You can see the whole town from up here."
  [(name townSquare)]
  []

npc0 :: Character
npc0 = Character
  { charName = "Chris"
  , charRace = Human
  , charRole = Fool
  , charGender = Male
  , charLevel = 17
  , charInventory = []
  , charAddress = ""
  , charId = "chris@localhost"
  }

player0 :: Character
player0 = Character
  { charName = "player0"
  , charRace = Human
  , charRole = Fool
  , charGender = Female
  , charLevel = 1
  , charInventory = []
  , charAddress = "player0addr"
  , charId = "player0@localhost"
  }

world :: World
world = World { worldRooms = [tavern, townSquare, ivoryTower], idleCharacters = [] }
