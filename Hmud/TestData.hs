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
  ["Town Square"]

townSquare = mkRoom
  "Town Square"
  "the central meeting place of the town. There is a fountain, and lots of people are busy with their daily routine."
  ["The Black Unicorn"]

npc0 = Character
  { charName = "Chris"
  , charRace = Human
  , charRole = Fool
  , charGender = Male
  , charLevel = 17
  }

world = World { worldRooms = [tavern, townSquare] }
