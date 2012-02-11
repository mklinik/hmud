import Item
import Describable
import Character
import Room
import Util

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

main = do
  player <- randomCharacter "Markus"
  npc1 <- randomCharacter "Tom"
  npc2 <- randomCharacter "Karin"
  let room = townSquare
  putStrLn $ "Welcome " ++ (name player) ++ ", you are " ++ (describe player) ++ "."
  let room2 = roomEnter player $ roomEnter npc1 room
  putStrLn $ "You are in " ++ (name room2) ++ ", " ++ (describe room2)
