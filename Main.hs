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

main = do
  player <- randomCharacter "Markus"
  npc1 <- randomCharacter "Tom"
  npc2 <- randomCharacter "Karin"
  -- let room = townSquare
  -- putStrLn $ "Welcome " ++ (name player) ++ ", you are " ++ (describe player) ++ "."
  -- let room2 = roomEnter player $ roomEnter npc1 room
  -- putStrLn $ "You are in " ++ (name room2) ++ ", " ++ (describe room2)

  let world2 = case insertCharacterToRoom player "The Bl" world of Just w -> w; otherwise -> world
  putStrLn $ worldSummary world2
  let world3 = case gotoFromTo "Markus" "The Bl" "Tow" world2 of Just w -> w; otherwise -> world2
  putStrLn $ worldSummary world3
