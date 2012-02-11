import Item
import Describable
import Character
import Util

amulet = Item { itemName = "Emperor's Amulet", itemDescription = "a shiny amulet, probably with magic powers" }

main = do
  putStrLn $ describe amulet
  char <- randomCharacter "Markus"
  putStrLn $ describe char
