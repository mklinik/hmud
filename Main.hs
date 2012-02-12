import Hmud.Item
import Hmud.Describable
import Hmud.Character
import Hmud.Room
import Hmud.World
import Hmud.Util
import Hmud.TestData

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
