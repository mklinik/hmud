module Hmud.World where

import Data.List (isPrefixOf, find, delete, deleteBy, intercalate)

import Hmud.Describable
import Hmud.Room
import Hmud.Character

data World = World { worldRooms :: [Room]
                   }
  deriving Eq

findRoom :: String -> World -> Maybe Room
findRoom rName world = find (\room -> rName `isPrefixOf` (roomName room)) (worldRooms world)

insertCharacterToRoom :: Character -> String -> World -> Maybe World
insertCharacterToRoom char toName world = do
  toRoom <- find (\room -> toName   `isPrefixOf` (roomName room)) (worldRooms world)
  let remainingRooms = delete toRoom (worldRooms world)
  let newToRoom = toRoom { roomCharacters = char:(roomCharacters toRoom) }
  return world { worldRooms = newToRoom:remainingRooms }


-- may fail for various obvious reasons
gotoFromTo :: String -> String -> String -> World -> Maybe World
gotoFromTo playerName fromName toName world = do
  fromRoom <- findRoom fromName world
  toRoom   <- findRoom toName world
  let remainingRooms = delete toRoom $ delete fromRoom (worldRooms world)
  player <- find (\char -> playerName `isPrefixOf` (charName char)) (roomCharacters fromRoom)
  let newFromRoom = fromRoom { roomCharacters = delete player (roomCharacters fromRoom) }
  let newToRoom   = toRoom   { roomCharacters = player:(roomCharacters toRoom) }
  return world { worldRooms = newFromRoom:newToRoom:remainingRooms }

worldSummary :: World -> String
worldSummary world = intercalate "\n" (map roomSummary (worldRooms world))
