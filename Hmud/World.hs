module Hmud.World where

import Data.List (isPrefixOf, find, delete, deleteBy, intercalate)
import qualified Control.Monad.Error
import Data.Either (rights)

import Hmud.Describable
import Hmud.Room
import Hmud.Character
import Hmud.Item

data World = World { worldRooms :: [Room]
                   }
  deriving Eq


findRoom :: String -> World -> Either String Room
findRoom rName world = maybe (Left $ rName ++ ": no such room") Right
  $ find (\room -> rName `isPrefixOf` (roomName room)) (worldRooms world)

insertCharacterToRoom :: Character -> String -> World -> Either String World
insertCharacterToRoom char toName world = do
  toRoom <- findRoom toName world
  let remainingRooms = delete toRoom (worldRooms world)
  let newToRoom = toRoom { roomCharacters = char:(roomCharacters toRoom) }
  return world { worldRooms = newToRoom:remainingRooms }

insertItemToRoom :: Item -> String -> World -> Either String World
insertItemToRoom item toName world = do
  toRoom <- findRoom toName world
  let remainingRooms = delete toRoom (worldRooms world)
  let newToRoom = toRoom { roomItems = item:(roomItems toRoom) }
  return world { worldRooms = newToRoom:remainingRooms }

-- may fail for various obvious reasons
gotoFromTo :: String -> String -> String -> World -> Either String (World, Room)
gotoFromTo playerName fromName toName world = do
      fromRoom <- findRoom fromName world
      toRoom   <- findRoom toName world
      if fromRoom == toRoom
        then (Left "You are already there.")
        else do
          let remainingRooms = delete toRoom $ delete fromRoom (worldRooms world)
          player <- findCharacter playerName fromRoom
          let newFromRoom = fromRoom { roomCharacters = delete player (roomCharacters fromRoom) }
          let newToRoom   = toRoom   { roomCharacters = player:(roomCharacters toRoom) }
          Right (world { worldRooms = newFromRoom:newToRoom:remainingRooms }, newToRoom)

worldSummary :: World -> String
worldSummary world = intercalate "\n" (map roomSummary (worldRooms world))

findRoomOfPlayerExactly :: String -> World -> Either String Room
findRoomOfPlayerExactly playerName world =
  case filter (roomHasCharacterExactly playerName) (worldRooms world) of
    []        -> Left $ "no such character: " ++ playerName
    r:[]      -> Right r
    otherwise -> Left $ "ambiguous character name: " ++ playerName -- should never happen

-- Assuming that player names are unique, if we find any players at all, we only find one
findCharacterExactly :: String -> World -> Either String Character
findCharacterExactly playerName world =
  case rights $ map (findCharacterInRoomExactly playerName) (worldRooms world) of
    []        -> Left $ "no such character: " ++ playerName
    p:[]      -> Right p
    otherwise -> Left $ "ambiguous character name: " ++ playerName

characterPickupItem :: String -> String -> World -> Either String (World, Item)
characterPickupItem chName itName world = do
  oldRoom <- findRoomOfPlayerExactly chName world
  oldChar <- findCharacterInRoomExactly chName oldRoom
  (tmpRoom, item) <- removeItemFromRoom itName oldRoom
  let newChar = giveItemToCharacter item oldChar
  newRoom <- updateCharInRoom newChar tmpRoom
  newWorld <- updateRoomInWorld newRoom world
  Right (newWorld, item)

updateRoomInWorld :: Room -> World -> Either String World
updateRoomInWorld newRoom world = do
  oldRoom <- findRoom (name newRoom) world
  Right $ world { worldRooms = newRoom : (delete oldRoom (worldRooms world)) }
