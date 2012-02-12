module Hmud.World where

import Data.List (isPrefixOf, find, delete, deleteBy, intercalate)
import qualified Control.Monad.Error
import Data.Either (rights)

import Hmud.Describable
import Hmud.Room
import Hmud.Character

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


-- may fail for various obvious reasons
gotoFromTo :: String -> String -> String -> World -> Either String World
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
          Right $ world { worldRooms = newFromRoom:newToRoom:remainingRooms }

worldSummary :: World -> String
worldSummary world = intercalate "\n" (map roomSummary (worldRooms world))

findRoomOfPlayer :: String -> World -> [Room]
findRoomOfPlayer playerName world = filter (roomHasCharacter playerName) (worldRooms world)

-- Assuming that player names are unique, if we find any players at all, we only find one
findCharacterExactly :: String -> World -> Either String Character
findCharacterExactly playerName world =
  case rights $ map (findCharacterInRoomExactly playerName) (worldRooms world) of
    []        -> Left $ "no such character: " ++ playerName
    p:[]      -> Right p
    otherwise -> Left $ "ambiguous character name: " ++ playerName
