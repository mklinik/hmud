module Hmud.Room where

import Data.List (find, intercalate, isPrefixOf, delete)
import qualified Control.Monad.Error

import Hmud.Describable
import Hmud.Character
import Hmud.Item

data Room = Room { roomName :: String
                 , roomDescription :: String
                 , roomCharacters :: [Character]
                 , roomItems :: [Item]
                 , roomAdjacents :: [String]
                 }
  deriving (Eq)

instance Describable Room where
  name = roomName
  describe room = (roomDescription room) ++ people ++ items ++ directions
    where
      people =
        if (not $ null $ roomCharacters room)
          then "\n\nThese people are present:\n" ++ (intercalate ", " $ map name $ roomCharacters room)
          else ""
      items =
        if (not $ null $ roomItems room)
          then "\n\nThese things lie about:\n" ++ (intercalate ", " $ map name $ roomItems room)
          else ""
      directions =
        if (not $ null $ roomAdjacents room)
          then "\n\nFrom here you can go to:\n" ++ (intercalate ", " $ roomAdjacents room)
          else ""

instance Show Room where
  show = describe

mkRoom name description adjacents =
  Room { roomName = name
       , roomDescription = description
       , roomCharacters = []
       , roomItems = []
       , roomAdjacents = adjacents
       }

findCharacter :: String -> Room -> Either String Character
findCharacter playerName room = maybe (Left $ "no character " ++ playerName ++ " in " ++ (roomName room)) Right
  $ find (\char -> playerName `isPrefixOf` (charName char)) (roomCharacters room)

findCharacterInRoomExactly :: Address -> Room -> Either String Character
findCharacterInRoomExactly playerId room =
  maybe
    (Left $ "no character " ++ (show playerId) ++ " in " ++ (roomName room))
    Right
    (find (\char -> playerId == (charAddress char)) (roomCharacters room))

roomHasCharacterExactly :: Address -> Room -> Bool
roomHasCharacterExactly playerId room =
  either (const False) (const True) $ findCharacterInRoomExactly playerId room

roomEnter :: Character -> Room -> Room
roomEnter char room = room { roomCharacters = char:(roomCharacters room) }

roomSummary :: Room -> String
roomSummary room = (roomName room) ++ people
    where
      people =
        if (not $ null $ roomCharacters room)
          then ": " ++ (intercalate ", " $ map name $ roomCharacters room)
          else ": -"

findItemInRoom :: String -> Room -> Either String Item
findItemInRoom itName room = maybe (Left $ "no item " ++ itName ++ " in " ++ (roomName room)) Right
  $ find (\item -> itName `isPrefixOf` (itemName item)) (roomItems room)

removeItemFromRoom :: String -> Room -> Either String (Room, Item)
removeItemFromRoom itName room = do
  item <- findItemInRoom itName room
  let newRoom = room { roomItems = delete item (roomItems room) }
  Right (newRoom, item)

updateCharInRoom :: Character -> Room -> Either String Room
updateCharInRoom newChar room = do
  oldChar <- findCharacterInRoomExactly (charAddress newChar) room
  Right $ room { roomCharacters = newChar : (delete oldChar (roomCharacters room)) }
