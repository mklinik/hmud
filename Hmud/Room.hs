module Hmud.Room where

import Data.List (find, intercalate, isPrefixOf, delete)
import qualified Control.Monad.Error ()

import Hmud.Describable
import Hmud.Character
import Hmud.Item

data Room = Room { roomName :: String
                 , roomDescription :: String
                 , roomCharacters :: [Character]
                 , roomItems :: [Item]
                 , roomAdjacents :: [String]
                 }
  deriving (Eq, Show, Read)

instance Describable Room where
  name = roomName
  describe room = (roomDescription room) ++ people ++ items ++ directions
    where
      people =
        if (not $ null $ roomCharacters room)
          then "\n\nThese people are present:\n" ++ (intercalate ", " $ map name $ roomCharacters room)
          else "\n\nNo people are present."
      items =
        if (not $ null $ roomItems room)
          then "\n\nThese things lie about:\n" ++ (intercalate ", " $ map name $ roomItems room)
          else "\n\nNo items lie about."
      directions =
        if (not $ null $ roomAdjacents room)
          then "\n\nFrom here you can go to:\n" ++ (intercalate ", " $ roomAdjacents room)
          else "\n\nFrom here, you cannot go anywhere. YOU ARE TRAPPED! Holy cow, how did that happen?"

mkRoom :: String -> String -> [String] -> [Item] -> Room
mkRoom romName description adjacents items =
  Room { roomName = romName
       , roomDescription = description
       , roomCharacters = []
       , roomItems = items
       , roomAdjacents = adjacents
       }

findCharacterInRoom :: String -> Room -> Either String Character
findCharacterInRoom playerName room = maybe (Left $ "no character " ++ playerName ++ " in " ++ (roomName room)) Right
  $ find (\char -> playerName `isPrefixOf` (charName char)) (roomCharacters room)

findCharacterInRoomByAddress :: Address -> Room -> Either String Character
findCharacterInRoomByAddress playerAddr room =
  maybe
    (Left $ "no character " ++ (show playerAddr) ++ " in " ++ (roomName room))
    Right
    (find (\char -> playerAddr == (charAddress char)) (roomCharacters room))

findCharacterInRoomById :: String -> Room -> Either String Character
findCharacterInRoomById playerId room =
  maybe
    (Left $ "no character " ++ (show playerId) ++ " in " ++ (roomName room))
    Right
    (find (\char -> playerId == (charId char)) (roomCharacters room))

roomHasCharacterByAddress :: Address -> Room -> Bool
roomHasCharacterByAddress playerAddr room =
  either (const False) (const True) $ findCharacterInRoomByAddress playerAddr room

roomHasCharacterById :: String -> Room -> Bool
roomHasCharacterById playerId room =
  either (const False) (const True) $ findCharacterInRoomById playerId room

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
  oldChar <- findCharacterInRoomById (charId newChar) room
  Right $ room { roomCharacters = newChar : (delete oldChar (roomCharacters room)) }
