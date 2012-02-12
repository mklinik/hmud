module Hmud.Room where

import Data.List (find, intercalate, isPrefixOf)

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

findCharacterInRoomExactly :: String -> Room -> Either String Character
findCharacterInRoomExactly playerName room =
  maybe
    (Left $ "no character " ++ playerName ++ " in " ++ (roomName room))
    Right
    (find (\char -> playerName == (charName char)) (roomCharacters room))

roomHasCharacter :: String -> Room -> Bool
roomHasCharacter chName room =
  either (const False) (const True) $ findCharacter chName room

roomEnter :: Character -> Room -> Room
roomEnter char room = room { roomCharacters = char:(roomCharacters room) }

roomSummary :: Room -> String
roomSummary room = (roomName room) ++ people
    where
      people =
        if (not $ null $ roomCharacters room)
          then ": " ++ (intercalate ", " $ map name $ roomCharacters room)
          else ": -"
