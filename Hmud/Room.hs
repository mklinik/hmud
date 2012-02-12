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
  describe room = (roomDescription room) ++ people ++ directions
    where
      people =
        if (not $ null $ roomCharacters room)
          then "\nThese people are present:\n" ++ (intercalate ", " $ map name $ roomCharacters room)
          else ""
      directions =
        if (not $ null $ roomAdjacents room)
          then "\nFrom here you can go to:\n" ++ (intercalate ", " $ roomAdjacents room)
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

findCharacter :: String -> Room -> Maybe Character
findCharacter playerName room = find (\char -> playerName `isPrefixOf` (charName char)) (roomCharacters room)

roomEnter :: Character -> Room -> Room
roomEnter char room = room { roomCharacters = char:(roomCharacters room) }

roomSummary :: Room -> String
roomSummary room = (roomName room) ++ people
    where
      people =
        if (not $ null $ roomCharacters room)
          then ": " ++ (intercalate ", " $ map name $ roomCharacters room)
          else ": -"
