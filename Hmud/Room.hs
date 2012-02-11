module Hmud.Room where

import qualified Data.List as List

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
          then "\nThese people are present:\n" ++ (List.intercalate ", " $ map name $ roomCharacters room)
          else ""
      directions =
        if (not $ null $ roomAdjacents room)
          then "\nFrom here you can go to:\n" ++ (List.intercalate ", " $ roomAdjacents room)
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

roomEnter :: Character -> Room -> Room
roomEnter char room = room { roomCharacters = char:(roomCharacters room) }

roomSummary :: Room -> String
roomSummary room = (roomName room) ++ people
    where
      people =
        if (not $ null $ roomCharacters room)
          then ": " ++ (List.intercalate ", " $ map name $ roomCharacters room)
          else ": -"
