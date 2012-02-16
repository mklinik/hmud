module Hmud.Commands where

import Data.List (intercalate)

import Hmud.Describable
import Hmud.World
import Hmud.Room
import Hmud.Character

data CommandTag = Quit | Other

type WorldAction = World -> (World, String)

idWorldAction :: String -> WorldAction
idWorldAction text world = (world, text)

insert player toName world =
  case insertCharacterToRoom player toName world of
    Left err -> (world, err)
    Right w  -> (w, (name player) ++ " is now in " ++ toName)

insertItem item toName world =
  case insertItemToRoom item toName world of
    Left err -> (world, err)
    Right w  -> (w, (name item) ++ " is now in " ++ toName)

goto :: String -> [String] -> WorldAction
goto playerName args world =
  case findRoomOfPlayerExactly playerName world of
    Left err -> (world, err)
    Right r  -> case gotoFromTo playerName (name r) arg world of
                    Left err        -> (world, err)
                    Right (w, room) -> (w, "You are now in " ++ (name room) ++ ", " ++ (describe room))
  where arg = unwords args

-- find something to describe in the given room
-- if no argument is given, describe the room
-- otherwise, try to find something to describe in the following order:
--   1) a character in the room
--   2) an item in the room
--   3) an item in the current players inventory (TODO)
describeThing :: Room -> String -> String -> String
describeThing room [] _ = "This is " ++ (name room) ++ ", " ++ (describe room)
describeThing room arg playerName =
  case findCharacter arg room of
    Right p  -> "You see " ++ (name p) ++ ", " ++ (describe p)
    Left err ->
      case findItemInRoom arg room of
        Right p  -> "You see " ++ (name p) ++ ", " ++ (describe p)
        Left err -> case findCharacterInRoomExactly playerName room of
          Right p  -> case characterFindItem arg p of
            Right i  -> "You see " ++ (name i) ++ ", " ++ (describe i)
            Left err -> "you can't see " ++ arg
          Left err -> "you can't see " ++ arg

lookAt :: String -> [String] -> WorldAction
lookAt playerName args world =
  case findRoomOfPlayerExactly playerName world of
    Left err -> (world, err)
    Right r  -> (world, describeThing r (unwords args) playerName)

inventory :: String -> [String] -> WorldAction
inventory playerName _ world = case findCharacterExactly playerName world of
  Left err   -> (world, err)
  Right char -> (world, if null $ charInventory char
                          then "Your bag of swag is empty."
                          else "Your possessions: " ++ (intercalate ", " $ map name (charInventory char))
                )

pickup :: String -> [String] -> WorldAction
pickup playerName args world =
  case characterPickupItem playerName (unwords args) world of
    Left err     -> (world, err)
    Right (w, i) -> (w, "You take " ++ (name i))

put :: String -> [String] -> WorldAction
put playerName [] world =
    (world, "You drop nothing.")
put playerName args world =
  case characterPutItem playerName (unwords args) world of
    Left err     -> (world, err)
    Right (w, i) -> (w, "You drop " ++ (name i))
