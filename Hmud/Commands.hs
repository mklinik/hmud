module Hmud.Commands where

import Hmud.Describable
import Hmud.World
import Hmud.Room

data CommandTag = Quit | Other

type WorldAction = World -> (World, String)

idWorldAction :: String -> WorldAction
idWorldAction text world = (world, text)

insert player toName world =
  case insertCharacterToRoom player toName world of
    Left err -> (world, err)
    Right w  -> (w, (name player) ++ " is now in " ++ toName)

goto :: String -> [String] -> WorldAction
goto playerName args world =
  case findRoomOfPlayer playerName world of
    []        -> (world, "player " ++ playerName ++ " not found in any room")
    r:[]      -> case gotoFromTo playerName (name r) arg world of
                    Left err -> (world, err)
                    Right w  -> (w, "You are now in " ++ (arg))
    otherwise -> (world, "ambiguous player name: " ++ playerName)
  where arg = unwords args

-- find something to describe in the given room
-- if no argument is given, describe the room
-- otherwise, try to find something to describe in the following order:
--   1) a character in the room
--   2) an item in the room (TODO)
--   3) an item in the current players inventory (TODO)
describeThing :: Room -> String -> String
describeThing room []  = "This is " ++ (name room) ++ ", " ++ (describe room)
describeThing room arg =
  case findCharacter arg room of
    Left err -> "no such thing to look at: " ++ arg
    Right p  -> "You see " ++ (name p) ++ ", " ++ (describe p)

lookAt :: String -> [String] -> WorldAction
lookAt playerName args world =
  case findRoomOfPlayer playerName world of
    []        -> (world, "player " ++ playerName ++ " not found in any room")
    r:[]      -> (world, describeThing r (unwords args))
    otherwise -> (world, "ambiguous player name: " ++ playerName)
