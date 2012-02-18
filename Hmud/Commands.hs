module Hmud.Commands where

import Data.List (isPrefixOf, intercalate)

import Hmud.Describable
import Hmud.World
import Hmud.Room
import Hmud.Character
import Hmud.Item
import Hmud.Message

data CommandTag = Quit | Other

type WorldAction = World -> (World, Message)

idWorldAction :: String -> WorldAction
idWorldAction text world = (world, MsgInfo text)

insert :: Character -> String -> WorldAction
insert player toName world =
  case insertCharacterToRoom player toName world of
    Left err -> (world, MsgInfo err)
    Right w  -> (w, MsgInfo $ (name player) ++ " is now in " ++ toName)

insertItem :: Item -> String -> WorldAction
insertItem item toName world =
  case insertItemToRoom item toName world of
    Left err -> (world, MsgInfo err)
    Right w  -> (w, MsgInfo $ (name item) ++ " is now in " ++ toName)

goto :: Address -> [String] -> WorldAction
goto playerId args world =
  case findRoomOfPlayerExactly playerId world of
    Left err -> (world, MsgInfo err)
    Right r  -> case gotoFromTo playerId (name r) arg world of
                    Left err        -> (world, MsgInfo err)
                    Right (w, fromRoom, char, toRoom) -> (w, MsgGoto fromRoom char toRoom)
  where arg = unwords args

-- find something to describe in the given room
-- if no argument is given, describe the room
-- otherwise, try to find something to describe in the following order:
--   1) a character in the room
--   2) an item in the room
--   3) an item in the current players inventory
describeThing :: Room -> String -> Address -> String
describeThing room [] _ = "This is " ++ (name room) ++ ", " ++ (describe room)
describeThing room arg playerId =
  case findCharacter arg room of
    Right p  -> "You see " ++ (name p) ++ ", " ++ (describe p)
    Left err ->
      case findItemInRoom arg room of
        Right p  -> "You see " ++ (name p) ++ ", " ++ (describe p)
        Left err -> case findCharacterInRoomExactly playerId room of
          Right p  -> case characterFindItem arg p of
            Right i  -> "You see " ++ (name i) ++ ", " ++ (describe i)
            Left err -> "you can't see " ++ arg
          Left err -> "you can't see " ++ arg

lookAt :: Address -> [String] -> WorldAction
lookAt playerId args world =
  case findRoomOfPlayerExactly playerId world of
    Left err -> (world, MsgInfo err)
    Right r  -> (world, MsgInfo $ describeThing r (unwords args) playerId)

inventory :: Address -> [String] -> WorldAction
inventory playerId _ world = case findCharacterExactly playerId world of
  Left err   -> (world, MsgInfo err)
  Right char -> (world, MsgInfo $ if null $ charInventory char
                          then "Your bag of swag is empty."
                          else "Your possessions: " ++ (intercalate ", " $ map name (charInventory char))
                )

pickup :: Address -> [String] -> WorldAction
pickup playerId [] world =
    (world, MsgInfo $ "You take nothing.")
pickup playerId args world =
  case characterPickupItem playerId (unwords args) world of
    Left err     -> (world, MsgInfo err)
    Right (w, i) -> (w, MsgInfo $ "You take " ++ (name i))

put :: Address -> [String] -> WorldAction
put playerId [] world =
    (world, MsgInfo "You drop nothing.")
put playerId args world =
  case characterPutItem playerId (unwords args) world of
    Left err     -> (world, MsgInfo err)
    Right (w, i) -> (w, MsgInfo $ "You drop " ++ (name i))

forge :: Address -> [String] -> WorldAction
forge playerId args world =
  let (name_, desc_) = span (\x -> x /= "$") args
  in
    if (length name_ > 0 && length desc_ > 1)
      then let description = unwords $ tail desc_
               itName = unwords name_
           in
             case do char <- findCharacterExactly playerId world
                     _ <- characterFindItem "scroll of forgery" char -- if this fails, the whole command fails
                     let newChar = giveItemToCharacter
                                     (Item { itemName = itName, itemDescription = description })
                                     char
                     room <- findRoomOfPlayerExactly playerId world
                     newRoom <- updateCharInRoom newChar room
                     updateRoomInWorld newRoom world
             of
               Left err -> (world, MsgInfo err)
               Right w  -> (w, MsgInfo $ "The world around you gets dark. All sounds seem to fade. A moment of complete darkness is followed by a bright flash. As you slowly open your eyes again, a brand new " ++ itName ++ " hovers in the air before you, then floats slowly into your hands.")
      else (world, MsgInfo $ "usage: forge <item-name> $ <item-description>")

discard :: Address -> [String] -> WorldAction
discard playerId [] world = do
    (world, MsgInfo "You discard nothing.")
discard playerId args world =
  case do
    oldRoom <- findRoomOfPlayerExactly playerId world
    oldChar <- findCharacterInRoomExactly playerId oldRoom
    (newChar, item) <- removeItemFromInventory (unwords args) oldChar
    newRoom <- updateCharInRoom newChar oldRoom
    newWorld <- updateRoomInWorld newRoom world
    Right (newWorld, item)
  of
    Left err     -> (world, MsgInfo err)
    Right (w, i) -> (w, MsgInfo $ "You discard " ++ (name i))

-- syntax: give <item-name> to <player-name>
give :: Address -> [String] -> WorldAction
give playerId args world =
  let (itName_, receiverName_) = span (\x -> x /= "to") args
  in
    if (length itName_ > 0 && length receiverName_ > 1)
      then let receiverName = unwords $ tail receiverName_
               itName = unwords itName_
           in
             case do char <- findCharacterExactly playerId world
                     room <- findRoomOfPlayerExactly playerId world
                     receiver <- findCharacter receiverName room
                     if char == receiver then Left "You give the item to yourself." else do
                         (newChar, item) <- removeItemFromInventory itName char
                         let newReceiver = giveItemToCharacter item receiver
                         tmpRoom <- updateCharInRoom newChar room
                         newRoom <- updateCharInRoom newReceiver tmpRoom
                         newWorld <- updateRoomInWorld newRoom world
                         Right (newWorld, newReceiver, item)
             of
               Left err -> (world, MsgInfo err)
               Right (w, recv, item)  -> (w, MsgInfo $ "You give " ++ (name item) ++ " to " ++ (name recv))
      else (world, MsgInfo "usage: give <item-name> to <player-name>")

-- main loop:

  -- * have one world object
  -- * pick a world action
  -- * apply world action to world, yield either a new world or Nothing
  -- * update world on success


-- Each step performs an atomic update on the World. For example, if a player
-- moves from one room to another or if a player picks up an item. Each step
-- also produces a message about what just happened, or an error if the
-- requested step was not possible. The message will be delivered to all
-- affected players. For example, if a player picks up an item, all players in
-- the room see the message "Tom picked up Sword". If there is no such item to
-- pick up, only the player will see the error message "There is no Sord here
-- to pick up". The deliver callback is responsible to do this.
stepWorld :: Monad m => (Message -> m ()) -> World -> WorldAction -> m World
stepWorld deliver world action = do
  let (newWorld, message) = action world
  deliver message
  return newWorld

commands :: [(String, Address -> [String] -> WorldAction)]
commands =
  [ ("lookat", lookAt)
  , ("goto", goto)
  , ("inventory", inventory)
  , ("take", pickup)
  , ("put", put)
  , ("forge", forge)
  , ("discard", discard)
  , ("give", give)
  ]

dispatch :: Maybe Address -> [String] -> Maybe WorldAction
dispatch Nothing tokens = Just $ idWorldAction "Sorry, you don't have a character."
dispatch (Just playerId) tokens = do
  case tokens of
    []             -> Nothing
    (command:args) -> case (filter (\c -> command `isPrefixOf` (fst c)) commands) of
      []   -> Just $ idWorldAction $ "no such command: " ++ command
      c:[] -> Just $ (snd c) playerId args
      cs   -> Just $ idWorldAction $ "ambiguous command: " ++ command ++ " could be: "
                     ++ (intercalate ", " $ map fst cs)

stepToStdout = stepWorld (\msg -> do
  case msg of
    MsgInfo m ->
      putStrLn m
    MsgGoto _ char room ->
      putStrLn $ (name char) ++ " goes to " ++ (name room)
    MsgTake char item ->
      putStrLn $ (name char) ++ " takes " ++ (name item)
    MsgPut char item ->
      putStrLn $ (name char) ++ " puts down " ++ (name item)
    MsgGive giver item receiver ->
      putStrLn $ (name giver) ++ " gives " ++ (name item) ++ " to " ++ (name receiver)
    )
