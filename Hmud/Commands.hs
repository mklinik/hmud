module Hmud.Commands where

import Data.List (isPrefixOf, intercalate)

import Hmud.Describable
import Hmud.World
import Hmud.Room
import Hmud.Character
import Hmud.Item
import Hmud.Message
import Hmud.Hmud
import Hmud.Util

data CommandTag = Quit | Other

type WorldAction = World -> (World, Message)

idWorldAction :: String -> WorldAction
idWorldAction text world = (world, MsgInfo text)

insertNewPlayer :: Character -> String -> WorldAction
insertNewPlayer player toName world =
  case insertCharacterToRoom player toName world of
    Left err -> (world, MsgInfo err)
    Right w  -> (w, MsgInfo $ "Welcome " ++ (name player) ++ ", you are " ++ (describe player) ++ ". Type \"help\" for more information.")

insertItem :: Item -> String -> WorldAction
insertItem item toName world =
  case insertItemToRoom item toName world of
    Left err -> (world, MsgInfo err)
    Right w  -> (w, MsgInfo $ (name item) ++ " is now in " ++ toName)

goto :: Address -> [String] -> WorldAction
goto playerAddr args world =
  case findRoomOfPlayerByAddress playerAddr world of
    Left err -> (world, MsgInfo err)
    Right r  -> case gotoFromTo playerAddr (name r) arg world of
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
describeThing room arg playerAddr =
  case findCharacterInRoom arg room of
    Right p  -> "You see " ++ (name p) ++ ", " ++ (describe p)
    Left err ->
      case findItemInRoom arg room of
        Right p  -> "You see " ++ (name p) ++ ", " ++ (describe p)
        Left err -> case findCharacterInRoomByAddress playerAddr room of
          Right p  -> case characterFindItem arg p of
            Right i  -> "You see " ++ (name i) ++ ", " ++ (describe i)
            Left err -> "you can't see " ++ arg
          Left err -> "you can't see " ++ arg

lookAt :: Address -> [String] -> WorldAction
lookAt playerAddr args world =
  case findRoomOfPlayerByAddress playerAddr world of
    Left err -> (world, MsgInfo err)
    Right r  -> (world, MsgInfo $ describeThing r (unwords args) playerAddr)

inventory :: Address -> [String] -> WorldAction
inventory playerAddr _ world = case findCharacterByAddress playerAddr world of
  Left err   -> (world, MsgInfo err)
  Right char -> (world, MsgInfo $ if null $ charInventory char
                          then "Your bag of swag is empty."
                          else "Your possessions: " ++ (intercalate ", " $ map name (charInventory char))
                )

pickup :: Address -> [String] -> WorldAction
pickup playerAddr [] world =
    (world, MsgInfo $ "You take nothing.")
pickup playerAddr args world =
  case characterPickupItem playerAddr (unwords args) world of
    Left err     -> (world, MsgInfo err)
    Right (w, c, i) -> (w, MsgTake c i)

put :: Address -> [String] -> WorldAction
put playerAddr [] world =
    (world, MsgInfo "You drop nothing.")
put playerAddr args world =
  case characterPutItem playerAddr (unwords args) world of
    Left err     -> (world, MsgInfo err)
    Right (w, c, i) -> (w, MsgPut c i)

forge :: Address -> [String] -> WorldAction
forge playerAddr args world =
  let (name_, desc_) = span (\x -> x /= "$") args
  in
    if (length name_ > 0 && length desc_ > 1)
      then let description = unwords $ tail desc_
               itName = unwords name_
           in
             case do char <- findCharacterByAddress playerAddr world
                     _ <- characterFindItem "scroll of forgery" char -- if this fails, the whole command fails
                     let item = (Item { itemName = itName, itemDescription = description })
                     let newChar = giveItemToCharacter item char
                     room <- findRoomOfPlayerByAddress playerAddr world
                     newRoom <- updateCharInRoom newChar room
                     newWorld <- updateRoomInWorld newRoom world
                     return (newWorld, char, item)
             of
               Left err -> (world, MsgInfo err)
               Right (w, char, item) -> (w, MsgForge char item)
      else (world, MsgInfo $ "usage: forge <item-name> $ <item-description>")

discard :: Address -> [String] -> WorldAction
discard playerAddr [] world = do
    (world, MsgInfo "You discard nothing.")
discard playerAddr args world =
  case do
    oldRoom <- findRoomOfPlayerByAddress playerAddr world
    oldChar <- findCharacterInRoomByAddress playerAddr oldRoom
    (newChar, item) <- removeItemFromInventory (unwords args) oldChar
    newRoom <- updateCharInRoom newChar oldRoom
    newWorld <- updateRoomInWorld newRoom world
    Right (newWorld, item)
  of
    Left err     -> (world, MsgInfo err)
    Right (w, i) -> (w, MsgInfo $ "You discard " ++ (name i))

-- syntax: give <item-name> to <player-name>
give :: Address -> [String] -> WorldAction
give playerAddr args world =
  let (itName_, receiverName_) = span (\x -> x /= "to") args
  in
    if (length itName_ > 0 && length receiverName_ > 1)
      then let receiverName = unwords $ tail receiverName_
               itName = unwords itName_
           in
             case do char <- findCharacterByAddress playerAddr world
                     room <- findRoomOfPlayerByAddress playerAddr world
                     receiver <- findCharacterInRoom receiverName room
                     if char == receiver then Left "You give the item to yourself." else do
                         (newChar, item) <- removeItemFromInventory itName char
                         let newReceiver = giveItemToCharacter item receiver
                         tmpRoom <- updateCharInRoom newChar room
                         newRoom <- updateCharInRoom newReceiver tmpRoom
                         newWorld <- updateRoomInWorld newRoom world
                         Right (newWorld, newChar, item, newReceiver)
             of
               Left err -> (world, MsgInfo err)
               Right (w, giver, item, givee)  -> (w, MsgGive giver item givee)
      else (world, MsgInfo "usage: give <item-name> to <player-name>")

help :: Address -> [String] -> WorldAction
help playerAddr args world
  | args == ["commands"] = (world, MsgInfo $ "\n" ++ (intercalate "\n" $ map (\(_, _, helpText)->helpText) commands))
  | otherwise = (world, MsgInfo $ "Welcome to "++ gameName ++". Please visit " ++ homepageURL ++ " for even more information.\nType \"help commands\" to get a list of what you can do here."
  )

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
stepWorld :: MonadHmud m => Address -> World -> WorldAction -> m World
stepWorld sender world action = do
  let (newWorld, message) = action world

  case message of
    MsgInfo m -> sendMessage sender message
    MsgGoto fromRoom char toRoom ->
      mapM_ (\char -> sendMessage (charAddress char) message)
        $ (roomCharacters fromRoom) ++ (roomCharacters toRoom)
    MsgTake char item -> do
      case findRoomOfPlayerByAddress (charAddress char) newWorld of
        Left err -> debugOut err
        Right room ->
          mapM_ (\c -> sendMessage (charAddress c) message) (roomCharacters room)
    MsgPut char item -> do
      case findRoomOfPlayerByAddress (charAddress char) newWorld of
        Left err -> debugOut err
        Right room ->
          mapM_ (\c -> sendMessage (charAddress c) message) (roomCharacters room)
    MsgGive giver item receiver -> do
      case findRoomOfPlayerByAddress (charAddress giver) newWorld of
        Left err -> debugOut err
        Right room ->
          mapM_ (\c -> sendMessage (charAddress c) message) (roomCharacters room)
    MsgForge char item -> sendMessage sender message

  return newWorld

commands :: [(String, Address -> [String] -> WorldAction, String)]
commands =
  [ ("lookat", lookAt, "lookat\n  Describes your immediate surroundings.\nlookat <name>\n  Look at items or players.")
  , ("goto", goto, "goto <room-name>\n  Go to a different room.")
  , ("inventory", inventory, "inventory\n  List your possessions.")
  , ("take", pickup, "take <item-name>\n  Pick up an item.")
  , ("put", put, "put <item-name>\n  Put down an item.")
  , ("forge", forge, "forge <name> $ <description>\n  Create a new item. Requires a scroll of forgery.")
  , ("discard", discard, "discard <item-name>\n  Delete an item. Completely. Forever. Think twice.")
  , ("give", give, "give <item-name> to <player-name>\n  Give an item to another player.")
  , ("help", help, "")
  ]

dispatch :: Address -> [String] -> Maybe WorldAction
dispatch playerAddr tokens = do
  case tokens of
    []             -> Nothing
    (command:args) -> case (filter (\(cname, _, _) -> command `isPrefixOf` cname) commands) of
      []   -> Just $ idWorldAction $ "no such command: " ++ command ++ ". Type \"help commands\" to get a list of what you can do"
      (_, c, _):[] -> Just $ c playerAddr args
      cs   -> Just $ idWorldAction $ "ambiguous command: " ++ command ++ " could be: "
                     ++ (intercalate ", " $ map (\(cname, _, _) -> cname) cs)

run :: MonadHmud m => World -> m World
run world = do
  msg <- waitForMessage
  case msg of
    MsgCommand playerAddr tokens -> case dispatch playerAddr tokens of
          Nothing -> run world -- empty command
          Just a  -> do
            w2 <- stepWorld playerAddr world a
            run w2
    MsgPlayerEnters playerAddr playerName primKey -> do
      newWorld <- do
        case findCharacterById primKey world of
          Left _ -> do
            player <- mkRandomCharacter playerName playerAddr primKey
            newWorld <- stepWorld playerAddr world (insertNewPlayer player "The Black Unicorn")
            return newWorld
          Right char -> case do
              room <- findRoomOfPlayerById primKey world
              let newChar = char { charAddress = playerAddr }
              newRoom <- updateCharInRoom newChar room
              updateRoomInWorld newRoom world
            of
              Left err -> debugOut err >> return world
              Right newWorld -> return newWorld
      run newWorld
    MsgExit -> return world
