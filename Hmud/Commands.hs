module Hmud.Commands where

import Data.List (isPrefixOf, intercalate)

import Hmud.Describable
import Hmud.World
import Hmud.Room
import Hmud.Character
import Hmud.Item
import Hmud.Message
import Hmud.Hmud

data CommandTag = Quit | Other

type WorldAction = World -> (World, Message)

idWorldAction :: String -> WorldAction
idWorldAction text world = (world, MsgInfo text)

insertNewPlayer :: Character -> String -> WorldAction
insertNewPlayer player toName world =
  case insertCharacterToRoom player toName world of
    Left err -> (world, MsgInfo err)
    Right w  -> (w, MsgInfo $ "Welcome " ++ (name player) ++ ", you are a " ++ (describe player) ++ ". Type help for more info.")

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
  case findCharacterInRoom arg room of
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
    Right (w, c, i) -> (w, MsgTake c i)

put :: Address -> [String] -> WorldAction
put playerId [] world =
    (world, MsgInfo "You drop nothing.")
put playerId args world =
  case characterPutItem playerId (unwords args) world of
    Left err     -> (world, MsgInfo err)
    Right (w, c, i) -> (w, MsgPut c i)

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
                     let item = (Item { itemName = itName, itemDescription = description })
                     let newChar = giveItemToCharacter item char
                     room <- findRoomOfPlayerExactly playerId world
                     newRoom <- updateCharInRoom newChar room
                     newWorld <- updateRoomInWorld newRoom world
                     return (newWorld, char, item)
             of
               Left err -> (world, MsgInfo err)
               Right (w, char, item) -> (w, MsgForge char item)
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
      case findRoomOfPlayerExactly (charAddress char) newWorld of
        Left err -> debugOut err
        Right room ->
          mapM_ (\c -> sendMessage (charAddress c) message) (roomCharacters room)
    MsgPut char item -> do
      case findRoomOfPlayerExactly (charAddress char) newWorld of
        Left err -> debugOut err
        Right room ->
          mapM_ (\c -> sendMessage (charAddress c) message) (roomCharacters room)
    MsgGive giver item receiver -> do
      case findRoomOfPlayerExactly (charAddress giver) newWorld of
        Left err -> debugOut err
        Right room ->
          mapM_ (\c -> sendMessage (charAddress c) message) (roomCharacters room)
    MsgForge char item -> sendMessage sender message

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

dispatch :: Address -> [String] -> Maybe WorldAction
dispatch playerId tokens = do
  case tokens of
    []             -> Nothing
    (command:args) -> case (filter (\c -> command `isPrefixOf` (fst c)) commands) of
      []   -> Just $ idWorldAction $ "no such command: " ++ command
      c:[] -> Just $ (snd c) playerId args
      cs   -> Just $ idWorldAction $ "ambiguous command: " ++ command ++ " could be: "
                     ++ (intercalate ", " $ map fst cs)

run :: MonadHmud m => World -> m World
run world = do
  msg <- waitForMessage
  case msg of
    MsgCommand playerId tokens -> case dispatch playerId tokens of
          Nothing -> run world -- empty command
          Just a  -> do
            w2 <- stepWorld playerId world a
            run w2
    MsgPlayerEnters playerId playerName -> do
      newWorld <- do
        case findCharacter playerName world of
          Left _ -> do
              player <- mkRandomCharacter playerName playerId
              newWorld <- stepWorld playerId world (insertNewPlayer player "The Black Unicorn")
              return newWorld
          Right _ -> return world
      run newWorld
    MsgExit -> return world
