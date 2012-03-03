module Hmud.World where

import Data.List (isPrefixOf, find, delete, intercalate)
import Data.Either (rights)
import qualified Control.Exception as Exception

import Hmud.Describable
import Hmud.Room
import Hmud.Character
import Hmud.Item

data World = World { worldRooms :: [Room]
                   , idleCharacters :: [Character]
                   }
  deriving (Eq, Show, Read)


-- Tries to load the savegame with the given name.
-- If an error occurs, it returns the default world
loadWorld :: FilePath -> World -> IO World
loadWorld fileName defaultWorld = do
  eWorld <- Exception.try (read `fmap` readFile fileName) :: IO (Either Exception.SomeException World)
  case eWorld of
    Left _ -> putStrLn "Error loading save game. Using default." >> return defaultWorld
    Right w -> putStrLn "Succesfully loaded save game." >> return w

saveWorld :: FilePath -> World -> IO ()
saveWorld fileName w = putStrLn ("Saving to " ++ fileName) >> writeFile fileName (show w)

findRoom :: String -> World -> Either String Room
findRoom rName world = maybe (Left $ rName ++ ": no such room") Right
  $ find (\room -> rName `isPrefixOf` (roomName room)) (worldRooms world)

insertCharacterToRoom :: Character -> String -> World -> Either String World
insertCharacterToRoom char toName world = do
  toRoom <- findRoom toName world
  let remainingRooms = delete toRoom (worldRooms world)
  let newToRoom = toRoom { roomCharacters = char:(roomCharacters toRoom) }
  return world { worldRooms = newToRoom:remainingRooms }

insertItemToRoom :: Item -> String -> World -> Either String World
insertItemToRoom item toName world = do
  toRoom <- findRoom toName world
  let remainingRooms = delete toRoom (worldRooms world)
  let newToRoom = toRoom { roomItems = item:(roomItems toRoom) }
  return world { worldRooms = newToRoom:remainingRooms }

-- may fail for various obvious reasons
gotoFromTo :: Address -> String -> String -> World -> Either String (World, Room, Character, Room)
gotoFromTo playerAddr fromName toName world = do
      fromRoom <- findRoom fromName world
      toRoom   <- findRoom toName world
      if fromRoom == toRoom
        then (Left "You are already there.")
        else do
          let remainingRooms = delete toRoom $ delete fromRoom (worldRooms world)
          player <- findCharacterInRoomByAddress playerAddr fromRoom
          let newFromRoom = fromRoom { roomCharacters = delete player (roomCharacters fromRoom) }
          let newToRoom   = toRoom   { roomCharacters = player:(roomCharacters toRoom) }
          Right (world { worldRooms = newFromRoom:newToRoom:remainingRooms }, newFromRoom, player, newToRoom)

worldSummary :: World -> String
worldSummary world = intercalate "\n" (map roomSummary (worldRooms world))

findRoomOfPlayerByAddress :: Address -> World -> Either String Room
findRoomOfPlayerByAddress playerAddr world =
  case filter (roomHasCharacterByAddress playerAddr) (worldRooms world) of
    []        -> Left $ "no such character: " ++ (show playerAddr)
    r:[]      -> Right r
    _         -> Left $ "ambiguous character name: " ++ (show playerAddr) -- should never happen

findRoomOfPlayerById :: String -> World -> Either String Room
findRoomOfPlayerById playerId world =
  case filter (roomHasCharacterById playerId) (worldRooms world) of
    []        -> Left $ "no such character: " ++ (show playerId)
    r:[]      -> Right r
    _         -> Left $ "ambiguous character name: " ++ (show playerId) -- should never happen

-- Assuming that player names are unique, if we find any players at all, we only find one
findCharacterByAddress :: Address -> World -> Either String Character
findCharacterByAddress playerAddr world =
  case rights $ map (findCharacterInRoomByAddress playerAddr) (worldRooms world) of
    []        -> Left $ "no such character: " ++ (show playerAddr)
    p:[]      -> Right p
    _         -> Left $ "ambiguous character name: " ++ (show playerAddr)

findCharacterById :: String -> World -> Either String Character
findCharacterById playerId world =
  case rights $ map (findCharacterInRoomById playerId) (worldRooms world) of
    []        -> Left $ "no such character: " ++ (show playerId)
    p:[]      -> Right p
    _         -> Left $ "ambiguous character name: " ++ (show playerId)

findCharacter :: String -> World -> Either String Character
findCharacter playerName world =
  case rights $ map (findCharacterInRoom playerName) (worldRooms world) of
    []        -> Left $ "no such character: " ++ (show playerName)
    p:[]      -> Right p
    _         -> Left $ "ambiguous character name: " ++ (show playerName)

characterPickupItem :: Address -> String -> World -> Either String (World, Character, Item)
characterPickupItem playerAddr itName world = do
  oldRoom <- findRoomOfPlayerByAddress playerAddr world
  oldChar <- findCharacterInRoomByAddress playerAddr oldRoom
  (tmpRoom, item) <- removeItemFromRoom itName oldRoom
  let newChar = giveItemToCharacter item oldChar
  newRoom <- updateCharInRoom newChar tmpRoom
  newWorld <- updateRoomInWorld newRoom world
  Right (newWorld, newChar, item)

updateRoomInWorld :: Room -> World -> Either String World
updateRoomInWorld newRoom world = do
  oldRoom <- findRoom (name newRoom) world
  Right $ world { worldRooms = newRoom : (delete oldRoom (worldRooms world)) }

characterPutItem :: Address -> String -> World -> Either String (World, Character, Item)
characterPutItem playerAddr itName world = do
  oldRoom <- findRoomOfPlayerByAddress playerAddr world
  oldChar <- findCharacterInRoomByAddress playerAddr oldRoom
  (newChar, item) <- removeItemFromInventory itName oldChar
  newRoom <- updateCharInRoom newChar oldRoom
  tmpWorld <- updateRoomInWorld newRoom world
  newWorld <- insertItemToRoom item (name newRoom) tmpWorld
  Right (newWorld, newChar, item)

getIdleCharacterById :: String -> World -> Maybe (Character, World)
getIdleCharacterById primKey world = do
  (found, restIdles) <- extract (\c -> charId c == primKey) (idleCharacters world)
  return (found, world { idleCharacters = restIdles })

extract :: (a -> Bool) -> [a] -> Maybe (a, [a])
extract p xs = case break p xs of
  (nots, found:rest) -> Just (found, nots ++ rest)
  _ -> Nothing
