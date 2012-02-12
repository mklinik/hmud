import Control.Monad

import Data.List (isPrefixOf)

import Hmud.Item
import Hmud.Describable
import Hmud.Character
import Hmud.Room
import Hmud.World
import Hmud.Util
import Hmud.TestData
import Hmud.Commands

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
stepWorld :: Monad m => (String -> m ()) -> World -> (World -> (World, String)) -> m World
stepWorld deliver world action = do
  let (newWorld, message) = action world
  deliver message
  return newWorld

stepToStdout = stepWorld putStrLn

commands :: [(String, String -> [String] -> WorldAction)]
commands = [("lookat", lookAt), ("goto", goto)]

dispatch :: String -> [String] -> Maybe WorldAction
dispatch playerName tokens = do
  case tokens of
    []             -> Nothing
    (command:args) -> case (filter (\c -> command `isPrefixOf` (fst c)) commands) of
                               []        -> Just $ idWorldAction $ "no such command: " ++ command
                               c:[]      -> Just $ (snd c) playerName args
                               otherwise -> Just $ idWorldAction $ "ambiguous command: " ++ command

loopWithWorld playerName world = do
  putStr ">>> "
  tokens <- fmap words getLine
  case dispatch playerName tokens of
    Nothing -> return ()
    Just a  -> do w2 <- stepToStdout world a
                  loopWithWorld playerName w2

main = do
  player <- randomCharacter "Markus"
  npc1 <- randomCharacter "Martin"
  npc2 <- randomCharacter "Karin"
  npc3 <- randomCharacter "Kathy"

  w2 <- stepToStdout world (insert player "The Black Unicorn")
  w3 <- stepToStdout w2 (insert npc1 "The Black Unicorn")
  w4 <- stepToStdout w3 (insert npc2 "The Black Unicorn")
  w5 <- stepToStdout w4 (insert npc3 "to")

  loopWithWorld "Markus" w5

  putStrLn "bye."
