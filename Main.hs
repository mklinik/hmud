import Control.Monad

import System.IO (hFlush, stdout)
import Data.List (isPrefixOf, intercalate)

import Hmud.Hmud
import Hmud.Message
import Hmud.Item
import Hmud.Describable
import Hmud.Character
import Hmud.Room
import Hmud.World
import Hmud.Util
import Hmud.TestData
import Hmud.Commands

instance MonadHmud IO where
  waitForMessage = do
    putStr ">>> "
    hFlush stdout
    tokens <- fmap words getLine
    case tokens of
      [] -> return MsgExit
      otherwise -> return $ MsgCommand (Just "player") tokens
  sendMessage addr msg = putStrLn $ describeMessage addr msg
  mkRandomCharacter = randomCharacter
  debugOut = putStrLn

main = do
  player <- randomCharacter "Markus" (Just "player") "a"
  npc1 <- randomCharacter "Martin" Nothing ""
  npc2 <- randomCharacter "Karin" Nothing ""
  npc3 <- randomCharacter "Kathy" Nothing ""

  w2 <- stepWorld Nothing world (insertNewPlayer player "The Black Unicorn")
  w3 <- stepWorld Nothing w2 (insertNewPlayer npc1 "The Black Unicorn")
  w4 <- stepWorld Nothing w3 (insertNewPlayer npc2 "The Black Unicorn")
  w5 <- stepWorld Nothing w4 (insertNewPlayer npc3 "town square")
  w6 <- stepWorld Nothing w5 (insertItem scroll0 "ivory tower")
  w7 <- stepWorld Nothing w6 (insertItem beer "The Black Unicorn")
  w8 <- stepWorld Nothing w7 (insertItem scroll1 "The Black Unicorn")

  run w8

  putStrLn "bye."
