{-# LANGUAGE FlexibleInstances #-}
import Control.Monad

import System.IO (hFlush, stdout)
import System.Console.Haskeline
import Data.List (isPrefixOf, intercalate)
import Control.Monad.Trans (liftIO)

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

instance MonadHmud (InputT IO) where
  waitForMessage = parseMsg `fmap` getInputLine ">>> "
    where
      parseMsg = maybe MsgExit (MsgCommand (Just "player") . words)
  sendMessage addr msg = liftIO . putStrLn $ describeMessage addr msg
  mkRandomCharacter = randomCharacter
  debugOut = liftIO . putStrLn

main :: IO ()
main = runInputT defaultSettings $ do
  player <- randomCharacter "Markus" (Just "player") "a"

  npc1 <- randomCharacter "Martin" Nothing ""
  npc2 <- randomCharacter "Karin" Nothing ""
  npc3 <- randomCharacter "Kathy" Nothing ""

  w2 <- stepWorld Nothing world (insertNewPlayer player "Black Unicorn")
  w3 <- stepWorld Nothing w2 (insertNewPlayer npc1 "Black Unicorn")
  w4 <- stepWorld Nothing w3 (insertNewPlayer npc2 "Black Unicorn")
  w5 <- stepWorld Nothing w4 (insertNewPlayer npc3 "town square")
  w6 <- stepWorld Nothing w5 (insertItem scroll0 "ivory tower")
  w7 <- stepWorld Nothing w6 (insertItem beer "Black Unicorn")
  w8 <- stepWorld Nothing w7 (insertItem scroll1 "Black Unicorn")

  run w8

  liftIO $ putStrLn "bye."
