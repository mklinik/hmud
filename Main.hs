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
      parseMsg = maybe MsgExit (MsgCommand "player" . words)
  sendMessage addr msg = liftIO . putStrLn $ describeMessage addr msg
  mkRandomCharacter = randomCharacter
  debugOut = liftIO . putStrLn

main :: IO ()
main = runInputT defaultSettings $ do
  player <- randomCharacter "Markus" "player" "a"

  npc1 <- randomCharacter "Martin" "martin" ""
  npc2 <- randomCharacter "Karin" "karin" ""
  npc3 <- randomCharacter "Kathy" "kathy" ""

  w2 <- stepWorld "" world (insertNewPlayer player "Black Unicorn")
  w3 <- stepWorld "" w2 (insertNewPlayer npc1 "Black Unicorn")
  w4 <- stepWorld "" w3 (insertNewPlayer npc2 "Black Unicorn")
  w5 <- stepWorld "" w4 (insertNewPlayer npc3 "town square")
  w6 <- stepWorld "" w5 (insertItem scroll0 "ivory tower")
  w7 <- stepWorld "" w6 (insertItem beer "Black Unicorn")
  w8 <- stepWorld "" w7 (insertItem scroll1 "Black Unicorn")

  run w8

  liftIO $ putStrLn "bye."
