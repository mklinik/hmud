import Control.Monad

import Data.List (isPrefixOf, intercalate)

import Hmud.Item
import Hmud.Describable
import Hmud.Character
import Hmud.Room
import Hmud.World
import Hmud.Util
import Hmud.TestData
import Hmud.Commands

loopWithWorld playerName world = do
  putStr ">>> "
  tokens <- fmap words getLine
  case dispatch playerName tokens of
    Nothing -> return ()
    Just a  -> do w2 <- stepToStdout world a
                  loopWithWorld playerName w2

main = do
  player <- randomCharacter "Markus" $ Address "player"
  npc1 <- randomCharacter "Martin" $ Address ""
  npc2 <- randomCharacter "Karin" $ Address ""
  npc3 <- randomCharacter "Kathy" $ Address ""

  w2 <- stepToStdout world (insert player "The Black Unicorn")
  w3 <- stepToStdout w2 (insert npc1 "The Black Unicorn")
  w4 <- stepToStdout w3 (insert npc2 "The Black Unicorn")
  w5 <- stepToStdout w4 (insert npc3 "town square")
  w6 <- stepToStdout w5 (insertItem scroll0 "ivory tower")
  w7 <- stepToStdout w6 (insertItem beer "The Black Unicorn")
  w8 <- stepToStdout w7 (insertItem scroll1 "The Black Unicorn")

  loopWithWorld (Address "player") w8

  putStrLn "bye."
