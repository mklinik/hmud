{-# LANGUAGE ScopedTypeVariables #-}
module Hmud.Util where

import qualified Random
import Hmud.Character
import Data.Char

gameName = "The Tower"
homepageURL = "http://mklinik.github.com/hmud"
hmudVersion = "0.1"

randomEnum :: forall a. (Enum a, Bounded a) => IO a
randomEnum = do
  let minBoundInt = fromEnum (minBound :: a)
  let maxBoundInt = fromEnum (maxBound :: a)
  randomInt <- Random.randomRIO (minBoundInt, maxBoundInt)
  return $ toEnum randomInt

randomCharacter :: String -> Address -> String -> IO Character
randomCharacter name addr primKey = do
  race <- randomEnum
  role <- randomEnum
  gender <- randomEnum
  level <- Random.randomRIO (1,99)
  return Character { charName = name
                   , charRace = race
                   , charRole = role
                   , charGender = gender
                   , charLevel = level
                   , charInventory = []
                   , charAddress = addr
                   , charId = primKey
                   }

-- like words, put use  to split instead of whitespace
wordsPunct :: String -> [String]
wordsPunct [] = []
wordsPunct input = nextWord : wordsPunct remaining
  where tmp = dropWhile (not . isLetter) input
        nextWord = takeWhile isLetter tmp
        remaining = dropWhile isLetter tmp

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = (toUpper c):cs
